(require 'cl-lib)
(require 'websocket)
(require 's)
(require 'selcand)
(require 'json)

(defvar sms-fanout-address nil)
(defvar sms-fanout-client nil)
(defvar sms-fanout-client-last-pong-sent nil)
(defvar sms-fanout-client-last-pong-received nil)
(defvar sms-fanout-reconnect-interval-mins 1)
(defvar sms-fanout-ping-interval-seconds 30)
(defvar sip-last-known-identity nil)
(defvar sip-inhibit-echo-linphone-command nil)
(defvar sip-messages (make-hash-table))
(defvar sip-quiet-p t)

(defun sms-fanout-disconnected-p (&optional client)
  (cond
   ((not (setq client (or client sms-fanout-client)))
    "null client")
   ((not (websocket-openp client)) "websocket-openp is false")
   ((not sms-fanout-client-last-pong-received) "no last pong received")
   (t (let ((last-pong-ago-secs
             (- (float-time)
                sms-fanout-client-last-pong-received))
            (max-allowed-last-pong-ago-secs
             (* sms-fanout-ping-interval-seconds 2)))
        (if (<= max-allowed-last-pong-ago-secs last-pong-ago-secs)
            (format
             "last pong received %ss ago, max is %ss ago"
             last-pong-ago-secs max-allowed-last-pong-ago-secs))))))

(defun json-parse-whole-string (json)
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (prog1
        (json-read)
      (unless (s-blank? (buffer-substring (point) (point-max)))
        (error "entire text did not parse as json: %s" json)))))

(defvar sip-buffer-fmt "#sip-sms-%s")

(defvar-local sip-from-phone-number nil)
(defvar-local sip-did nil)
(defvar-local sip-max-timestamp nil)

(defvar linphone-profile-id nil)

(defun linphone-current-profile-id ())

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  (let ((linphone-profile-id (linphone-current-profile-id))
        (env-args (append
                   (list (format "LINPHONE_PROFILE_ID=%s" (linphone-current-profile-id))
                         "linphonecsh")
                   args)))
    (unless nil
      (message "running: env %s" (string-join env-args " ")))
    (with-temp-buffer
      (apply #'call-process "env" nil (current-buffer) nil env-args)
      (buffer-string))))

(defun sip-current-identity ()
  (save-match-data
    (let* ((output (linphonecsh "generic" "proxy show default"))
           (identity (string-match "identity: sip:\\(.*\\)@\\(.*\\)" output)))
      (unless identity (error "unable to determine default proxy: %s"
                              output))
      (list (match-string 1 output) (match-string 2 output)))))

(defun sip-default-host ()
  (cl-second (sip-current-identity)))

(defun sip-phone-number-clean (number)
  (let* ((number-clean (replace-regexp-in-string "[^0-9]" "" number))
         (number-clean
          (if (and (eq 11 (length number-clean))
                   (s-starts-with-p "1" number-clean))
              (substring number-clean 1)
            number-clean)))
    number-clean))

(defun sip-phone-number-to-address (number &optional sip-host)
  (let* ((sip-host (or sip-host (sip-default-host)))
         (number-clean (sip-phone-number-clean number))
         (sip-address (format "sip:%s@%s" number-clean sip-host)))
    sip-address))

(defun sip-add-message (sip-message)
  (let* ((id (sip-message-id sip-message))
         (id-numeric (if (stringp id) (string-to-number id) id))
         (exists (gethash id-numeric sip-messages)))
    (when (not exists)
      (puthash id-numeric sip-message sip-messages)
      t)))

(defun url-retrieve-with-auth (user password &rest r)
  (let ()
    (apply #'url-retrieve r)))

(defun voipms-service-request (path)
  (let*
      ((parts (url-generic-parse-url (sms-fanout-read-address)))
       (user (url-user parts))
       (password (url-password parts))
       (hostname (url-host parts))
       (port (url-port parts))
       (port-opt (if (equal port 0) "" (format ":%s" port)))
       (wss-scheme (url-type parts))
       (http-scheme (cond
                     ((equal "wss" wss-scheme) "https")
                     ((equal "ws" wss-scheme) "http")
                     ((member wss-scheme '("http" "https")) wss-scheme)
                     (t (error "unknown scheme: %s" wss-scheme))))
       (url (format "%s://%s%s%s" http-scheme hostname port-opt path)))
    (let ((url-request-extra-headers
           `(("Authorization" .
              ,(concat "Basic "
                       (base64-encode-string
                        (format "%s:%s" user password)))))))
      (url-retrieve-synchronously-curl url))))

(defun url-retrieve-synchronously-curl (url)
  (let* ((buffer (format "*curl-%s" (uuid)))
         (args (list
                "curl" "-sL" "--fail-with-body"
                (format "-X%s" (or url-request-method
                                   (if url-request-data "POST" "GET")))
                url))
         proc)
    (when url-request-data (push-last "-d@-" args))
    (cl-loop for (k . v) in url-request-extra-headers
             do (push-last (format "-H%s:%s" k v) args))
    (message "%s" (string-join args " "))
    (message "POST data: %s" url-request-data)
    (setq proc (apply #'start-process buffer buffer args))
    (set-process-sentinel proc
                          (lambda (proc status)
                            (message "process sentinel: %s %s" proc status)))
    (when url-request-data
      (process-send-string proc url-request-data))
    (cl-loop
     for i below 30
     while (process-live-p proc)
     do (progn (process-send-eof proc)
               (message "waiting for process to die... %s" buffer)
               (sit-for 1)))
    (let ((resp (with-current-buffer buffer
                  (buffer-string))))
      (if (zerop (process-exit-status proc))
          (prog1
              resp
            (kill-buffer buffer))
        (progn
          (switch-to-buffer-other-window buffer)
          (error "non-zero curl exit: %s %s" (process-exit-status proc) resp))))))

(defun string-to-batches (message batch-size)
  (cl-loop while (not (string-empty-p message))
           as i = (min batch-size (length message))
           collect (substring message 0 i)
           do (setq message (substring message i))))

(defun sms-send-batch (from to message)
  (cl-loop for batch in (string-to-batches message 160)
           collect (sms-send from to batch)))

(defun sms-send (from to message)
  (message "sending message of length %s from %s to %s. message: %s"
           (length message) from to message)
  (let ((url-request-method "POST")
        (url-request-data
         (json-encode `((from . ,from) (to . ,to) (message . ,message)))))
    (let ((resp (voipms-service-request "/sms")))
      (if (save-match-data
            (string-match "status.*success" resp))
          resp
        (error "non-success sms-send response: %s" resp)))))

(defun sms-send-linphonecsh (from to message)
  (let* ((current-identity (sip-current-identity))
         (sip-address
          (sip-phone-number-to-address
           from
           (cl-second current-identity))))
    (cl-assert current-identity)
    (unless (or (null sip-last-known-identity)
                (equal current-identity sip-last-known-identity)
                (y-or-n-p (format "use new sip identity: %s? "
                                  (s-join "@" current-identity))))
      (error "identity change not acknowledged"))
    (setq sip-last-known-identity current-identity)
    (linphonecsh "generic" (format "chat %s %s" sip-address message))))

(defun sip-send-chat-line ()
  (interactive)
  (cl-assert (bound-and-true-p sip-from-phone-number))
  (cl-assert (bound-and-true-p sip-did))
  (goto-char (point-max))
  (let* ((message (buffer-substring
                   (line-beginning-position)
                   (line-end-position)))
         (to sip-from-phone-number)
         (from sip-did)
         (resp (sms-send-batch from to message)))
    (message "resp: %s" resp)
    (goto-char (line-beginning-position))
    (insert "        YOU say: ")
    ;; maybe add a newline
    (goto-char (point-max))
    (unless (eq (point) (line-beginning-position))
      (newline))))

(defun sip-send-region (a b)
  (interactive "r")
  (let ((sip-address (sip-phone-number-to-address sip-from-phone-number)))
    (linphonecsh "generic" (format "chat %s %s"
                                   sip-address
                                   (buffer-substring (min a b)
                                                     (max a b))))))

(defvar sip-last-message-buffer nil)

(defvar sip-autosend-message nil
  "A message to send automatically to any new SMS chat")

(defun string-null-or-empty-p (string)
  (or (null string) (string-empty-p string)))

(defun sip-chat-maybe-autosend-message ()
  (when
      (and (eq 1 (line-number-at-pos))
           (not (string-null-or-empty-p sip-autosend-message))
           (not (save-excursion
                  (goto-char (point-min))
                  (re-search-forward (regexp-quote sip-autosend-message) nil t))))
    (insert sip-autosend-message)
    (sip-send-chat-line)
    (sit-for 1)))

(defvar sip-chat-pretyped-messages nil)

(defun sip-chat-insert-pretyped-message ()
  (interactive)
  (let ((selection
         (selcand-select sip-chat-pretyped-messages
                         "select a reply: ")))
    (goto-char (point-max))
    (insert selection)
    (sip-send-chat-line)
    (sit-for 1)))

(defun sip-chat-buffer (other-number self-number)
  (cl-assert (not (s-blank? other-number)))
  (cl-assert self-number)
  (let* ((other-number-clean (sip-phone-number-clean other-number))
         (buffer-name (format sip-buffer-fmt
                              (concat other-number-clean
                                      (when self-number
                                        (concat "-to-" self-number)))))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (sip-chat-mode)
      (setq-local sip-from-phone-number other-number-clean)
      (setq-local sip-did self-number)
      (sip-chat-maybe-autosend-message))
    buffer))

(defun sip-chat-all-region ()
  (interactive)
  (let ((numbers (cl-remove-if
                  #'string-blank-p
                  (split-string (region) "[,\"]" t "[][]"))))
    (cl-loop for number in numbers
             do (message "opening %s" number)
             do (sip-chat number))))

(cl-defstruct sip-message id from to message date)

(defmacro my-with-slots (class-name slots object &rest body)
  `(let
       ,(cl-loop for slot in slots
                 collect
                 (list slot
                       `(,(intern
                           (format "%s-%s" class-name slot))
                         ,object)))
     ,@body))

(defun sip-message-received (sip-message &optional supress-echo)
  (my-with-slots sip-message (id from to message date) sip-message
                 (if (null (sip-add-message sip-message))
                     (sip-ws-log (format "skipping previously-received message with id %s" id))
                   (let* ((buffer (sip-chat-buffer from to))
                          (line (format "%s says: %s" from message))
                          (timestamp
                           (condition-case ex (->> date
                                                   parse-time-string
                                                   (apply #'encode-time)
                                                   time-to-seconds)
                             (error
                              (sip-ws-log (format "failed to parse timestamp: %s" ex))))))
                     (with-current-buffer buffer
                       (sip-chat-mode t)
                       (setq was-at-bottom (eq (point-max) (point)))
                       (save-excursion
                         (goto-char (point-max))
                         (goto-char (line-beginning-position))
                         (open-line 1)
                         (insert
                          (format-time-string "%d %b %I:%M%p (%a)" (seconds-to-time timestamp)))
                         (insert line)
                         (setq-local sip-from-phone-number from)
                         (setq-local sip-max-timestamp (max timestamp (or sip-max-timestamp 0))))
                       (when was-at-bottom
                         (goto-char (point-max))))
                     (unless supress-echo
                       (message "%s" line))
                     (setq sip-last-message-buffer buffer)))))

(defun sip-clean-phone-number (number)
  (replace-regexp-in-string "[^0-9]" "" number))

(defun sip-chat (number &optional did)
  (interactive "senter phone number: ")
  (let* ((number-clean (sip-clean-phone-number number))
         (did (or did (sip-select-did)))
         (buffer (sip-chat-buffer number-clean did)))
    (switch-to-buffer buffer)
    (sip-chat-maybe-autosend-message)))

(defun sip-message-exists-p (number message did)
  (with-current-buffer (sip-chat-buffer number did)
    (save-excursion
      (goto-char (point-min))
      (search-forward message))))

(defun sip-list-dids ()
  (let* ((url-request-data nil)
         (url-request-method "GET")
         (resp (voipms-service-request "/dids"))
         (json-object-type 'alist)
         (json (json-parse-whole-string resp)))
    (mapcar (apply-partially #'alist-get 'did) json)))

(defun sip-select-did ()
  (selcand-select (sip-list-dids) :prompt "select did: "))

(defun sip-chat-batch (numbers message did)
  (interactive (list
                (read-string "enter newline-separated phone numbers: ")
                (read-string "senter sms message: ")
                (sip-select-did)))
  (cl-loop for number in (split-string numbers "\n" t)
           if (sip-message-exists-p number message did)
           do (message "skipping previously-sent message to %s" number)
           else do (progn
                     (sip-chat number message did)
                     (sit-for 1))))

(defun sip-goto-last-message-buffer ()
  (interactive)
  (cl-assert sip-last-message-buffer)
  (switch-to-buffer sip-last-message-buffer))

(defmacro alist-let (alist vars &rest body)
  (declare (indent 2))
  (let ((alist-sym (gensym "alist-")))
    `(let ((,alist-sym ,alist))
       (let ,(cl-loop for var in vars
                      collect `(,var (cdr (assoc ',var ,alist-sym))))
         ,@body))))

(defvar sms-last-connection-timestamp nil)

;; TODO use consistent namespace

(defun sip-ws-maybe-reconnect ()
  ;; throttle reconnection
  (let ((elapsed (when sms-fanout-client-last-pong-sent
                   (- (float-time) sms-fanout-client-last-pong-sent))))
    (if (and elapsed (> elapsed 10))
        (progn
          (sip-ws-log "ws: attempting to reconnect...")
          (setq sms-fanout-client (sms-fanout-connect)))
      (warn "sip-sms-ws: giving up on reconnect. elapsed: %s" elapsed))))


(defun sip-ws-log (log-message)
  (with-current-buffer (get-buffer-create "*sip-sms-ws*")
    (goto-char (point-max))
    (insert (format "%s %s" (format-time-string "%Y-%m-%d at %H:%M:%S")
                    (substring log-message 0 (min 1000 (length log-message)))))
    (newline-and-indent)))

(defun sms-fanout-on-message (json)
  (cl-assert json)
  (unless (listp json)
    (error "non-json argument: %s" json))
  (alist-let json (status message-type)
    (cl-assert message-type)
    (cl-assert status)
    (cond
     ((not (zerop status))
      (error "Non-Zero status from server: %s" text))
     ((s-starts-with-p "push-messages/" message-type)
      (let ((messages (alist-get 'body json))
            (supress-echo (or (equal message-type "push-messages/old")
                              sip-quiet-p)))
        (sip-ws-log (format "received %s messages" (length messages)))
        (cl-loop
         for message across messages
         for i from 1
         do (sip-ws-log (format "on message %s/%s" i (length messages)))
         do
         (alist-let message (to from message id date)
           (if (null date)
               (sip-ws-log (format "skipping message with null date: %s" message))
             (condition-case err
                 (sip-message-received (make-sip-message
                                        :id id :to to
                                        :from from
                                        :message message
                                        :date date)
                                       supress-echo)
               (error
                (sip-ws-log (format "failed to insert message %s: %s" message err)))))))))
     ((s-starts-with-p "status" message-type)
      ;;
      )
     (t
      (error "unexpected message type: %s" message-type)))))

(defun sms-fanout-disconnect ()
  (setq sms-last-connection-timestamp nil
        sms-fanout-client-last-pong-received nil
        sms-fanout-client-last-pong-sent nil)
  (when sms-fanout-client
    (unless (eq 'closed (websocket-ready-state sms-fanout-client))
      (sip-ws-log "disconnecting...")
      (websocket-close sms-fanout-client))))

(defun url-parse-auth-from-url (url)
  (when-let* ((parts (url-generic-parse-url url))
              (user (url-user parts))
              (password (url-password parts)))
    (when (and user password)
      (cons user password))))

(defun url-auth-to-header (url)
  (let ((auth (url-parse-auth-from-url url)))
    (when auth
      (cl-destructuring-bind (user . password) auth
        (let ((base64 (base64-encode-string (format "%s:%s" user password))))
          (cons "Authorization" (format "Basic %s" base64)))))))


(defun sms-fanout-connect ()
  (sms-fanout-disconnect)
  (let ((auth-header-opt (url-auth-to-header sms-fanout-address))
        custom-headers)
    (when auth-header-opt
      (push auth-header-opt custom-headers))
    (websocket-open
     sms-fanout-address
     :on-open (lambda (_websocket)
                (sip-ws-log (format "ws connected to %s" sms-fanout-address))
                (setq sms-fanout-client-last-pong-received (float-time)
                      sms-last-connection-timestamp (float-time)))
     :on-message (lambda (_websocket frame)
                   (setq sms-fanout-client-last-pong-received (float-time))
                   (let* ((text (websocket-frame-text frame))
                          (json (json-parse-whole-string
                                 (or text (error "text is nil")))))
                     (sip-ws-log (format "received: %s" text))
                     (sms-fanout-on-message json)))
     :on-close (lambda (_websocket)
                 (sip-ws-log
                  (if (null sms-last-connection-timestamp)
                      "on-close: ws was never opened"
                    (format "ws closed after %s seconds"
                            (floor
                             (- (float-time) sms-last-connection-timestamp))))))
     :on-error (lambda (_websocket callback-id err)
                 (sip-ws-log
                  (format "ws closed with error: %s %s" callback-id err)))
     :custom-header-alist custom-headers)))

(define-minor-mode sip-chat-mode
  "Sip chat minor mode"
  :lighter " sip-chat-mode"
  :keymap
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") #'sip-send-chat-line)
    (define-key kmap (kbd "s-i") #'sip-chat-send-automated-message)
    kmap)
  (visual-line-mode t))

(autobuild-define-rule sip-chat-send (sip-chat-mode)
  (autobuild-nice 6)
  #'sip-send-chat-line)

(defun sms-fanout-client-loop ()
  (let (err)
    (when-let ((disconnected (sms-fanout-disconnected-p)))
      (sip-ws-log (format "disconnected: %s. attempting to reconnect on loop"
                          disconnected))
      (condition-case ex
          (setf sms-fanout-client (sms-fanout-connect))
        (error
         (sip-ws-log (format "unable to connect: %s" ex))
         (setq err ex))))
    (unless err
      (sip-ws-log "pinging")
      (websocket-send-text sms-fanout-client "ping")
      (setq sms-fanout-client-last-pong-sent (float-time)))))

(defun sip-chat-buffers-list ()
  (cl-loop for buffer in (buffer-list)
           when (s-starts-with-p "#sip-sms-" (buffer-name buffer))
           collect buffer))

(defun sip-chat-last-message ()
  (goto-char (point-max))
  (if (re-search-backward "^.*? says: .*" nil t)
      (buffer-substring (match-beginning 0) (point-max))
    (progn (sip-ws-log (format "no message found on buffer %s" buffer))
           nil)))

(defun sip-chat-menu ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*sip-chat-menu*"))
  (read-only-mode 0)
  (erase-buffer)
  (save-match-data
    (cl-loop
     for buffer in (sort-by
                    (sip-chat-buffers-list)
                    (lambda (buffer)
                      ;; '(buffer-local-value 'sip-max-timestamp buffer)
                      (with-current-buffer buffer
                        (if (bound-and-true-p sip-max-timestamp)
                            sip-max-timestamp
                          -1))))
     as line = (with-current-buffer buffer
                 (sip-chat-last-message))
     as phone-number = (buffer-local-value 'sip-from-phone-number buffer)
     as did = (buffer-local-value 'sip-did buffer)
     when line
     do (let ((map (make-sparse-keymap)))
          (define-key map (kbd "RET") `(lambda () (interactive)
                                         (if (buffer-live-p ,buffer)
                                             (switch-to-buffer ,buffer)
                                           (sip-chat ,phone-number ,did))))
          (insert (s-replace "\n" "\\n" line))
          (put-text-property (line-beginning-position)
                             (line-end-position) 'keymap map)
          (add-text-properties
           (line-beginning-position)
           (line-end-position)
           '(link highlight
                  help-echo
                  "mouse-2: visit chat buffer for this conversation"))
          (newline))))
  (read-only-mode t))

(defvar sms-fanout-client-timer nil)

(defun sms-fanout-client-start-timer ()
  (sms-fanout-client-stop-timer)
  (sms-fanout-disconnect)
  (setq sms-fanout-client-timer
        (run-at-time nil sms-fanout-ping-interval-seconds
                     #'sms-fanout-client-loop)))

(defun sms-fanout-client-stop-timer ()
  (when sms-fanout-client-timer
    (sip-ws-log (format "stopping previous timer"))
    (cancel-timer sms-fanout-client-timer)
    (setq sms-fanout-client-timer nil)))

(defun sms-fanout-read-address ()
  (when-let* ((info (authinfo-get-by-app "smsfan"))
              (api-key (alist-get 'api-key info))
              (base-url (alist-get 'base-url info)))
    (format "%s/fanout?api-key=%s" base-url api-key)))

(setq sip-cursor-colors
      '(
        "IndianRed" "ForestGreen" "DarkCyan" "DarkOliveGreen" "MidnightBlue"
        "MediumPurple3" "LightSalmon"
        "LightYellow4"  "LimeGreen"  "MediumAquamarine"
        "MediumBlue"  "MediumOrchid"  "MediumOrchid1"
        "MediumOrchid2"
        "MediumOrchid3"  "MediumOrchid4"
        "MediumPurple"  "MediumPurple1"  "MediumPurple2"
        "MediumPurple3"
        "MediumPurple4"
        "MediumSeaGreen"  "MediumSlateBlue"
        "MediumSpringGreen"  "MediumTurquoise"
        "MediumVioletRed"  "MidnightBlue"  "MintCream"
        "MistyRose"  "MistyRose1"  "MistyRose2"  "MistyRose3"
        "MistyRose4"  "NavajoWhite"  "NavajoWhite1"
        "NavajoWhite2"  "NavajoWhite3"  "NavajoWhite4"  "NavyBlue"
        "OldLace"  "OliveDrab"  "OliveDrab1"
        "OliveDrab2"  "OliveDrab3"  "OliveDrab4"
        "OrangeRed"  "OrangeRed1"  "OrangeRed2"  "OrangeRed3"
        "OrangeRed4"  "PaleGoldenrod"  "PaleGreen"
        "PaleGreen1"  "PaleGreen2"  "PaleGreen3"  "PaleGreen4"))

(defun sip-color-cursor-from-identity ()
  (let* ((user (cl-first (sip-current-identity)))
         (hash-code (sxhash-equal user))
         (index (mod hash-code (length sip-cursor-colors)))
         (color (nth index sip-cursor-colors)))
    (set-cursor-color color)))

(defvar sip-color-cursor-from-identity-timer nil)

(defun sip-color-cursor-from-identity-schedule ()
  (when
      (and sip-chat-mode (null sip-color-cursor-from-identity-timer))
    (setq sip-color-cursor-from-identity-timer
          (run-at-time 1
                       nil
                       (lambda ()
                         (let ((sip-inhibit-echo-linphone-command t))
                           (sip-color-cursor-from-identity))
                         (setq sip-color-cursor-from-identity-timer nil))))))

(add-hook 'buffer-list-update-hook
          #'sip-color-cursor-from-identity-schedule)

(defun sip-restart ()
  (setq buffer-list-update-hook nil)
  (if (not (setq sms-fanout-address (sms-fanout-read-address)))
      (warn "no sms fanout address could be parsed")
    (setq websocket-callback-debug-on-error t)
    (sms-fanout-client-start-timer)))

(defun sip-stop ()
  (interactive)
  (setq buffer-list-update-hook nil)
  (setq sms-fanout-address nil)
  (setq websocket-callback-debug-on-error t)
  (sms-fanout-client-stop-timer))

;; (sip-stop)
(when (sms-fanout-read-address)
  (sip-restart))
