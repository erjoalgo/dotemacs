(require 'cl-lib)
(require 'websocket)
(require 's)

(defvar sms-fanout-address nil)
(defvar sms-fanout-client nil)
(defvar sms-fanout-client-last-pong-sent nil)
(defvar sms-fanout-client-last-pong-received nil)
(defvar sms-fanout-reconnect-interval-mins 1)
(defvar sms-fanout-ping-interval-seconds 30)
(defvar sip-last-known-identity nil)
(defvar sip-inhibit-echo-linphone-command nil)
(defvar sip-messages (make-hash-table))

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

(defun json-parse (json)
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (json-read)))

(defvar sip-buffer-fmt "#sip-sms-%s")

(defvar-local sip-from-phone-number nil)
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

(defun sip-send-chat-line ()
  (interactive)
  (cl-assert (bound-and-true-p sip-from-phone-number))
  (goto-char (point-max))
  (let* ((current-identity (sip-current-identity))
        (message (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))
        (sip-address (sip-phone-number-to-address sip-from-phone-number
                                                  (cl-second current-identity))))
    (unless (or (null sip-last-known-identity)
                (equal current-identity sip-last-known-identity)
                (y-or-n-p (format "use new sip identity: %s? "
                                  (s-join "@" current-identity))))
      (error "identity change not acknowledged"))
    (setq sip-last-known-identity current-identity)
    (linphonecsh "generic" (format "chat %s %s" sip-address message))
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

(defun sip-chat-buffer (other-number &optional self-number)
  (cl-assert (not (s-blank? other-number)))
  (let* ((other-number-clean (sip-phone-number-clean other-number))
         (buffer-name (format sip-buffer-fmt
                              (concat other-number-clean
                                      (when self-number
                                        (concat "-to-" self-number)))))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (sip-chat-mode)
      (setq sip-from-phone-number other-number-clean)
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

(cl-defstruct sip-message id from to message timestamp)

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
  (my-with-slots sip-message (id from to message timestamp) sip-message
    (if (null (sip-add-message sip-message))
        (sip-ws-log (format "skipping previously-received message with id %s" id))
      (let* ((buffer (sip-chat-buffer from to))
             (line (format "%s says: %s" from message))
             (timestamp
              (condition-case ex (->> timestamp
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

(defun sip-chat (number &optional message)
  (interactive "senter phone number: \nsenter message: ")
  (let* ((number-clean (sip-clean-phone-number number))
         (buffer (sip-chat-buffer number-clean)))
    (switch-to-buffer buffer)
    (sip-chat-maybe-autosend-message)))

(defun sip-message-exists-p (number message)
  (with-current-buffer (sip-chat-buffer number)
    (save-excursion
      (goto-char (point-min))
      (search-forward message))))

(defun sip-chat-mass (numbers message)
  (interactive "senter newline-separated phone numbers: \nsenter sms message: ")
  (cl-loop for number in (split-string numbers "\n" t)
           if (sip-message-exists-p number message)
           do (message "skipping previously-sent message to %s" number)
           else do (progn
                     (sip-chat number message)
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
  (alist-let json (status message-type)
    (cl-assert message-type)
    (cl-assert status)
    (cond
     ((not (zerop status))
      (error "Non-Zero status from server: %s" text))
     ((s-starts-with-p "push-messages/" message-type)
      (let ((messages (alist-get 'body json))
            (supress-echo (equal message-type "push-messages/old")))
        (sip-ws-log (format "received %s messages" (length messages)))
        (cl-loop
         for message across messages
         for i from 1
         do (sip-ws-log (format "on message %s/%s" i (length messages)))
         do
         (alist-let message (to from message id timestamp)
           (if (null timestamp)
               (sip-ws-log (format "skipping message with null timestamp: %s" message))
             (condition-case err
                 (sip-message-received (make-sip-message
                                        :id id :to to
                                        :from from
                                        :message message
                                        :timestamp timestamp)
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

(defun sms-fanout-connect ()
  (sms-fanout-disconnect)
  (websocket-open
   sms-fanout-address
   :on-open (lambda (_websocket)
              (sip-ws-log "ws connected")
              (setq sms-fanout-client-last-pong-received (float-time)
                    sms-last-connection-timestamp (float-time)))
   :on-message (lambda (_websocket frame)
                 (setq sms-fanout-client-last-pong-received (float-time))
                 (let* ((text (websocket-frame-text frame))
                        (json (json-parse (or text (error "text is nil")))))
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
                (format "ws closed with error: %s %s" callback-id err)))))

(define-minor-mode sip-chat-mode
  "Sip chat minor mode"
  :lighter " sip-chat-mode"
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") #'sip-send-chat-line)
    (define-key kmap (kbd "s-i") #'sip-chat-send-automated-message)
    kmap)
  (let ((real-message #'message))
  (cl-letf (
            ;; '(real-message #'message)
            ((symbol-function #'message)
             (lambda (fmt &rest args)
               (if (equal fmt "Truncate long lines %s")
                   '(funcall real-message "DEBUG kx5a TRACE")
                 '(funcall real-message fmt args)))))
    (toggle-truncate-lines nil)))
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
     when line
     do (let ((map (make-sparse-keymap)))
          (define-key map (kbd "RET") `(lambda () (interactive)
                                         (if (buffer-live-p ,buffer)
                                             (switch-to-buffer ,buffer)
                                           (sip-chat ,phone-number))))
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
  (when-let* ((info (authinfo-get-by-app "sms-fanout"))
              (api-key (alist-get 'password info))
              (hostname (alist-get 'machine info)))
    (format "wss://%s/fanout?api-key=%s" hostname api-key)))

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
  (let* ((user (first (sip-current-identity)))
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
  (unless sms-fanout-address
    (setq sms-fanout-address (sms-fanout-read-address)))
  (setq websocket-callback-debug-on-error t)
  (when sms-fanout-address
    (sms-fanout-client-start-timer)))

(defun sip-stop ()
  (interactive)
  (setq buffer-list-update-hook nil)
  (setq sms-fanout-address nil)
  (setq websocket-callback-debug-on-error t)
  (sms-fanout-client-stop-timer))

(sip-restart)
