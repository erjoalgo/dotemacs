(require 'websocket)

(defvar sms-fanout-address nil)
(defvar sms-fanout-hostname nil)
(defvar sms-fanout-client nil)
(defvar sms-fanout-client-last-pong-sent nil)
(defvar sms-fanout-client-last-pong-received nil)
(defvar sms-fanout-reconnect-interval-mins 1)
(defvar sms-fanout-ping-interval-seconds 30)

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

(setq sip-buffer-fmt "#sip-sms-%s")

(defvar-local sip-from-phone-number nil)
(defvar-local sip-max-timestamp nil)

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  (message "running: linphonecsh %s" (string-join args " "))
  (apply #'call-process "linphonecsh" nil "*linphonecsh*" nil args))

(defvar *sip-default-host* "sanjose2.voip.ms")

(defun sip-phone-number-clean (number)
  (let* ((number-clean (replace-regexp-in-string "[^0-9]" "" number))
         (number-clean
          (if (and (eq 11 (length number-clean))
                   (s-starts-with-p "1" number-clean))
              (substring number-clean 1)
            number-clean)))
    number-clean))

(defun sip-phone-number-to-address (number &optional sip-host)
  (let* ((sip-host (or sip-host *sip-default-host*))
         (number-clean (sip-phone-number-clean number))
         (sip-address (format "sip:%s@%s" number-clean sip-host)))
    sip-address))

(defvar sip-message-ids-received (make-hash-table))

(defun sip-get-message-id (id &optional add)
  (let* ((id (if (stringp id) (string-to-number id) id))
         (exists (gethash id sip-message-ids-received)))
    (when (and add (not exists))
      (puthash id t sip-message-ids-received))
    add))

(defun sip-add-message-id (id)
  (sip-get-message-id id t))

(defun sip-send-chat-line ()
  (interactive)
  (cl-assert (bound-and-true-p sip-from-phone-number))
  (let ((message (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))
        (sip-address (sip-phone-number-to-address sip-from-phone-number)))
    (linphonecsh "generic" (format "chat %s %s" sip-address message))
    (goto-char (line-beginning-position))
    (insert "        YOU say: ")
    ;; maybe add a newline
    (goto-char (point-max))
    (unless (eq (point) (line-beginning-position))
      (newline))))

(defvar sip-last-message-buffer nil)

(defun sip-chat-buffer (other-number &optional self-number)
  (cl-assert (not (s-blank-p other-number)))
  (let* ((other-number-clean (sip-phone-number-clean other-number))
         (buffer-name (format sip-buffer-fmt
                              (concat other-number-clean
                                      (when self-number
                                        (concat "-to-" self-number)))))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (sip-chat-mode)
      (setq sip-from-phone-number other-number-clean))
    buffer))

(defun sip-message-received (to from message id datetime)
  (if (sip-get-message-id id)
      (sip-ws-log (format "skipping previously-received message with id %s" id))
    (let* ((buffer (sip-chat-buffer from to))
           (line (format "%s says: %s" from message))
           (timestamp (condition-case ex
                          (->> datetime
                            parse-time-string
                            (apply #'encode-time)
                            time-to-seconds)
                        (warn "failed to parse timestamp: %s"
                              'datetime)))
           was-at-bottom)
      (with-current-buffer buffer
        (sip-chat-mode t)
        (setq was-at-bottom (eq (point-max) (point)))
        (save-excursion
          (goto-char (point-max))
          (goto-char (line-beginning-position))
          (open-line 1)
          (insert line)
          (setq-local sip-from-phone-number from)
          (setq-local sip-max-timestamp (max timestamp (or sip-max-timestamp 0))))
        (when was-at-bottom
          (goto-char (point-max))))
      (message "%s" line)
      (setq sip-last-message-buffer buffer)
      (sip-add-message-id id))))

(defun sip-clean-phone-number (number)
  (replace-regexp-in-string "[^0-9]" "" number))

(defun sip-chat (number)
  (let* ((number-clean (sip-clean-phone-number number))
         (buffer (sip-chat-buffer number-clean)))
    (switch-to-buffer buffer)))

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
    (insert log-message)
    (newline-and-indent)))

(defun sms-fanout-on-message (json)
  (cl-assert json)
  (alist-let json (status message-type)
    (cl-assert message-type)
    (cl-assert status)
    (cond
     ((not (zerop status))
      (error "non-zero status from server: %s" text))
     ((s-starts-with-p "push-messages/" message-type)
      ;; (setq sip-messages json)
      (let ((messages (alist-get 'body json)))
        (if (null messages)
            (error "0 messages in body")
          (cl-loop
           for message across messages
           do (message "DEBUG xfbo message: %s" message)
           do
           (alist-let message (to from message id timestamp)
             (sip-message-received to from message id timestamp))))))
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
    (websocket-close sms-fanout-client)))

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
  nil
  "sip-chat-mode"
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") #'sip-send-chat-line)
    kmap)
  (toggle-truncate-lines nil)
  (visual-line-mode t))

(autobuild-define-rule sip-chat-send (sip-chat-mode)
  (autobuild-nice 6)
  #'sip-send-chat-line)

(defun sms-fanout-client-loop ()
  (when-let ((disconnected (sms-fanout-disconnected-p)))
    (sip-ws-log (format "disconnected: %s. attempting to reconnect on loop"
                        disconnected))
    (condition-case ex
        (setf sms-fanout-client (sms-fanout-connect))
      (error (sip-ws-log (format "unable to connect: %s" ex)))))
  (websocket-send-text sms-fanout-client "ping")
  (setq sms-fanout-client-last-pong-sent (float-time)))

(defun sip-chat-buffers-list ()
  (cl-loop for buffer in (buffer-list)
           when (s-starts-with-p "#sip-sms-" (buffer-name buffer))
           collect buffer))

(defun sip-chat-last-message ()
  (goto-char (point-max))
  (if (re-search-backward "^\\([0-9]+\\| *YOU\\) +says?: .*" nil t)
      (buffer-substring (match-beginning 0) (point-max))
    (warn "no message found on buffer %s" buffer)))

(defun sip-chat-menu ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*sip-chat-menu*"))
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
     when line
     do (let ((map (make-sparse-keymap)))
          (define-key map (kbd "RET") `(lambda () (interactive)
                                         (switch-to-buffer ,buffer)))
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
  (when sms-fanout-client-timer
    (sip-ws-log (format "stopping previous timer"))
    (cancel-timer sms-fanout-client-timer))
  (sms-fanout-disconnect)
  (setq sms-fanout-client-timer
        (run-at-time nil sms-fanout-ping-interval-seconds
                     #'sms-fanout-client-loop)))


(defun sms-fanout-read-address ()
  (when-let* ((info (authinfo-get sms-fanout-hostname))
              (api-key (alist-get 'password info)))
    (format "wss://%s/fanout?api-key=%s" sms-fanout-hostname api-key)))

(unless sms-fanout-address
  (setq sms-fanout-address (sms-fanout-read-address)))

(when sms-fanout-hostname
  (sms-fanout-client-start-timer))
