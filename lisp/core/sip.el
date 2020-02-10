(require 'websocket)

(defvar sms-fanout-address nil)
(defvar sms-fanout-client nil)
(defvar sms-fanout-client-last-pong nil)
(defvar sms-fanout-reconnect-interval-mins 1)
(defvar sms-fanout-address)
(defvar sms-fanout-ping-interval-seconds 30)

(defun sms-fanout-connected-p (&optional client)
  (let ((client (or client sms-fanout-client)))
    (when (and client (websocket-openp client)
               (<= sms-fanout-client-last-pong
                   (- (time-now-seconds)
                      (/ sms-fanout-ping-interval-seconds 2))))
      client)))

(defun json-parse (json)
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (json-read)))

(setq sip-buffer-fmt "#sip-sms-%s")

(defvar-local sip-from-phone-number nil)

(defun time-now-seconds ()
  (/ (current-time-ms) 1000))

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  (message "running: linphonecsh %s" (string-join args " "))
  (apply #'call-process "linphonecsh" nil "*linphonecsh*" nil args))

(defvar *sip-default-host* "sanjose2.voip.ms")

(defun sip-phone-number-to-address (number &optional sip-host)
  (let* ((sip-host (or sip-host *sip-default-host*))
         (number-clean (replace-regexp-in-string "[^0-9]" "" number))
         (sip-address (format "sip:%s@%s" number-clean sip-host)))
    sip-address))

(defvar sip-message-ids-received (make-hash-table))

(defun sip-add-message-id (id)
  (let ((id (if (stringp id) (string-to-number id) id)))
    (unless (gethash id sip-message-ids-received)
      (puthash id t sip-message-ids-received)
      t)))

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

(defun sip-message-received (to from message id)
  (if (not (sip-add-message-id id))
      (sip-ws-log (format "skipping previously-received message with id %s" id))
    (let* ((buffer-name (format sip-buffer-fmt (concat from "-to-" to)))
           (buffer (get-buffer-create buffer-name))
           (line (format "%s says: %s" from message)))
      (with-current-buffer buffer
          (sip-chat-mode t)
        (save-excursion
          (goto-char (point-max))
          (unless (eq (point) (line-beginning-position))
            (goto-char (line-beginning-position))
            (open-line 1))
          (insert line)
          (setq-local sip-from-phone-number from)))
      (message "%s" line)
      (setq sip-last-message-buffer buffer))))

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
  (let ((elapsed (when sms-last-connection-timestamp
                   (- (float-time) sms-last-connection-timestamp))))
    (if (and elapsed (> elapsed 10))
        ;; throttle reconnection
        (progn
          (sip-ws-log "ws: attempting to reconnect...")
          (setq sms-fanout-client (sms-fanout-connect)))
      (warn "sip-sms-ws: giving up on reconnect. elapsed: %s" elapsed))))


(defun sip-ws-log (log-message)
  (with-current-buffer (get-buffer-create "*sip-sms-ws*")
    (goto-char (point-max))
    (insert log-message)
    (newline-and-indent)))

(defun sms-fanout-connect ()
  (when-let ((current (sms-fanout-connected-p)))
    (websocket-close current))
  (websocket-open
   sms-fanout-address
   :on-open (lambda (_websocket)
              (sip-ws-log "ws connected")
              (setq sms-last-connection-timestamp (float-time)))
   :on-message
   (lambda (_websocket frame)
     (let* ((text (websocket-frame-text frame))
            (json (json-parse text)))
       (sip-ws-log (format "received: %s" text))
       (setq sms-fanout-client-last-pong (time-now-seconds))
       (alist-let json (status message-type)
         (cond
          ((not (zerop status))
           (websocket-close _websocket)
           (error "non-zero status from zerver: %s" text))
          ((s-starts-with-p "status" message-type)
           ;;
           )
          ((not (s-starts-with-p "push-messages/" message-type))
           (websocket-close _websocket)
           (error "unexpected message type: %s" message-type))
          (t
           (setq sip-messages json)
           (let ((messages (alist-get 'body json)))
               (if (null messages)
                   (error "0 messages in body")
                 (cl-loop
                  for message across messages
                  do
                  (alist-let message (to from message id)
                    (sip-message-received to from message id))))))))))
   :on-close (lambda (_websocket)
               (sip-ws-log
                (format "ws closed after %s seconds"
                        (floor
                         (- (float-time) sms-last-connection-timestamp))))
               (sip-ws-maybe-reconnect))
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
  (toggle-truncate-lines t))

(autobuild-define-rule sip-chat-send (sip-chat-mode)
  (autobuild-nice 6)
  #'sip-send-chat-line)

(defun sms-fanout-client-loop ()
  (unless (or (sms-fanout-connected-p) (null sms-fanout-address))
    (sip-ws-log (format "attempting to reconnect on loop"))
    (condition-case ex
        (setf sms-fanout-client (sms-fanout-connect))
      (websocket-send-text sms-fanout-client "ping")
      (error (sip-ws-log (format "unable to connect: %s" ex))))))

(defvar sms-fanout-client-timer nil)

(defun sms-fanout-client-start-timer ()
  (when sms-fanout-client-timer
    (sip-ws-log (format "stopping previous timer"))
    (cancel-timer sms-fanout-client-timer))
  (setq sms-fanout-client-timer
        (run-at-time nil sms-fanout-ping-interval-seconds
                     #'sms-fanout-client-loop)))

(when sms-fanout-client
  (websocket-close sms-fanout-client))

(sms-fanout-client-start-timer)
;; (sms-fanout-connected-p)
