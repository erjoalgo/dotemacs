(require 'websocket)

(defvar sms-fanout-address nil)

(defvar sms-fanout-client nil)

(defvar sms-fanount-reconnect-interval-mins 1)

(defvar sms-fanout-address)

(defun sms-fanout-connected-p (&optional client)
  (let ((client (or client sms-fanout-client)))
    (and client
         (websocket-openp client)
         client)))

(defun json-parse (json)
  (with-temp-buffer
    (insert json)
    (goto-char (point-min))
    (json-read)))

(setq sip-buffer-fmt "#sip-sms-%s")

(defvar-local sip-from-phone-number nil)

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

(defun sip-send-chat-line ()
  (interactive)
  (cl-assert (bound-and-true-p sip-from-phone-number))
  (let ((message (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))
        (sip-address (sip-phone-number-to-address sip-from-phone-number)))
    (linphonecsh "generic" (format "chat %s %s" sip-address message))
    ;; maybe add a newline
    (goto-char (point-max))
    (unless (eq (point) (line-beginning-position))
      (newline-and-indent))))

(defvar sip-last-message-buffer nil)

(defun sip-message-received (to from message)
  (let* ((buffer-name (format sip-buffer-fmt (concat to "-from-" from)))
         (buffer (get-buffer-create buffer-name))
         (line (format "%s says: %s" from message)))
    (with-current-buffer buffer
      (sip-chat-mode t)
      (goto-char (point-max))
      (unless (eq (point) (line-beginning-position))
        (goto-char (line-beginning-position))
        (open-line 1))
      (insert line)
      (newline-and-indent)
      (setq-local sip-from-phone-number from))
    (message "%s" line)
    (setq sip-last-message-buffer buffer)))

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
              (message "ws connected")
              (setq sms-last-connection-timestamp (float-time)))
   :on-message
   (lambda (_websocket frame)
     (let* ((text (websocket-frame-text frame))
            (json (json-parse text)))
       (sip-ws-log (format "received: %s" text))
       (alist-let json (to from message status code)
         (if status
             (progn (cl-assert (and status code))
                    (unless (zerop code)
                      (websocket-close _websocket)))
           (progn
             (cl-assert (and to from message))
             (sip-message-received to from message))))))
   :on-close (lambda (_websocket)
               (sip-ws-log
                (format "ws closed after %s seconds"
                        (floor
                         (- (float-time) sms-last-connection-timestamp))))
               (sip-ws-maybe-reconnect))
   :on-error (lambda (_websocket callback-id err)
               (sip-ws-log (format "ws closed with error: %s %s" callback-id err)))))

(unless (sms-fanout-connected-p)
  (setf sms-fanout-client (sms-fanout-connect)))

(define-minor-mode sip-chat-mode
  "Sip chat minor mode"
  nil
  "sip-chat-mode"
  (make-sparse-keymap)
  (toggle-truncate-lines t))

(autobuild-define-rule sip-chat-send (sip-chat-mode)
  (autobuild-nice 6)
  #'sip-send-chat-line)

;; (sms-fanout-connect)
;; (sms-fanout-connected-p)
