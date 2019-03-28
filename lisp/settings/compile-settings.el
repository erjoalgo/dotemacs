(setq compilation-always-kill t)

(defvar compilation-interpret-ansi-color nil)

(setf compilation-interpret-ansi-color t);; todo make buffer-local and mode-local

(setf compilation-save-buffers-predicate (lambda () nil))

;;taken from
;;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)

(setf compilation-ask-about-save nil)

(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

(defun maybe-colorize-compilation-buffer ()
  ;; (require 'ansi-color)
  ;; https://stackoverflow.com/questions/3072648/
  (when compilation-interpret-ansi-color
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(add-hook 'compilation-filter-hook 'maybe-colorize-compilation-buffer)

(defadvice next-error-find-buffer (around prioritize-compilation-buffer activate)
  (setq ad-return-value
        (if (and (bound-and-true-p autobuild-last-compilation-buffer)
                 (buffer-live-p autobuild-last-compilation-buffer))
            (progn
              autobuild-last-compilation-buffer)
          ad-do-it)))

(put 'compile-command 'safe-local-variable 'stringp)

(defun compilation-finished-notify (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (let ((msg (format "compilation %s: %s"
                     (s-trim compilation-state) compile-command))
        (color (if (autobuild-compilation-succeeded-p compilation-state)
                   'green 'red)))
    (stumpwm-message msg color)))

(setq autobuild-notification-function #'compilation-finished-notify
      autobuild-notify-threshold-secs 10)

(defun compilation-autorename-existing-buffer (&rest _args)
  "Start new compilations without deleting the current *compilation* buffer."
  (let* ((name "*compilation*")
         (compilation (get-buffer name)))
    (when compilation
      (with-current-buffer compilation
        (rename-buffer name t)
        (message "auto-renaming %s buffer to %s"
                 name (current-buffer))))))


(advice-add 'compilation-start :before #'compilation-autorename-existing-buffer)

1
