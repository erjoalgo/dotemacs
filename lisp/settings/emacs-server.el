(defun server-init  ()
  (require 'server)
  (setq server-name
        (format "%s.%s"
                (or (getenv "DESKTOP_GROUP_NUMBER") server-name)
                (random 1000)))
  (server-force-delete server-name)
  (server-start)
  (cl-assert (server-running-p server-name)))

(server-init)

(defun sig-usr1 ()
  "start or restart the server on USR1"
  (interactive)
  (message "got sigusr1! at %s" (format-time-string "%H:%M:%S"))
  (server-force-delete)
  (server-start))

(define-key special-event-map [sigusr1] 'sig-usr1)

(defun server-give-up-daemon ()
  "pass daemon to another emacs process"
  (let ((cands (->>
                 (s-split "\n" (shell-command-to-string "pidof emacs") t)
                 (mapcar 'string-to-number)
                 (cl-remove-if (lambda (pid) (= (emacs-pid) pid))))))
    (when cands
      (shell-command (format "kill -USR1 %d" (car cands))))))

(add-hook 'kill-emacs-hook 'server-give-up-daemon)


(defun sig-usr2-reset-debug-on-quit ()
  ;; (interactive)
  "Reset debug-on-quit to OFF after a USR2 signal."
  (setq debug-on-quit nil))

(define-key special-event-map [sigusr2] 'sig-usr2-reset-debug-on-quit)
