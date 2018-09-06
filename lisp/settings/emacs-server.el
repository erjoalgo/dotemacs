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
                (remove-if (lambda (pid) (= (emacs-pid) pid))))))
    (when cands
      (shell-command (format "kill -USR1 %d" (car cands))))))

(add-hook 'kill-emacs-hook 'server-give-up-daemon)
