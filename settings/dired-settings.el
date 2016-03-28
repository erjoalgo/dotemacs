(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "q" 'dired-up-directory)
     ))

;;don't prompt me
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))

