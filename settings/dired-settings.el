(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "q" 'dired-up-directory)
     ))

;;don't prompt me
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))

(with-eval-after-load "dired"
  ;;TODO
  (define-key dired-mode-map (kbd "s-c")
    (lambda () (interactive) (set-clipboard (dired-file-name-at-point))))
  
  (define-key dired-mode-map (kbd "s-f")
    (lambda () (interactive) (open-file (dired-file-name-at-point)))))
