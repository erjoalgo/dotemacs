(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "q" 'dired-up-directory)
     ))
