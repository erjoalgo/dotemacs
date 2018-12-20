(let ((warnings (get-buffer "*Warnings*")))
  (dolist (buff (list (get-buffer "*Messages*") warnings))
    (when buff
      (with-current-buffer buff
        (print (buffer-substring-no-properties
                (point-min)
                (point-max))
               #'external-debugging-output))))
  (kill-emacs (when warnings 1)))
