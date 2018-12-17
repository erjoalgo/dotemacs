(let ((warnings (get-buffer "*Warnings*")))
  (if (not warnings)
      (kill-emacs)
    (dolist (buff (list (get-buffer "*Messages*") warnings))
      (when buff
        (with-current-buffer buff
          (print (buffer-substring-no-properties
                  (point-min)
                  (point-max))
                 #'external-debugging-output))))
    (kill-emacs 1)))
