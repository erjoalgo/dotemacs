(defun code-golf-elisp ()
  (interactive)
  (multi-regexp-replace-sequential
   '(("[ \t\n]+" " " )
     (" *(" "(")
     (") *" ")"))))
