(defun whereis (program)
  (interactive "senter program: ")
  (let (dirs)
    (setf dirs
	  (remove
	   nil
	   (loop for dir in (split-string (getenv "PATH") ":" t)
		 if (file-exists-p dir)
		 collect (loop for fn in (directory-files dir)
			       thereis
			       (and (string= program fn)
				    (f-join dir fn))))))
    (if (called-interactively-p 'interactively)
	(message "%s" (remove nil dirs))
      (remove nil dirs))))

(defun recover-this-file-and-diff ()
  (interactive)
  (recover-this-file);;TODO ignore prompt
  (diff-buffer-with-file))


(defun shell-command-of-region (a b)
  (interactive "r")
  ;(shell-command-to-string (buffer-substring a b))
  (async-shell-command (buffer-substring a b)))

;;stands for stack exchange...
(defun sexchange-insert-cmd-and-output (cmd)
  (interactive (list (read-shell-command
		      "senter shell command: ")))
  (insert (format "\n\n%s\n%s\n\n"
		  (sexchange-code-block
		   (concat "$" cmd))
		  (sexchange-code-block (shell-command-to-string cmd)))))

(defun sexchange-code-block (code)
  (replace-regexp-in-string
   "^" "    " code))


(defun sexchange-code-block-region (a b)
  (interactive "r")
  (save-excursion
    (goto-char a)
    (while (< (point) b)
      (beginning-of-line)
      (insert "    ")
      (next-logical-line))))
  
(defun elisp-tab ()
  "Indent or complete."
  ;;based on ielm-tab
  (interactive)
  (if (or (eq (preceding-char) ?\n)
          (eq (char-syntax (preceding-char)) ?\s))
      (indent-for-tab-command)
    (completion-at-point)))
