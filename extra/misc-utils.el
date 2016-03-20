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
  
(defun lnabs (source &optional prompt)
  (interactive "fEnter soft link source: ")
  (let* ((source (expand-file-name source))
	 (base (f-filename source))
	 (destination (read-file-name "enter destination: " nil base nil nil nil ))
	 (command (format "ln -sf %s %s" source (expand-file-name destination))))
    (when (or (not prompt) (y-or-n-p command))
      (shell-command command))))

(defun message-current-buffer-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
	(message "%s" (process-command proc))
      (message "buffer has no process"))))

