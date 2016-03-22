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
  (let ((cmd (buffer-substring a b)))
    (async-shell-command cmd)))

(defun shell-command-of-current-line ()
  (interactive)
  (shell-command-of-region
   (line-beginning-position)
   (line-end-position)))
  
(defun lnabs (source &optional prompt)
  (interactive "fEnter soft link source: ")
  (let* ((source (expand-file-name source))
	 (base (f-filename source))
	 (destination (read-file-name "enter destination: " nil base nil nil nil ))
	 (command (format "ln -sf %s %s" source (expand-file-name destination))))
    (when (or (not prompt) (y-or-n-p command))
      (shell-command command))))


