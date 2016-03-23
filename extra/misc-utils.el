(defun whereis (program)
  (interactive "senter program: ")
  (let ((dirs
	 (loop for dir in (split-string (getenv "PATH") ":" t)
	       if (and (file-exists-p dir)
		       (member program (directory-files dir)))
	       collect (f-join dir program))))

    (when (called-interactively-p 'interactively)
      (message "%s" dirs))
  dirs))

(defun recover-this-file-and-diff ()
  (interactive)
  (recover-this-file);;TODO ignore prompt
  (diff-buffer-with-file))

(defun shell-command-of-region (a b)
  "compare to shell-command-on-region"
  (interactive "r")
  ;(shell-command-to-string (buffer-substring a b))
  (async-shell-command (buffer-substring a b)))

(defun lnabs (source &optional prompt)
  (interactive "fEnter soft link source: ")
  (let* ((source (expand-file-name source))
	 (base (f-filename source))
	 (destination (read-file-name "enter destination: " nil base nil nil nil ))
	 (command (format "ln -sf %s %s" source (expand-file-name destination))))
    (when (or (not prompt) (y-or-n-p command))
      (shell-command command))))

(cl-defun sort-key (list key &key descending (pred '<))
  (let* ((sorted-tuples (sort (mapcar (lambda (el)
				(cons el (funcall key el)))
			      list)
		      (lambda (a b)
			(funcall pred (cdr a) (cdr b)))))
	(sorted (mapcar 'car sorted-tuples)))
    (if descending (reverse sorted)
      sorted)))

  
			
  
(defun directory-files-sort-by-ctime-descending (dir)
  (let ((files (directory-files dir)))
    (reverse (sort-key files (lambda (fn)
		  (let ((attrs (file-attributes (f-join dir fn))))
		    (nth 6 attrs)))))))

  
		  
