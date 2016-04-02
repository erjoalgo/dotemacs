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
  (let ((cmd (buffer-substring a b)))
    (message "cmd is: %s" cmd)
    (async-shell-command cmd)))

(defun shell-command-of-current-line ()
  (interactive)
  (shell-command-of-region
   (line-beginning-position)
   (line-end-position)))
  
(defun lnabs (source &optional prompt)
  ;;(interactive "fEnter soft link source: ")
  (interactive (list
		(let ((initial (and (eq major-mode 'dired-mode)
				    (dired-file-name-at-point)
				    (f-filename
				     (dired-file-name-at-point)))))
		  
		  (read-file-name
		   "Enter soft link source: "
		   nil initial t initial))))
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

(defun read-symbol-completing (prompt &optional default)
  (intern
   (completing-read prompt obarray nil nil (and default (symbol-name default)))))

(defun add-file-local-variable-mode (mode)
  (interactive (list
		(if (eq major-mode 'fundamental-mode)
		    (read-symbol-completing "enter mode: ")
		  major-mode)))
  ;;first load mode to set the right comment-start
  (when (eq major-mode 'fundamental-mode)
    (funcall mode))
  (add-file-local-variable 'mode mode))
  

(defun check-unsaved-buffers ()
  (interactive)
  (loop as next-buff =
	(loop for buff in (buffer-list)
	      thereis (and
		       (not (get-buffer-process buff))
		       (not (string-match "^[[:space:]]*[*].*[*]$" (buffer-name buff)))
		       (not (member (buffer-local-value 'major-mode buff) '(dired-mode)))
			   (or (not (buffer-file-name buff))
			       (buffer-modified-p buff))
			   buff))
	
	while next-buff do
	(progn (switch-to-buffer next-buff)
	       (message "unsaved changes in: %s... close or save, then exit rec-edit"
			(buffer-name next-buff))
	       (recursive-edit))
	finally (message "done checking buffers")))
