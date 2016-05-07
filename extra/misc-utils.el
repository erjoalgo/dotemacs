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

(defun shell-command-of-buffer ()
  "like shell-command-of-region"
  (interactive)
  (shell-command-of-region (point-min) (point-max)))

(defun shell-command-of-current-line ()
  "like shell-command-of-region"
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

(defvar *shred-rec-default-times* 10)
(defun shred-rec (fn &optional shred-times)
  ;;(interactive "fEnter soft link source: ")
  (interactive (list
		(if (and
		     (eq major-mode 'dired-mode)
		     (dired-file-name-at-point))
		    (f-filename
		     (dired-file-name-at-point))
		  (read-file-name "enter fn to shred: "))))
  (unless shred-times
    (setf shred-times *shred-rec-default-times*))
  (y-or-n-p (format "confirm shred %s: " fn))
  (let ((shred-times-string (int-to-string shred-times)))
    
  (if (file-directory-p fn)
      (and (y-or-n-p (format "confirm recursive shred of %s: " fn))
	   (progn
	     (start-process "rec-shred" "rec-shred" "find" fn "-type" "f"
			  "-exec" "shred" "-zufn" 
			  shred-times-string "{}" ";")
	     (start-process "rec-shred" "rec-shred" "find" fn "-depth" "-type" "d"
			  "-exec" "rmdir" "{}" ";")))
    (start-process "shred" "shred" "shred" "-zufn"
		 shred-times-string fn)))
  (when (eq major-mode 'dired-mode)
    (call-interactively 'revert-buffer)))

		 
  

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
		(if (or current-prefix-arg
			(eq major-mode 'fundamental-mode))
		    (read-symbol-completing "enter mode: ")
		  major-mode)))
  ;;first load mode to set the right comment-start
  (funcall mode)
  (let ((mode-sans-mode
	 (let ((mode-name (symbol-name mode)))
	   (string-match
				"\\(.*\\)-mode"
				mode-name)
	   (intern (match-string 1 mode-name)))))
    (add-file-local-variable 'mode mode-sans-mode)))

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
	finally (message "done checking buffers"))
  t)
(add-hook 'kill-emacs-query-functions 'check-unsaved-buffers)

(defun diff-sexps (sexp-a sexp-b)
  ;;TODO loop fill in missing length
  (loop for a in sexp-a
	for b in sexp-b
	do 
	(if (not (eq (atom a) (atom b)))
	    (error "mismatch: %s %s" a b)
	  (if (not (atom a))
	      (diff-sexps a b)
	    (unless (equal a b)
	      (error "mismatch: %s %s" a b)))))
  (or (not (and (consp sexp-a) (consp sexp-b)))
      (= (length sexp-a) (length sexp-b))))
  
	
	
(defun lookup-key-in-current-maps (key)
  "list of active keymaps that bind the given key"
  (interactive (list (read-key-sequence "enter key to lookup in current maps: ")))
  (let* ((kmaps-filtered (remove-if-not (lambda (kmap)
					   (lookup-key kmap key))
				       (current-active-maps)))
	(kmap-syms (keymap-symbol kmaps-filtered)))
    (message "%s" kmap-syms)
    kmap-syms))

(defun clean-up-async-shell-command-buffers ()
  (interactive)
  (loop with async-buffs = (remove-if-not
			    (lambda (buff)
			      (string-match "Async.Shell"
					    (buffer-name buff)))
			    (buffer-list))
	for buff in async-buffs do
	(progn (switch-to-buffer buff)
	       (when (get-buffer-process buff)
		 (message "command was: %s"
			  (process-command (get-buffer-process buff))))
	       (recursive-edit))))

(defun walk-dir-tree (top fun)
  (loop with front = (list top)
	with new-front = nil 
	while front do
	(loop while front
	      as dir = (pop front)
	      as files = (progn (assert (f-dir? dir))
				(directory-files dir))
	      do (loop for base in files
		       as fn = (f-join dir base)
		       do (if (f-dir? fn)
			      (unless (member base '(".." "."))
				(push fn new-front))
			    (unless (string-match "^#[.]" base)
			      (funcall fun fn)))))
	do (setf front new-front
		 new-front nil)))



(defun completing-read-single-char (prompt candidates)
  (let* ((alist (mapcar (lambda (cand)  (cons (substring cand 0 1) cand))
		       candidates))
	(prompt (format "%s\nenter char: " (s-join "\n" (mapcar 'prin1-to-string alist))))
	(chars (mapcar 'car alist))
	char)
    (loop as char = (read-char prompt)
	  as pair =  (assoc (and char (char-to-string char)) alist)
	  until pair
	  finally (return (cdr pair)))))

(defmacro with-temporary-open-file (fn &rest body)
  `(let ((was-open (get-file-buffer ,fn))
	 buff)
     (setf buff (find-file ,fn))
     (save-excursion
       ,@body)
     (assert (eq buff (current-buffer)))
     (unless was-open (kill-buffer buff))))

(defun replace-regexp-dir (dir extension from to &optional pause)
  ;;TODO colored output
  (interactive
   (let* ((ext (read-string
		"enter extension (eg 'js'): "
		(f-ext (or (buffer-file-name (current-buffer)) ""))))
	  (dir (if current-prefix-arg
		   (read-directory-name "enter directory: ")
		 default-directory)))))
  (let ((count 0))
  (walk-dir-tree dir
		 (lambda (fn)
		   (when (string= (f-ext fn) extension)
		     (with-temporary-open-file
		      fn
		      (regexp-replace-current-buffer from to pause)
		      (save-buffer)))))
  (message "%d occurrences replaced" count)))

(defun regexp-replace-current-buffer (from to &optional pause)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward from nil t)
	(incf count)
	(replace-match (if (functionp to)
			   (save-excursion (funcall to))
			 to) t)
	(when pause (y-or-n-p (format
			       "confirm: (%s --> %s)"
			       from to)))))
    count))

(defun regexp-exists-current-buffer (regexp)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp nil t)))


(defun keymap-symbol (keymaps)
  "Return the symbol to which KEYMAP is ound, or nil if no such symbol exists."
  (unless (consp keymaps) (setf keymaps (list keymaps)))
  (let (syms)
    (mapatoms (lambda (sym)
                (and (not (eq sym 'keymap))
                     (boundp sym)
                     (find (symbol-value sym) keymaps)
                     (push sym syms))))
    syms))
