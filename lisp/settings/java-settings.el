
(defun java-set-indentation ()
					;(setf tab-width 8)
  (setf tab-width 2)
  (setf c-basic-offset 2)
					;(setf indent-tabs-mode t)
  (setf indent-tabs-mode nil))

(add-hook 'java-mode-hook 'java-set-indentation)

(defun buffer-indentations ()
  (save-excursion
    (goto-char (point-min))
    (loop as
	  match = (search-forward-regexp "^[ \t]+" nil t nil)
	  while match
	  collect (match-string 0)
	  do (goto-char (match-end 0)))))

(defun buffer-spaces-or-tabs-p ()
  (interactive)
  (let (
	(tab (string-to-char ""))
	(space (string-to-char " "))
	(indentations (buffer-indentations))
	(indentation-type
	 (lambda (whitespace)
	   (if (>= (count tab whitespace)
		   (/ (count space whitespace) 2.0))
	       'TAB 'SPACE))))

    (let ((tabs-vs-spaces (mapcar indentation-type indentations)))
      (if (> (count 'TAB tabs-vs-spaces)
	     (count 'SPACE tabs-vs-spaces))
	  'TABS 'SPACES))))

(defun buffer-indentation-fix-tabs-spaces ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\t" "")
    (indent-region (point-min) (point-max))))

(defun mvn-offline-p-toggle (&optional offline)
  (interactive (list (not (and (boundp 'mvn-offline-p) mvn-offline-p))))
  (setf mvn-offline-p offline)
  (message (if offline "offline" "online")))

(add-hook 'java-mode-hook 'java-imports-scan-file)

(defun java-sync-function-file-names ()
  ;; adapted from octave.el: octave-sync-function-file-names
  ;; TODO make generic
  "Ensure function name agree with function file name.
See Info node `(octave)Function Files'."
  (interactive)
  (when buffer-file-name
    (let* ((java-public-class-regexp
	    "class[ \t\n]+\\([^ \t\n]+\\)[ \t\n]*{")
	   (java-class-name
	    (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward
		   java-public-class-regexp nil)
		  (match-string 1)
		(prog1 nil (warn "no class name found"))))))
      (when java-class-name
        (let* ((func java-class-name)
               (file (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name)))
               (help-form (format "\
a: Use java class name `%s'
b: Use file name `%s'
q: Don't fix\n" func file))
               (c (unless (equal file func)
                    (save-window-excursion
                      (help-form-show)
                      (read-char-choice
                       "Which name to use? (a/b/q) " '(?a ?b ?q))))))
          (pcase c
            (`?a (let ((newname (expand-file-name
                                 (concat func (file-name-extension
                                               buffer-file-name t)))))
                   (when (or (not (file-exists-p newname))
                             (yes-or-no-p
                              (format "Target file %s exists; proceed? " newname)))
                     (when (file-exists-p buffer-file-name)
                       (rename-file buffer-file-name newname t))
                     (set-visited-file-name newname))))
            (`?b (save-excursion
		   (goto-char (point-min))
		   (re-search-forward
		    java-public-class-regexp nil)
		   (replace-match file t t nil 1)))))))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'java-sync-function-file-names nil t)))
