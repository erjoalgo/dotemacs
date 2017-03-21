
(add-hook 'java-mode-hook (lambda ()
			    ;(setf tab-width 8)
			     (setf tab-width 2)
			     (setf c-basic-offset 2)
					;(setf indent-tabs-mode t)
			     (setf indent-tabs-mode nil)))

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

(add-hook 'java-mode-hook 'java-imports-scan-file)
(define-key java-mode-map (kbd "s-m") 'java-imports-add-import-dwim)
