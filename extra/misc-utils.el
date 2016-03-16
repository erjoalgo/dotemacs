(defun whereis (program)
  (remove-if-not
   'identity
   (loop for dir in (split-string (getenv "PATH") ":" t)
	 if (file-exists-p dir)
	 collect (loop for fn in (directory-files dir) thereis
		       (and (string= program fn)
			    (f-join dir fn))))))
