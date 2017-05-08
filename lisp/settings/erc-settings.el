(defadvice erc-select-read-args (around maybe-save-erc-auth activate)
  (let ((fn (expand-file-name "~/.ercrc")))
    (setf ad-return-value
	  (if (file-exists-p fn)
	      (progn
		(message "reading erc login from %s..." fn)
		(read (with-temp-buffer (insert-file-contents-literally fn)
					(buffer-string))))
	    (let ((erc-login ad-do-it))
	      (with-temp-file fn (insert (prin1-to-string erc-login)))
	      erc-login)))))
