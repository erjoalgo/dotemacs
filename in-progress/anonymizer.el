(defun anonymize ()
  (let ((words (list `((,system-name "my-hostname")
		     (,user-login-name "my-login" )
		     (,user-real-login-name "my-login")
		     (,user-mail-address "me@example.com")
		     ((,first user-full-name "my-first-name"))
		     ((,second user-full-name "my-last-name"))
		     ))
	(regexps (("[[:alnum:]]+@[[:alnum:]]+[.][[:alnum:]]+" "user@example.com")
		  ("@[[:alnum:]]+[.][[:alnum:]]+" "@example.com")))))
    
	
		       
