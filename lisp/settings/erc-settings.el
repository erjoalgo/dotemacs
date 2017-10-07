(cl-defun erc-login-to-string (&key server password nick port)
  "this should be an anonymous function"
  (format "%s:%s (%s:%s)" server port nick nil))

(defun erc-select-server-login (&optional fn)
  (interactive)
  (setf fn (or fn (expand-file-name "~/.ercrc")))
  (when (file-exists-p fn)
    (message "reading erc login from %s..." fn)
    (let* ((logins (read (with-temp-buffer (insert-file-contents-literally fn)
					   (buffer-string))))
	   (selected (if (not (consp (car logins)))
			 logins
		       ;; we have a list of servers
		       (let* ((choices-alist
			       (loop for login in logins collect
				     (cons (apply 'erc-login-to-string login) login)))
			      (choice (completing-read "select an irc server: "
						       (mapcar 'car choices-alist) nil t)))
			 (cdr (assoc choice choices-alist))))))
      selected)))

(defadvice erc-select-read-args (around maybe-save-erc-auth activate)
  (let ((fn (expand-file-name "~/.ercrc")))
    (setf ad-return-value
	  (or (erc-select-server-login)
	      (let ((erc-login ad-do-it))
		;; TODO properly write as a list and append to existing entries
		;; if any
		(with-temp-file fn (insert (prin1-to-string erc-login)))
		erc-login)))))

(setq erc-autojoin-channels-alist
      '(("freenode.net"
	 "#emacs"
	 "#stumpwm")))

(defvar erc-select-server-login-example-list
  '(
   (:server "irc.freenode.net" :port 6667 :nick "ealfonso" :password "ooizew")
   (:server "user.com" :port 6667 :nick "" :password ""))
  "examples of ~/.erc contents, plus a quote")
