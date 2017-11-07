(cl-defun erc-login-to-string (&key server password nick port)
  "this should be an anonymous function"
  (format "%s:%s (%s:%s)" server port nick "HIDDEN"))

(defun erc-select-server-login (&optional fn)
  (interactive)
  (setf fn (or fn (expand-file-name "~/.ercrc")))
  (when (file-exists-p fn)
    (message "reading erc login from %s..." fn)
    (let* ((logins (with-temp-buffer
		     (insert-file-contents-literally fn)
		     (mapcar 'read (split-string (buffer-string) "\n" t))))
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
	 "#stumpwm")
	("erjoalgo.com"
	 "#kiwiirc-default")))

(defun erc-my-message-notify (match-type nickuserhost msg)
  ;; dbus rarely works reliably
  (destructuring-bind (nick user host) (erc-parse-user nickuserhost)
    (message "%s says: %s" nick (s-trim msg))))

'(with-eval-after-load 'erc
  (push 'notifications erc-modules)
  (push 'match erc-modules)
  (push 'pal erc-beep-match-types)
  ;; (push 'pal erc-notification-match-types)
  ;; (push 'erc-beep-on-match erc-text-matched-hook)
  (push 'erc-my-message-notify erc-text-matched-hook)
  (erc-update-modules))
