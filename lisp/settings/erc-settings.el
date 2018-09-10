(cl-defun erc-login-to-string (&key server password nick port)
  "this should be an anonymous function"
  (format "%s:%s (%s:%s)" server port nick "HIDDEN"))

(defvar erc-logins-filename nil
  "filename where to store erc login information")

(defun erc-select-server-login (&optional fn)
  (interactive)
  (setf fn (or fn erc-logins-filename (expand-file-name "~/.ercrc")))
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
	 "#stumpwm"
         "#lisp")
	("erjoalgo.com"
	 "#kiwiirc-default")))

(defun erc-my-message-notify (match-type nickuserhost msg)
  ;; dbus rarely works reliably
  (destructuring-bind (nick user host) (erc-parse-user nickuserhost)
    (message "%s says: %s" nick (s-trim msg))))

;; TODO /msg nickserv ghost USER PASSWORD

'(with-eval-after-load 'erc
  (push 'notifications erc-modules)
  (push 'match erc-modules)
  (push 'pal erc-beep-match-types)
  ;; (push 'pal erc-notification-match-types)
  ;; (push 'erc-beep-on-match erc-text-matched-hook)
  (push 'erc-my-message-notify erc-text-matched-hook)
  (erc-update-modules))

(defun erc-ghost-maybe (server nick)
  ;; taken from https://www.emacswiki.org/emacs/ErcTips
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
    (let ((nick-orig (match-string 1 nick))
          (password erc-session-password))
      (message "change attempt count: %s %s %s"
               erc-nick-change-attempt-count
               erc-bad-nick
               nick)
      (when (y-or-n-p (format "Current nick is '%s'. Do you want to ghost?"
                              nick))
        (message "orig pass is %s" password)
        (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
				       nick-orig password))
	(erc-cmd-NICK nick-orig)
	(erc-message "PRIVMSG" (format "NickServ identify %s %s"
				       nick-orig password))))))

(add-hook 'erc-after-connect 'erc-ghost-maybe)
