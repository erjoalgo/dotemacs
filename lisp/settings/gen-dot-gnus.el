;;link to this file from ~/.gnus

(defun gnus-imap-smtp-form (email smtp-server-port imap-server-port)
  (destructuring-bind ((smtp-server . smtp-port) . (imap-server . imap-port))
      (cons smtp-server-port imap-server-port)
    `(let ((email ,email)
           (hostname ,(replace-regexp-in-string "^.*?@" "" email))
	   (smtp-server ,smtp-server)
	   (smtp-port ,smtp-port)
	   (imap-server ,imap-server)
	   (imap-port ,imap-port))
       (setf gnus-select-method
	     `(nnimap ,email
		      (nnimap-address ,imap-server)
		      (nnimap-server-port ,imap-port)
		      (nnimap-stream ssl)))

       (setf message-send-mail-function 'smtpmail-send-it
	     smtpmail-starttls-credentials `((,smtp-server ,smtp-port nil nil))
	     smtpmail-auth-credentials `((,smtp-server ,smtp-port
						       ,email nil))
	     smtpmail-default-smtp-server smtp-server
	     smtpmail-smtp-server smtp-server
	     smtpmail-smtp-service smtp-port
	     gnus-ignored-newsgroups
	     "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

       (setf user-mail-address email)
       (setf smtpmail-stream-type 'starttls))))


(cl-defun gnus-gen-dot-gnus (email &key smtp imap dot-gnus
				   inbox-group-name sent-group-name
				   (if-file-exists prompt))
  (let* ((form (gnus-imap-smtp-form email smtp imap))
	 (dot-gnus (f-expand (or dot-gnus "~/.gnus"))))
    (when (or (not (file-exists-p dot-gnus))
	      (when
		  (case if-file-exists
		    (overwrite t)
		    (ignore nil)
		    (prompt (y-or-n-p (format "%s file exists. overwrite?" dot-gnus))))
		(progn
		  (funcall (if (fboundp 'shred-rec) 'shred-rec 'delete-file) dot-gnus)
		  t)))
      (with-temp-file dot-gnus
	(insert (pp form))
	(insert (pp `(setf inbox-group-name ,inbox-group-name)))
	(insert (pp `(setf sent-group-name ,sent-group-name)))
	(add-file-local-variable 'mode 'emacs-lisp))
      (message "wrote to %s" dot-gnus))))

(gnus-gen-dot-gnus "gmail.com"
		    :smtp '("smtp.gmail.com" . 587)
		   :imap '("imap.gmail.com" . 993)
		   :dot-gnus "~/.gnus-gmail"
		   :inbox-group-name "INBOX"
		   :sent-group-name "[Gmail]/Sent Mail"
		   :if-file-exists 'ignore)

'(gnus-gen-dot-gnus "ealfonso@alumni.cmu.edu"
		   :smtp '("smtp.gmail.com" . 587)
		   :imap '("imap.gmail.com" . 993)
		   :dot-gnus "~/.gnus-cmu"
		   :inbox-group-name "INBOX"
		   :sent-group-name "[Gmail]/Sent Mail")





;; Local Variables:
;; mode: emacs-lisp
;; End:
