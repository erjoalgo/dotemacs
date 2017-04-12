;;link to this file from ~/.gnus

(defun gnus-imap-smtp-form (email smtp-server-port imap-server-port)
  (destructuring-bind ((smtp-server . smtp-port) . (imap-server . imap-port))
      (cons smtp-server-port imap-server-port)
    `(let ((email ,email)
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


(cl-defun gnus-gen-dot-gnus (email &key smtp imap dot-gnus)
  (let ((form (gnus-imap-smtp-form email smtp imap))
	(dot-gnus (f-expand (or dot-gnus "~/.gnus"))))
    (unless (or (not (file-exists-p dot-gnus))
		(when (y-or-n-p (format "%s file exists. overwrite?" dot-gnus))
		  (funcall (if (fboundp 'shred-rec) 'shred-rec 'delete-file) dot-gnus)
		  t))
      (error "~/.gnus already exists"))
    (write-region (pp form) nil dot-gnus)
    (message "wrote to %s" dot-gnus)))

'(gnus-gen-dot-gnus "erjoalgo@gmail.com"
		    :smtp '("smtp.gmail.com" . 587)
		   :imap '("imap.gmail.com" . 993)
		   :dot-gnus "~/.gnus-1")



;; Local Variables:
;; mode: emacs-lisp
;; End:
