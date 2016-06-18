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
	     `(nnimap "gmail"
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

(defun gnus-gen-dot-gnus (email smtp-server-port imap-server-port)
  (let ((form (gnus-imap-smtp-form email smtp-server-port imap-server-port))
	(dot-gnus (f-expand "~/.gnus")))
    (unless (or (not (file-exists-p dot-gnus))
		(when (y-or-n-p "~/.gnus file exists. overwrite?")
		  (funcall (if (fboundp 'shred-rec) 'shred-rec 'delete-file) dot-gnus)
		  t))
    (error "~/.gnus already exists"))
    (write-region (pp form) nil dot-gnus)
    (message "wrote to %s" dot-gnus)))


;(gnus-gen-dot-gnus "erjoalgo@gmail.com" '("smtp.gmail.com" . 587) '("imap.gmail.com" . 993))




;; Local Variables:
;; mode: emacs-lisp
;; End: