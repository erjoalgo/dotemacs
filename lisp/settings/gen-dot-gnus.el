;;link to this file from ~/.gnus

(defun gnus-imap-smtp-form (email smtp-server-port imap-server-port)
  (cl-destructuring-bind ((smtp-server . smtp-port) . (imap-server . imap-port))
      (cons smtp-server-port imap-server-port)
    `(let ((email ,email)
           (hostname ,(replace-regexp-in-string "^.*?@" "" email))
	   (smtp-server ,smtp-server)
	   (smtp-port ,smtp-port)
	   (imap-server ,imap-server)
	   (imap-port ,imap-port))
       (setq gnus-select-method
	     `(nnimap ,hostname
		      (nnimap-address ,imap-server)
		      (nnimap-server-port ,imap-port)
		      (nnimap-stream ssl)))
       ;; avoid one big setq to skip formatting issues
       (setq message-send-mail-function 'smtpmail-send-it)
       (setq smtpmail-starttls-credentials `((,smtp-server ,smtp-port nil nil)))
       (setq smtpmail-auth-credentials `((,smtp-server ,smtp-port
						       ,email nil)))
       (setq smtpmail-default-smtp-server smtp-server)
       (setq smtpmail-smtp-server smtp-server)
       (setq smtpmail-smtp-service smtp-port)
       (setq gnus-ignored-newsgroups
	     "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
       (setq user-mail-address email)
       (setq smtpmail-stream-type 'starttls))))


(cl-defun gnus-gen-dot-gnus (email &key smtp imap dot-gnus
				   inbox-group-name sent-group-name
				   (if-file-exists prompt))
  (let* ((form (gnus-imap-smtp-form email smtp imap))
	 (dot-gnus (f-expand (or dot-gnus "~/.gnus"))))
    (when (or (not (file-exists-p dot-gnus))
	      (when
		  (cl-case if-file-exists
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
        (emacs-lisp-mode)
	(add-file-local-variable 'mode 'emacs-lisp))
      (message "wrote to %s" dot-gnus)
      dot-gnus)))

(defun gen-dot-gnus-gmail-account (gmail-address)
  (interactive "senter gmail address: ")
  (gnus-gen-dot-gnus gmail-address
		     :smtp '("smtp.gmail.com" . 587)
		     :imap '("imap.gmail.com" . "imaps")
		     :dot-gnus (format "~/.gnus-%s" gmail-address)
		     :inbox-group-name "INBOX"
		     :sent-group-name "[Gmail]/Sent Mail"
		     :if-file-exists 'overwrite))

;; Local Variables:
;; mode: emacs-lisp
;; End:
