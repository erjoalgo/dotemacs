(setf gnus-attachments-default
      (expand-file-name
       "~/Downloads/gnus-attachments/"))

(unless (f-exists? gnus-attachments-default)
  (mkdir gnus-attachments-default))

(defvar sent-group-name "[Gmail]/Sent Mail")
(defun gnus-goto-inbox ()
  (interactive)
  (require 'gnus)
  (let ((inbox "*Summary INBOX*"))
    (or
     (and (get-buffer inbox)
	  (switch-to-buffer inbox))
     (gnus)
     (gnus-group-read-group 1000 t "INBOX" )
     ;;(gnus-summary-sort-by-most-recent-date)
     (gnus-group-read-group 5000 t sent-group-name ))))


(defun gnus-goto-sent-emails ()
  (interactive)
  (let (groups)
    (mapatoms
     (lambda (group)
       (and (symbol-name group)
	    (string-match "Sent" (symbol-name group))
	    (symbol-value group)
	    (push (symbol-name group) groups)))
     gnus-active-hashtb)
    (gnus-group-read-group 200 t sent-group-name nil )))

(defun gnus-quit ()
  (interactive)
  ;;(gnus-summary-quit t)
  ;;(gnus-group-quit)
  (gnus-group-exit)
  (gnus-goto-inbox))


(defun ispell-mail ()
  ;;why not just (ispell-buffer) or (ispell)?
  (ispell-region
   (point-min)
   (point-max) t))

(defvar gnus-my-goto-map
  (make-sparse-keymap))

(setf gnus-always-read-dribble-file t)

(defmacro gnus-load-bindings (kmap &rest bindings)
  `(progn ,@(loop for (key cmd) in bindings collect
		  `(define-key ,kmap ,key ,cmd))))

(with-eval-after-load "message"
  (gnus-load-bindings
   message-mode-map
   ((kbd "\C-ci") 'gmail-contacts-insert-contact)
   ("" nil)
   ("" nil)
   ("a" nil )
   ("M" nil)
   ((kbd "s-a") 'gnus-attach-file-simple)
   ((kbd "M-c") 'message-send-and-exit))
  (add-hook 'message-mode-hook
	    (lambda ()
	      (erjoalgo-indent-mode 1)
	      (indent-mode-set-string ">"))))

(defun setup-gnus-notifications ()
  (require 'gnus-desktop-notify)
  (setq gnus-desktop-notify-function 'gnus-desktop-notify-exec
					;gnus-desktop-notify-exec-program )
	gnus-desktop-notify-exec-program
	(case system-type
	  ((gnu/linux) "notify-send")
	  ((darwin) "growlnotify -a Emacs.app -m")))
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail)
  )

(with-eval-after-load "gnus-sum"
  (gnus-load-bindings gnus-my-goto-map
		      ("g" 'gmail-search-query)
		      ("t" 'gnus-goto-sent-emails)
		      ("r" 'gnus-summary-insert-new-articles)
		      ("f" 'gnus-summary-mail-forward))

  (gnus-load-bindings gnus-summary-mode-map
		      ("R" 'gnus-summary-wide-reply-with-original)
		      ("r" 'gnus-summary-reply-with-original)
		      ((kbd "s-g") 'gmail-search-query)
		      ((kbd "s-t") 'gnus-goto-sent-emails)
		      ((kbd "s-r")
		       'gnus-summary-insert-new-articles)
		      ("g" gnus-my-goto-map))
    (setup-gnus-notifications))

(with-eval-after-load "gnus-art"
  (gnus-load-bindings gnus-article-mode-map
		      ("F" 'gnus-summary-mail-forward)
		      ("R" 'gnus-article-wide-reply-with-original)
		      ((kbd "s-s") 'gnus-mime-save-all-attachmnets)))



(defun gnus-attach-file-simple (file)
  (interactive "fenter file to attach: ")
  (let* (
	 ;;(type (mml-minibuffer-read-type file))
	 (type (mm-default-file-encoding file))
	 (description nil )
	 (disposition "attachment"))
    ;;(mml-attach-file FILE &optional TYPE DESCRIPTION DISPOSITION)
    (mml-attach-file file type description disposition)))

(defun gmail-search-query (query)
  (interactive "sEnter search query to send to gmail: ")
  (let* (
	 (group-spec '(("nnimap:gmail" ("INBOX"))))
	 (query-spec `((query . ,query)))
	 )
    (gnus-group-make-nnir-group
     nil
     (cons 'nnir-specs (list (cons 'nnir-query-spec query-spec)
			     (cons 'nnir-group-spec group-spec))))))

(defun gnus-mime-save-all-attachmnets (dir)
  ;;(interactive "GEnter destination directory to save attachments: " )
  (interactive (list (read-directory-name "Enter destination directory to save attachments: " gnus-attachments-default ) ))
  (unless (file-exists-p dir)
    (or (y-or-n-p (format "making directory %s" dir) ) (error "failed to confirm "))
    (mkdir dir t))
  (gnus-summary-save-parts ".*/.*" dir nil )
  (find-file dir))

(defadvice gnus-group-select-group
    ;;there must be a better way
    (around select-unread-email-advice activate)
  '(ad-set-arg 0 t)
  ad-do-it
  (gnus-summary-sort-by-most-recent-date))

(defadvice gnus-summary-insert-new-articles
    ;;there must be a better way
    (after sort-by-date-after-refresh activate)
  (gnus-summary-sort-by-most-recent-date))

(defadvice gnus-group-read-group
    ;;there must be a better way!
    (after sort-by-date-after-nnir activate)
  (gnus-summary-sort-by-most-recent-date))

(defadvice smtpmail-send-it (around fix-using-openssl activate)
  ;;TODO where did this come from, is this secure
  (let ((tls-program
	 (or (when (eq smtpmail-stream-type 'ssl)
	       '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -starttls smtp"))
	     tls-program)))
    ad-do-it))

(defvar gmail-app-specific-url
  "https://security.google.com/settings/security/apppasswords?pli=1")

(defun shell-command-to-string-message (cmd)
  (message "result of %s: %s" cmd (shell-command-to-string cmd)))

(defun gnus-erjoalgo-setup ()
  (interactive)
  (let ((gnus-fn (expand-file-name "~/.gnus"))
	(authinfo-fn (expand-file-name "~/.authinfo"))
	(truename-exists-p (lambda (fn)
			     (and (file-exists-p fn)
				  (file-exists-p (file-truename fn)))))
	(maybe-unlink (lambda (fn)
			(and (file-symlink-p fn)
			     (shell-command-to-string-message
			      (format "unlink %s" fn))))))

    (unless (funcall truename-exists-p gnus-fn)
      (funcall maybe-unlink gnus-fn)
      (shell-command-to-string-message
       (format "ln -s %s %s"
	       (f-join emacs-top "settings" ".gnus-erjoalgo")
	       gnus-fn)))

    (unless (funcall truename-exists-p authinfo-fn)
      (funcall maybe-unlink authinfo-fn)
      (when (boundp 'firefox-new-tab)
	(firefox-new-tab gmail-app-specific-url))

      (let ((pass (read-string
		   (format "enter gmail app-specific pass (%s): "
			   gmail-app-specific-url)))
	    (email "erjoalgo@gmail.com"))
	(append-to-file
	 (format
	  "machine imap.gmail.com login %s password %s port 993
machine smtp.gmail.com login %s password %s port 587"
	  email pass
	  email pass) nil
	  authinfo-fn)))))

'(require erjoalgo-indent-mode)
