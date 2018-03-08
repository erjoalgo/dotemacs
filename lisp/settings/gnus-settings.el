(setf gnus-attachments-top
      (expand-file-name
       "~/Downloads/gnus-attachments/"))

(defun gnus-init-filename ()
  "return a ~/.gnus filename based on the current emacs session"
  (let* ((emacs-self-pid (emacs-pid))
	 (all-emacs-pids (->> (shell-command-to-string "pidof emacs")
			      s-trim
			      (s-split " ")
			      (mapcar 'string-to-int)
			      reverse))
	 (gnus-init-idx (loop for i from 0
			      for pid in all-emacs-pids
			      when (eql pid emacs-self-pid)
			      return i))
	 (gnus-init-filename (format "~/.gnus%s"
				     (if (or (null gnus-init-idx)
					     (zerop gnus-init-idx)) ""
				       (format "-%d" gnus-init-idx))))
	 )
    (if (file-exists-p gnus-init-filename)
	gnus-init-filename
      (and (boundp 'gnus-init-file) gnus-init-file))))

(defun longest-common-prefix (cands)
  (when cands
    (loop with min-len = (apply 'min (mapcar 'length cands))
	  with i = 0
	  while (and (< i min-len)
		     (let ((char (aref (car cands) i)))
		       (every (lambda (s) (eql (aref s i) char))
			      cands)))
	  do (incf i)
	  finally (return (substring (or (car cands) "") 0 i)))))

(defun gnus-select-init-filename ()
  (interactive)
  (let* ((cands (remove-if-not (lambda (fn) (string-match "^[.]?gnus-?.*" fn))
			       (directory-files (expand-file-name "~"))))
	 (selection (cond
		     ((null cands) (error "no ~/.gnus* found"))
		     ((null (cdr cands)) (car cands))
		     (t (completing-read "select ~/.gnus init file: " cands
					 nil t (longest-common-prefix cands) nil (car cands)))))
	 (filename (f-join "~" selection)))
    (setf gnus-init-file filename)
    ))

(setf gnus-init-file
      (gnus-init-filename))

(unless (f-exists? gnus-attachments-top)
  (mkdir gnus-attachments-top t))

(setf mm-default-directory (expand-file-name "~/Downloads"))

(defvar inbox-group-name)
(defvar sent-group-name)

(defun gnus-goto-inbox ()
  (interactive)
  (require 'gnus)
  (let* ((inbox (or "INBOX"
		    (car (gnus-filter-groups
			  (lambda (name) (string-match "INBOX" name))))))
	 (inbox-buffer (format "*Summary %s*" inbox)))
    (if (get-buffer inbox-buffer)
	(switch-to-buffer inbox-buffer)
      (when (get-buffer "*Group*")
	(kill-buffer "*Group*"))
      (gnus-select-init-filename)
      (load gnus-init-file)
      (gnus)
      ;; (gnus-group-read-group 5000 t sent-group-name )
      (gnus-group-read-group 1000 t inbox )
      ;;(gnus-summary-sort-by-most-recent-date)
      )))


(defun gnus-filter-groups (pred)
  (let (groups)
    (mapatoms
     (lambda (group)
       (when
	   (and (symbol-name group)
		(funcall pred (symbol-name group))
		(symbol-value group))
	 (push (symbol-name group) groups)))
     gnus-active-hashtb)
    groups))


(defun gnus-goto-sent-emails ()
  (interactive)
  (let ((sent (or sent-group-name
		  (car (gnus-filter-groups
			(lambda (name) (string-match "Sent" name)))))))
    (gnus-group-read-group 200 t sent nil)))

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
		       ((kbd "M-c") 'message-send-and-exit)
		       ((kbd "s-A") 'gnus-insert-html-from-file))
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
  (gnus-demon-add-scanmail))

(defun setup-gnus-notifications ())
(with-eval-after-load "gnus-sum"
		      (gnus-load-bindings
		       gnus-my-goto-map
		       ("g" 'gmail-search-query)
		       ("t" 'gnus-goto-sent-emails)
		       ("r" 'gnus-summary-insert-new-articles)
		       ("f" 'gnus-summary-mail-forward))

		      (gnus-load-bindings
		       gnus-summary-mode-map
		       ("R" 'gnus-summary-wide-reply-with-original)
		       ("r" 'gnus-summary-reply-with-original)
		       ((kbd "s-g") 'gmail-search-query)
		       ((kbd "s-t") 'gnus-goto-sent-emails)
		       ((kbd "s-r")
			'gnus-summary-insert-new-articles)
		       ("g" gnus-my-goto-map))
		      (setup-gnus-notifications))

(with-eval-after-load "gnus-art"
		      (gnus-load-bindings
		       gnus-article-mode-map
		       ("F" 'gnus-summary-mail-forward)
		       ("R" 'gnus-article-wide-reply-with-original)
		       ((kbd "s-s") 'gnus-mime-save-all-attachments)))



(defun gnus-attach-file-simple (file)
  (interactive "fenter file to attach: ")
  (let* (
	 ;;(type (mml-minibuffer-read-type file))
	 (type (mm-default-file-encoding file))
	 (description nil )
	 (disposition "attachment"))
    ;;(mml-attach-file FILE &optional TYPE DESCRIPTION DISPOSITION)
    (mml-attach-file file type description disposition)))

(defun gmail-search-query (arg)
  (interactive "P")
  (with-current-buffer "*Group*"
    (save-excursion (goto-char (point-min))
		    (re-search-forward
		     (format "^[ 	]+\\([0-9]+\\|[*]\\):.*%s$"
			     (regexp-quote (if arg sent-group-name "INBOX"))))
		    (call-interactively 'gnus-group-make-nnir-group))))

(defvar gnus-mime-save-all-attachments-prompt-mkdir-p nil)

(defun gnus-dir-name-for-message ()
  (let* ((from (message-fetch-field "From"))
	 (date (message-fetch-field "Date"))
	 (from-nw (gnus-replace-in-string from ".*<\\(.*\\)>.*" "\\1"))
	 (date-nw (-> date
		      (gnus-replace-in-string "^[A-Z][a-z]\\{2\\}, \\([0-9]+ [A-Z][a-z]+ [0-9]\\{4\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [-+][0-9]\\{4\\} ([A-Z]\\{3\\})\\).*" "\\1")
		      s-trim
		      (gnus-replace-in-string "[ :]" "-"))))
    (f-join gnus-attachments-top from-nw date-nw)))


(defun gnus-mime-save-all-attachments (dir)
  ;;(interactive "GEnter destination directory to save attachments: " )
  (interactive (list (gnus-dir-name-for-message) ))
  (unless (file-exists-p dir)
    (unless (or (not gnus-mime-save-all-attachments-prompt-mkdir-p)
		 (y-or-n-p (format "making directory %s" dir)))
      (error "failed to confirm"))
    (mkdir dir t))
  (gnus-summary-save-parts ".*" dir nil )
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
      (when (boundp 'browser-new-tab)
	(browser-new-tab gmail-app-specific-url))

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

(setq gnutls-min-prime-bits 2048)
;; https://www.emacswiki.org/emacs/GnusSpeed#toc3
(setq gc-cons-threshold 3500000)
(setq gnus-use-correct-string-widths nil)

(setf gnus-activate-level 1)
(defun gnus-prioritize-inbox ()
  (dolist (group '("INBOX" "[Gmail]/Sent Mail"))
    (gnus-group-change-level
     group (or (when (fboundp 'gnus-group-group-level)
		 (gnus-group-group-level))
	       gnus-level-killed))
    (gnus-group-change-level group 1)))

(with-eval-after-load "gnus-start"
  (condition-case ex (gnus-prioritize-inbox)
    (error (warn "(gnus-prioritize-inbox) failed: %s" ex))))

(defun gnus-insert-html-from-file (filename)
  (interactive "fenter html filename: ")
  (mml-insert-tag 'part
		  'type "text/html"
		  'disposition "inline")
  (destructuring-bind (_ len)
      (insert-file-contents filename nil nil nil)
    (goto-char (+ (point) len))
    (mml-insert-tag '/part)))

(defvar gnus-email-templates-home
  (expand-file-name "~/.gnus-templates"))

(defun gnus-email-templates-toaddress-to-hostname (toaddr)
  (cadr (split-string toaddr "@")))

(defun gnus-email-templates-hostname-to-filename (hostname)
  (f-join gnus-email-templates-home hostname))

(defun gnus-email-templates-filename-to-hostname (filename)
  (->> filename f-name))

(defun message-mode-maybe-insert-email-template ()
  (let ((to-addr (message-fetch-field "To")))
    (when to-addr
      (let ((filename
	     (-> to-addr
		 gnus-email-templates-toaddress-to-hostname
		 gnus-email-templates-hostname-to-filename)))
	(when (file-exists-p filename)
	  ;; TODO how to insert this before the body?
	  (message "inserting template from %s" filename)
	  (end-of-buffer)
	  (insert-file (expand-file-name filename)))))))

(defun gnus-templates-add-template (hostname &optional text)
  (interactive
   (list (read-string "enter 'To' field hostname: "
		     (when (x-get-selection)
		       (let ((toaddr (x-get-selection)))
			 (gnus-email-templates-toaddress-to-hostname toaddr))))))
  (find-file (gnus-email-templates-hostname-to-filename hostname))
  (when text (insert text)))


(add-hook 'message-signature-setup-hook
	  'message-mode-maybe-insert-email-template)


(defun gnus-save-all-mail-from-to (addresses &optional summary-buffer)
  (let* ((summary-buffer (or summary-buffer "*Summary INBOX*"))
	 (summary-name (gnus-replace-in-string
			(buffer-name (get-buffer summary-buffer))
			"[*]Summary \\(.*\\)[*]" "\\1"))
	 (article-buffer (format "*Article %s*" summary-name))
	 (gnus-prompt-before-saving nil)
	 )
    (loop
     do (progn
	  (switch-to-buffer summary-buffer)
	  (gnus-summary-prev-article)
	  (switch-to-buffer article-buffer)
	  (let ((art-addresses
		 (concat (message-fetch-field "From")
			 (message-fetch-field "Cc")
			 (message-fetch-field "CC")
			 (message-fetch-field "Bcc")
			 (message-fetch-field "BCC")
			 (message-fetch-field "To"))))
	    (when (zerop (length art-addresses))
	      (error "unable to fetch message fields"))
	    (when (loop for addr in addresses thereis
			(s-contains-p addr art-addresses))
	      (message "saving  %s" (message-fetch-field "Subject")
		       (gnus-mime-save-all-attachments (gnus-dir-name-for-message))
		       (gnus-summary-save-article-mail))))))))

(defun gnus-insert-base64-image-src (filename)
  (interactive "fenter image: ")
  (let ((ext (f-ext filename))
	(base64 (shell-command-to-string (format "base64 %s | tr -d '\\n'" filename))))
    ;; "data:image/jpg;base64,/*base64-data-string here*/"
    (insert (format "\"data:image/%s;base64,%s\"" ext base64))))

'(require erjoalgo-indent-mode)
