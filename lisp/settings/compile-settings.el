(defun erjoalgo-compile-compile (arg)
  (interactive "P")
  (when arg
    (call-interactively 'erjoalgo-compile-prompt-and-set-command))
  (let ((cmd-list
	 (or (erjoalgo-compile-read-cmd-list)
	     (erjoalgo-compile-cmd-for-buffer (current-buffer)))))
    (when (functionp cmd-list)
      (setf cmd-list (list cmd-list)))
    (if (or arg (not cmd-list))
	(recompile)
      (loop for cmd in
	    ;(if (atom cmd-list) (list cmd-list) cmd-list)
	    cmd-list
	    do
	    (cond
	     ((stringp cmd) (compile cmd))
	     ((functionp cmd) (funcall cmd (current-buffer)))
	     (t (error "cmd must be function or string, not %s" cmd)))))))

(defun erjoalgo-compile-read-cmd-list ()
  ;;(read-file-local-variable-value 'compile-command)
  ;;TODO read file local compile-command
  )

(defun erjoalgo-compile-cmd-for-buffer (buffer)
  (loop for matcher in erjoalgo-compile-cmd-for-buffer
	thereis (funcall matcher buffer)))

(defvar erjoalgo-compile-cmd-for-buffer ()
  "list of functions, each should return
the command for compiling a particular buffer,
or nil if unknown")

(defun erjoalgo-compile-prompt-and-set-command (cmd)
  (interactive
   (list (read-shell-command "enter compile command: " compile-command)))
  (add-file-local-variable 'compile-command compile-command)
  (setf compile-command cmd)
  ;(compile compile-command)
  )

(defun wrap-ignore (fun)
  (lexical-let ((fun fun))
    (lambda (&rest args) (funcall fun))))

(setf erjoalgo-compile-cmd-for-buffer
      ;;TODO make this less verbose
      (list
       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'fundamental-mode))
	   ;;git commit
	   (wrap-ignore 'server-edit)))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'sh-mode))
	   (format "bash %s" (f-filename (buffer-file-name)))))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'java-mode))
	   (let ((f-no-ext
		  (-> (buffer-file-name) (f-filename) (f-no-ext))))
	     (format "javac %s.java && java %s" f-no-ext f-no-ext))))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'c-mode))
	   (let ((fn (f-filename (buffer-file-name))))
	     (format "gcc -g %s && ./a.out" fn fn))))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'go-mode))
	   "go test"))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'tex-mode))
	   latex-compile))

       (lambda (buff)
	 (when (-> (buffer-local-value 'major-mode buff)
		   (eq 'python-mode))
	   (format "python %s" (f-filename (buffer-file-name)))))
))



(global-set-key (kbd "M-c") 'erjoalgo-compile-compile)
(global-set-key (kbd "M-C") (lambda () (interactive)
			      (erjoalgo-compile-compile '(4))))

(setf compilation-ask-about-save nil)
;;(setf compilation-read-command nil)

(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)


;taken from
;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)

(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

(add-to-list 'compilation-finish-functions 'cc-goto-first-error)

(defun compilation-finished-notify (buff finish-description)
  (call-process
   "notify-send" nil 0 nil
   (format "compilation: %s" finish-description))
  (let ((current-hour
	 (third (decode-time (current-time)))))
    (unless (or (> current-hour 23)
	      (< current-hour 9))
      ;;this theme is nice. text easy to read, dark background
      ;;only load at night?
      '(beeper-beep)
      )))

(add-to-list 'compilation-finish-functions
	     'compilation-finished-notify)

;;TODO autoload recompile
(require 'compile)
