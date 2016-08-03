(defun erjoalgo-compile-compile (arg)
  (interactive "P")
  (if (and arg compile-command) (recompile)
    (let* ((cmd-list
	    (or
	     (and compile-command-set compile-command)
		(erjoalgo-compile-read-file-local-cmd-list);;file-local
		(erjoalgo-compile-cmd-for-buffer (current-buffer));;matcher
		(call-interactively 'erjoalgo-compile-ask '(4)));;ask user and save
	    )

	   (cmd-list (if (or (functionp cmd-list)
			     (atom cmd-list))
			 (list cmd-list)
		       cmd-list)))
      (loop for cmd in cmd-list do
	    (cond
	     ((stringp cmd) (let ((compile-command cmd))
			      (compile cmd)))
	     ((functionp cmd) (funcall cmd (current-buffer)))
	     ((null cmd) (error "no compile command found for this buffer"))
	     (t (error "cmd must be function or string, not %s" cmd)))))))

(defvar-local compile-command-set nil)

(defun erjoalgo-compile-read-file-local-cmd-list ()
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

(defun erjoalgo-compile-ask (cmd save-file-local)
  (interactive
   (list (read-shell-command "enter compile command: " compile-command)
	 current-prefix-arg))
  (when save-file-local
    (add-file-local-variable 'compile-command compile-command))
  (setf compile-command cmd)
  (setf compile-command-set t)
  ;(compile compile-command)
  )

(defun wrap-ignore-args (fun)
  (lexical-let ((fun fun))
    (lambda (&rest args) (funcall fun))))

(defmacro buffer-major-mode-matcher (modes &rest forms)
  `(lambda (buffer)
     (when (member
	    (buffer-local-value 'major-mode buffer)
	    ',(if (atom modes) (list modes) modes))
       (with-current-buffer buffer ,@forms))))

(defun walk-up-directories (dir)
  (loop with dir = default-directory
	while dir
	collect dir
	do (setf dir (f-dirname dir))))

(setf
 erjoalgo-compile-cmd-for-buffer
 (list
  (buffer-major-mode-matcher
   fundamental-mode
   ;;git commit
   (wrap-ignore-args 'server-edit))

  (buffer-major-mode-matcher
   sh-mode
   (format "bash %s" (f-filename (buffer-file-name))))

  (buffer-major-mode-matcher
   java-mode
   (let ((f-no-ext
	  (-> (buffer-file-name) (f-filename) (f-no-ext)))
	 (pom-directory (loop for dir in (walk-up-directories
					  default-directory)
			      thereis (and
				       (member "pom.xml"
					       (directory-files dir))
				       dir))))
     (if pom-directory
	 (format "cd %s && mvn clean install" pom-directory)
       (format "javac %s.java && java %s" f-no-ext f-no-ext))))

  (buffer-major-mode-matcher
   c-mode
   (let ((fn (f-filename (buffer-file-name))))
     (format "gcc -g %s && ./a.out" fn fn)))

  (buffer-major-mode-matcher
   go-mode
   "go test")

  (buffer-major-mode-matcher tex-mode 'latex-compile)

  (buffer-major-mode-matcher
   python-mode
   (format "python %s" (f-filename (buffer-file-name))))

  (buffer-major-mode-matcher
   (git-rebase-mode text-mode)
   (lambda (buf) (call-interactively 'with-editor-finish)))

  (buffer-major-mode-matcher
   diff-mode
   (call-interactively 'server-edit))

  (buffer-major-mode-matcher clojure-mode 'cider-load-buffer)

  (buffer-major-mode-matcher message-mode-map 'message-send-and-exit)
  (buffer-major-mode-matcher org-mode 'org-export-mine)
  ))


(setf compilation-ask-about-save nil)
(global-set-key (kbd "M-c") 'erjoalgo-compile-compile)
(global-set-key (kbd "M-C")
		(lambda (arg) (interactive "P")
		  (call-interactively
		   'erjoalgo-compile-ask arg)
		  (compile compile-command)))
;;(setf compilation-read-command nil)

(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)


;taken from
;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)
(setf compilation-ask-about-save nil)
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
      '(beeper-beep))))

(add-to-list 'compilation-finish-functions
	     'compilation-finished-notify)

;;TODO autoload recompile
(require 'compile)

