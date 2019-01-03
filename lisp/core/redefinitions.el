

(defun ask-user-about-supersession-threat (fn)
  "Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called."
  (discard-input)
  (save-window-excursion
    (let ((prompt
	   (format "%s changed on disk; \
really edit the buffer? (y, n, r or C-h) "
		   (file-name-nondirectory fn)))
	  (choices '(?y ?n ?r ?? ?\C-h))
	  answer)
      (while (null answer)
	(setq answer (if (and (boundp 'dont-ask-user-about-supersession-threat)
			      dont-ask-user-about-supersession-threat)
			 (string-to-char "y")
			 (read-char-choice prompt choices)))
	(cond ((memq answer '(?? ?\C-h))
	       (ask-user-about-supersession-help)
	       (setq answer nil))
	      ((eq answer ?r)
	       ;; Ask for confirmation if buffer modified
	       (revert-buffer nil (not (buffer-modified-p)))
	       (signal 'file-supersession
		       (list "File reverted" fn)))
	      ((eq answer ?n)
	       (signal 'file-supersession
		       (list "File changed on disk" fn)))))
      (message
       "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))



(defvar compilation-kill-previous-without-prompt t)

'(defun compilation-start (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
	  (if (eq mode t)
	      "compilation"
	    (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
	 (thisdir default-directory)
	 (thisenv compilation-environment)
	 outwin outbuf)
    (with-current-buffer
	(setq outbuf
	      (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function)))
      (let ((comp-proc (get-buffer-process (current-buffer))))
      (if comp-proc
          (if (or (not (eq (process-status comp-proc) 'run))
                  (eq (process-query-on-exit-flag comp-proc) nil)
		  compilation-kill-previous-without-prompt
                  (yes-or-no-p
                   (format "A %s process is running; kill it? "
                           name-of-mode)))
              (condition-case ()
                  (progn
                    (interrupt-process comp-proc)
                    (sit-for 1)
                    (delete-process comp-proc))
                (error nil))
            (error "Cannot have two processes in `%s' at once"
                   (buffer-name)))))
      ;; first transfer directory from where M-x compile was called
      (setq default-directory thisdir)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
	    (default-directory thisdir))
	;; Then evaluate a cd command if any, but don't perform it yet, else
	;; start-command would do it again through the shell: (cd "..") AND
	;; sh -c "cd ..; make"
	(cd (cond
             ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]"
                                 command))
              default-directory)
             ((not (match-end 1)) "~")
             ((eq (aref command (match-beginning 1)) ?\')
              (substring command (1+ (match-beginning 1))
                         (1- (match-end 1))))
             ((eq (aref command (match-beginning 1)) ?\")
              (replace-regexp-in-string
               "\\\\\\(.\\)" "\\1"
               (substring command (1+ (match-beginning 1))
                          (1- (match-end 1)))))
             ;; Try globbing as well (bug#15417).
             (t (let* ((substituted-dir
                        (substitute-env-vars (match-string 1 command)))
                       ;; FIXME: This also tries to expand `*' that were
                       ;; introduced by the envvar expansion!
                       (expanded-dir
                        (file-expand-wildcards substituted-dir)))
                  (if (= (length expanded-dir) 1)
                      (car expanded-dir)
                    substituted-dir)))))
	(erase-buffer)
	;; Select the desired mode.
	(if (not (eq mode t))
            (progn
              (buffer-disable-undo)
              (funcall mode))
	  (setq buffer-read-only nil)
	  (with-no-warnings (comint-mode))
	  (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be done after (funcall mode) as that resets local variables
        (set (make-local-variable 'compilation-directory) thisdir)
	(set (make-local-variable 'compilation-environment) thisenv)
	(if highlight-regexp
	    (set (make-local-variable 'compilation-highlight-regexp)
		 highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
		(eq compilation-scroll-output 'first-error))
            (set (make-local-variable 'compilation-auto-jump-to-next) t))
	;; Output a mode setter, for saving and later reloading this buffer.
	(insert "-*- mode: " name-of-mode
		"; default-directory: "
                (prin1-to-string (abbreviate-file-name default-directory))
		" -*-\n"
		(format "%s started at %s\n\n"
			mode-name
			(substring (current-time-string) 0 19))
		command "\n")
	(setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf '(nil (allow-no-window . t))))
    (with-current-buffer outbuf
      (let ((process-environment
	     (append
	      compilation-environment
	      (if (if (boundp 'system-uses-terminfo);`If' for compiler warning.
		      system-uses-terminfo)
		  (list "TERM=dumb" "TERMCAP="
			(format "COLUMNS=%d" (window-width)))
		(list "TERM=emacs"
		      (format "TERMCAP=emacs:co#%d:tc=unknown:"
			      (window-width))))
	      ;; Set the EMACS variable, but
	      ;; don't override users' setting of $EMACS.
	      (unless (getenv "EMACS")
		(list "EMACS=t"))
	      (list "INSIDE_EMACS=t")
	      (copy-sequence process-environment))))
	(set (make-local-variable 'compilation-arguments)
	     (list command mode name-function highlight-regexp))
	(set (make-local-variable 'revert-buffer-function)
	     'compilation-revert-buffer)
	(and outwin (set-window-start outwin (point-min)))

	;; Position point as the user will see it.
	(let ((desired-visible-point
	       ;; Put it at the end if `compilation-scroll-output' is set.
	       (if compilation-scroll-output
		   (point-max)
		 ;; Normally put it at the top.
		 (point-min))))
	  (goto-char desired-visible-point)
	  (when (and outwin (not (eq outwin (selected-window))))
	    (set-window-point outwin desired-visible-point)))

	;; The setup function is called before compilation-set-window-height
	;; so it can set the compilation-window-height buffer locally.
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	(and outwin (compilation-set-window-height outwin))
	;; Start the compilation.
	(if (fboundp 'start-process)
	    (let ((proc
		   (if (eq mode t)
		       ;; comint uses `start-file-process'.
		       (get-buffer-process
			(with-no-warnings
			  (comint-exec
			   outbuf (downcase mode-name)
			   (if (file-remote-p default-directory)
			       "/bin/sh"
			     shell-file-name)
			   nil `("-c" ,command))))
		     (start-file-process-shell-command (downcase mode-name)
						       outbuf command))))
              ;; Make the buffer's mode line show process state.
              (setq mode-line-process
                    '(:propertize ":%s" face compilation-mode-line-run))

              ;; Set the process as killable without query by default.
              ;; This allows us to start a new compilation without
              ;; getting prompted.
              (when compilation-always-kill
                (set-process-query-on-exit-flag proc nil))

              (set-process-sentinel proc 'compilation-sentinel)
              (unless (eq mode t)
                ;; Keep the comint filter, since it's needed for proper
		;; handling of the prompts.
		(set-process-filter proc 'compilation-filter))
	      ;; Use (point-max) here so that output comes in
	      ;; after the initial text,
	      ;; regardless of where the user sees point.
	      (set-marker (process-mark proc) (point-max) outbuf)
	      (when compilation-disable-input
		(condition-case nil
		    (process-send-eof proc)
		  ;; The process may have exited already.
		  (error nil)))
	      (run-hook-with-args 'compilation-start-hook proc)
              (setq compilation-in-progress
		    (cons proc compilation-in-progress)))
	  ;; No asynchronous processes available.
	  (message "Executing `%s'..." command)
	  ;; Fake mode line display as if `start-process' were run.
	  (setq mode-line-process
		'(:propertize ":run" face compilation-mode-line-run))
	  (force-mode-line-update)
	  (sit-for 0)			; Force redisplay
	  (save-excursion
	    ;; Insert the output at the end, after the initial text,
	    ;; regardless of where the user sees point.
	    (goto-char (point-max))
	    (let* ((inhibit-read-only t) ; call-process needs to modify outbuf
		   (compilation-filter-start (point))
		   (status (call-process shell-file-name nil outbuf nil "-c"
					 command)))
	      (run-hooks 'compilation-filter-hook)
	      (cond ((numberp status)
		     (compilation-handle-exit
		      'exit status
		      (if (zerop status)
			  "finished\n"
			(format "exited abnormally with code %d\n" status))))
		    ((stringp status)
		     (compilation-handle-exit 'signal status
					      (concat status "\n")))
		    (t
		     (compilation-handle-exit 'bizarre status status)))))
	  (set-buffer-modified-p nil)
	  (message "Executing `%s'...done" command)))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir)
      ;; The following form selected outwin ever since revision 1.183,
      ;; so possibly messing up point in some other window (bug#1073).
      ;; Moved into the scope of with-current-buffer, though still with
      ;; complete disregard for the case when compilation-scroll-output
      ;; equals 'first-error (martin 2008-10-04).
      (when compilation-scroll-output
	(goto-char (point-max))))

    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))
