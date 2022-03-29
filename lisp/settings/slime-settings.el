(defvar slime-helper-filename nil "Quicklisp slime helper")

(setq slime-helper-filename
      (expand-file-name "~/quicklisp/slime-helper.el"))

(if (not (file-exists-p slime-helper-filename))
    (warn "slime helper %s does not exist. hint: %s"
	  slime-helper-filename
	  '(ql:quickload 'quicklisp-slime-helper))
  ;; not the full slime
  (load slime-helper-filename))

(with-eval-after-load "slime"
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (require 'slime-autoloads)
  (add-hook 'lisp-mode-hook 'slime-mode)

  (defun find-buffer-by-prefix (prefix)
    (cl-loop for buff in (buffer-list)
	     thereis (and (s-starts-with-p
			   prefix
			   (buffer-name buff))
			  buff)))



  (add-hook 'sldb-hook 'visual-line-mode)
  (add-hook 'sldb-hook 'beginning-of-buffer)
  (setf slime-load-failed-fasl 'never)

  (defun slime-killall ()
    (interactive)
    (kill-buffers-matching-regexp "[*]\\(slime-repl\\|inferior-lisp\\|slime\\)")
    (slime-disconnect-all)))

(defmacro slime-eval-async-and-message (form)
  "slime-eval-async-and-message FORM."
  `(slime-eval-async ,form
    (lambda (result) (message "%s" result))))

(defun slime-unexport-all-symbols (package)
  "Unexport all symbols in PACKAGE."
  (interactive (list
                (slime-read-package-name "from package: "
                                         (slime-current-package))))
  (slime-eval-async-and-message `(dbg:unexport-all-symbols ,package)))

(defun slime-unintern-shadowing-symbols (package)
  "Unintern the symbol given with SYMBOL-NAME PACKAGE."
  (interactive
   (list
    (slime-read-package-name "from package: "
                             (slime-current-package))))
  (slime-eval-async-and-message `(dbg:unintern-all-symbols ,package)))

(defun process-ignore-on-exit (regexp)
  (cl-loop for proc in (process-list)
           when (s-matches-p regexp (process-name proc))
           do
           (progn (message "disabling query-on-exit for '%s'" proc)
                  (set-process-query-on-exit-flag proc nil))))

(defun slime-ignore-processes-on-exit (&rest r)
  (process-ignore-on-exit "SLIME"))

(advice-add #'save-some-buffers :before #'slime-ignore-processes-on-exit)
