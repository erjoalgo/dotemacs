(defvar emacs-top
  ;;  find .emacs's real location
  ;;  (concat (expand-file-name ".") "/"))
  (let ((ealfonso
	(file-name-directory (file-truename "~ealfonso/.emacs"))))
    (if (file-exists-p ealfonso) ealfonso
      (file-name-directory (file-truename "~/.emacs")))))


(defun add-hook-to-modes (hook mode-sym-list)
  (dolist (mode-sym mode-sym-list)
  (let ((hook-sym (intern (concat (symbol-name mode-sym) "-hook"))))
    (add-hook hook-sym hook)
    (when (derived-mode-p mode-sym)
      (funcall hook)))))


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (defun ensure-packages-exist (packages)
    (let (refreshed-p)
      (dolist (package packages)
	(when
	    (and (not (package-installed-p package))
		 (loop for i below 2 always
		       (y-or-n-p (format "connect to the internet to install %s? (%d)"
					 package i))))
	  (condition-case ex
	      (progn (or refreshed-p (progn
				       (package-refresh-contents)
				       (setf refreshed-p t)))
		     (package-install package))
	    ('error
	     (warn "WARNING: unable to install %s:\n %s" package ex)))))))

  (ensure-packages-exist
   '(company legalese magit)))

(defun safe-fun (fun-sym)
  `(lambda (&rest args)
     (condition-case ex (apply ',fun-sym args)
       ('error
	(warn ,(format "WARNING: unable to %s on args %%s:\n" fun-sym) args)))))

(dolist (dir '("lisp/libs" "lisp/core" "lisp/extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(mapc (safe-fun 'require)
	'(f
	  goto-last-change
	  quick-yes
	  cl-lib
	  cl
	  erjoalgo-command-mode
	  zoom-global
	  isearch-fast-reverse
	  company
	  legalese
	  my-emacs-settings
	  proxy-mode
	  plusx
	  ))

(loop with top = (f-join emacs-top "lisp" "libs")
      for lib-dir in (directory-files top)
      as fn = (f-join top lib-dir)
      if (file-directory-p fn)  do
      (add-to-list 'load-path fn))

(loop with safe-load = (safe-fun 'load)
      for dir in '("settings" "sensitive" "extra")
      as top = (f-join emacs-top "lisp" dir)
      if (file-exists-p top) do
      (loop when (file-exists-p top)
	    for fn in (directory-files top)
	    as fn = (f-join top fn)
	    if (file-regular-p fn) do
	    (funcall safe-load fn)))

