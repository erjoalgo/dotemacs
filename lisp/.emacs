(defvar emacs-top
  ;;  find .emacs's real location
  ;;  (concat (expand-file-name ".") "/"))
  (let ((ealfonso
	(file-name-directory (file-truename "~ealfonso/.emacs"))))
    (if (and (file-regular-p ealfonso) (file-exists-p ealfonso))
	ealfonso
      (file-name-directory (file-truename "~/.emacs")))))


(defun add-hook-to-modes (hook mode-sym-list)
  (dolist (mode-sym mode-sym-list)
  (let ((hook-sym (intern (concat (symbol-name mode-sym) "-hook"))))
    (add-hook hook-sym hook)
    (when (derived-mode-p mode-sym)
      (funcall hook)))))


(defun safe-fun (fun-sym)
  `(lambda (&rest args)
     (condition-case ex (apply ',fun-sym args)
       ('error
	(warn ,(format "WARNING: unable to %s on args %%s %%s:\n" fun-sym)
	      args ex)))))

(dolist (dir '("libs" "core" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(mapc (safe-fun 'require)
	'(f
	  goto-last-change
	  quick-yes
	  cl-lib
	  cl
	  zoom-global
	  isearch-fast-reverse
	  my-emacs-settings
	  proxy-mode
	  plusx
	  dash
	  dash-functional
	  dedicated
	  ))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (defun ensure-packages-exist (packages)
    (let (refreshed-p)
      (dolist (package packages)
	(when
	    (and (not (package-installed-p package)))
	  (condition-case ex
	      (progn (or refreshed-p (progn
				       (package-refresh-contents)
				       (setf refreshed-p t)))
		     (package-install package))
	    ('error
	     (warn "WARNING: unable to install %s:\n %s" package ex)))))))

  (ensure-packages-exist
   '(company legalese magit dash dash-functional go-mode calfw calfw-gcal
	     ;; gnus-desktop-notify
	     java-imports bbdb nginx-mode
	     dedicated
	     ))

  (funcall (safe-fun 'require) 'company)
  (require 'dash)
  (require 'dash-functional))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (feature &rest forms)
    `(eval-after-load ,feature '(progn ,@forms))))

(loop with top = (f-join emacs-top "libs")
      for lib-dir in (directory-files top)
      as fn = (f-join top lib-dir)
      if (file-directory-p fn)  do
      (add-to-list 'load-path fn))

(loop with safe-load = (safe-fun 'load)
      for dir in `("core" "private" "settings" "extra"
                   ,(expand-file-name "~/private-data/emacs-lisp"))
      as top = (if (file-name-absolute-p dir) dir
                 (f-join emacs-top dir))
      if (file-exists-p top) do
      (loop when (file-exists-p top)
	    for fn in (directory-files top)
	    as fn = (f-join top fn)
	    if (and (file-regular-p fn)
		    (equal "el" (f-ext fn)))
	    do
	    (funcall safe-load fn)))
