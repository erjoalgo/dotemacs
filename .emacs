(defvar emacs-top
  ;;  find .emacs's real location
  ;;  (concat (expand-file-name ".") "/"))
  (file-name-directory (file-truename "~ealfonso/.emacs")))

(defun add-hook-to-modes (hook mode-sym-list)
  (dolist (mode-sym mode-sym-list)
  (let ((hook-sym (intern (concat (symbol-name mode-sym) "-hook"))))
    (add-hook hook-sym hook)
    (when (derived-mode-p mode-sym)
      (funcall hook)))))

(dolist (dir '("libs" "basic" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(mapcar 'require
	'(f
	  goto-last-change
	  quick-yes
	  cl-lib
	  cl
	  erjoalgo-command-mode
	  zoom-global
	  isearch-fast-reverse

	  legalese
	  my-emacs-settings))



(mapc 'load-file
      (list (f-join emacs-top "misc-temp.el")
	    (f-join emacs-top "extra" "misc-utils.el")))

(require 'proxy-mode)
(require 'plusx)

(defun load-file-safe (fn)
  (condition-case ex (load fn)
    ('error
     (message "WARNING: unable to load %s:\n %s" fn ex))))




(dolist (top-base '("libs-submodules" "libs-dirs"))
  (loop with top = (f-join emacs-top top-base)
	for lib-dir in (directory-files top)
	as fn = (f-join top lib-dir)
	if (file-directory-p fn)  do
	(add-to-list 'load-path fn)))

(loop for dir in '("settings" "sensitive" "extra")
      as top = (f-join emacs-top dir)
      if (file-exists-p top) do
      (loop when (file-exists-p top)
	    for fn in (directory-files top)
	    as fn = (f-join top fn)
	    if (file-regular-p fn) do
	    (load-file-safe fn)))

(ensure-packages-exist
 '(company legalese go-mode magit))
