(defvar emacs-top
  ;;  find .emacs's real location
  ;;  (concat (expand-file-name ".") "/"))
  (file-name-directory (file-truename "~ealfonso/.emacs")))

(dolist (dir '("libs" "basic" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(require 'f)
(require 'goto-last-change)
(require 'quick-yes)
;;(require 'help-fns+)

(require 'cl-lib)
(require 'cl)

(require 'erjoalgo-command-mode)
(require 'zoom-global)
(require 'isearch-fast-reverse)

(require 'legalese)
(require 'my-emacs-settings)


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
