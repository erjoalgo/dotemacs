(defvar emacs-top
  ;;find .emacs's real location
  (file-name-directory (file-truename "~/.emacs")))

(dolist (dir '("libs" "basic" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(require 'f)
(require 'goto-last-change)
(require 'quick-yes)
;;(require 'help-fns+)

(require 'cl-lib)
(require 'cl)

(require 'command-mode)
(require 'zoom-global)
(require 'isearch-fast-reverse)

(require 'legalese)
(require 'emacs-settings)


(load-file
 (f-join emacs-top "misc-temp.el")
 (f-join emacs-top "extra" "misc-utils.el"))

(require 'proxy-mode)
(require 'plusx)

(defun load-file-safe (fn)
  (condition-case ex (load fn)
    ('error (message "WARNING: unable to load %s:\n %s" fn ex))))


(loop with top = (f-join emacs-top "libs-dirs")
      for lib-dir in (directory-files top)
      as fn = (f-join top lib-dir)
      if (file-directory-p fn)  do
      (add-to-list 'load-path fn))

(dolist (dir '("settings" "sensitive"))
  (loop with top = (f-join emacs-top dir)
	for fn in (directory-files top)
	as fn = (f-join top fn)
	if (file-regular-p fn) do
	(load-file-safe fn)))
