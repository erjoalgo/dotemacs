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
 (f-join emacs-top "misc-temp.el"))

(require 'proxy-mode)
(require 'plusx)

(defun load-file-safe (fn)
  (condition-case ex (load fn)
    ('error (message "WARNING: unable to load %s:\n %s" fn ex))))

(loop with top = (f-join emacs-top "settings")
      for fn in (directory-files top) do
      (load-file-safe (f-join top fn)))
