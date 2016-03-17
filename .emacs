(unless (fboundp 'string-trim)  
  (defun string-trim (string)
    (let* ((regex "\\`[ \n\t\r]*\\(\\(.\\|\n\\)*?\\)[ \n\t\r]*\\'"))
      (string-match regex string)
      (match-string 1 string))))

(defvar emacs-top
  ;;find .emacs's real location
  (string-trim (shell-command-to-string "dirname $(readlink ~/.emacs)")))

(defvar hostname
  (string-trim (shell-command-to-string "hostname")))


;;for legalese
(setq user-mail-address "erjoalgo@gmail.com"
      user-full-name "Ernesto Alfonso")

;;add libs, basic to load-path
(add-to-list 'load-path (concat emacs-top "/libs"))


(add-to-list 'load-path (concat emacs-top "/basic"))
(add-to-list 'load-path (concat emacs-top "/extra"))

(require 'f);; python-like os.path for emacs lisp
(require 'goto-last-change);; jump to last buffer edit location. like eclipse alt+left
(require 'quick-yes);; auto-say "yes RET" to annoying yes prompts
;;(require 'help-fns+);; describe keymap

(require 'cl-lib)
(require 'cl)

(require 'command-mode)
(require 'zoom-global)
(require 'isearch-fast-reverse)

(require 'legalese)
(require 'emacs-settings)


(load-file (f-join emacs-top "misc-temp.el"))

;;todo hostname-specific settings

(require 'proxy-mode)
(require 'plusx)

(defun load-file-safe (fn)
  (condition-case ex (load fn)
    ('error (message "WARNING: unable to load %s:\n %s" fn ex))))

(loop with top = (f-join emacs-top "settings")
      for fn in (directory-files top) do
      (load-file-safe (f-join top fn)))
