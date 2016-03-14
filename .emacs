
;;find .emacs's real location
(setq emacs-top
      (car
       (split-string
	(shell-command-to-string "dirname $(readlink ~/.emacs)")
	      nil nil "[[:space:]]+")));;trim trailing newline
;;add libs, basic to load-path
(add-to-list 'load-path (concat emacs-top "/libs"))
(add-to-list 'load-path (concat emacs-top "/basic"))
(add-to-list 'load-path (concat emacs-top "/extra"))

(require 'f);; python-like os.path for emacs lisp
(require 'goto-last-change);; jump to last buffer edit location. like eclipse alt+left
(require 'quick-yes);; auto-say "yes RET" to annoying yes prompts
(require 'help-fns+);; describe keymap

(require 'cl-lib)
(require 'cl)

(require 'command-mode)
(require 'zoom-global)
(require 'isearch-fast-reverse)

(require 'legalese)
(require 'emacs-settings)
(setq user-mail-address "erjoalgo@gmail.com"
      user-full-name "Ernesto Alfonso")

(load-file (f-join emacs-top "misc-temp.el"))

;;todo hostname-specific settings
(setq proxy-mode-proxy "http://proxy-src.research.ge.com:8080")
(require 'proxy-mode)
(require 'plusx)
