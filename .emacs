(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))
;;find .emacs's real location
(setq emacs-top (s-trim-right (shell-command-to-string "dirname $(readlink ~/.emacs)")))
;;add libs, basic to load-path
(add-to-list 'load-path (concat emacs-top "/libs"))
(add-to-list 'load-path (concat emacs-top "/basic"))

(require 'f);; python-like os.path for emacs lisp. not needed anymore
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

(setq proxy-mode-proxy "http://proxy-src.research.ge.com:8080")
(require 'proxy-mode)
