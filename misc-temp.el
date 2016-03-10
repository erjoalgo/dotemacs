;;minibuffer-local-map
(load-file (f-join "~/python-buttons.el"))
(elisp_install_buttons read-expression-map)
(add-hook 'emacs-lisp-mode-hook 'elisp_install_buttons)
(add-hook 'ielm-mode-hook 'elisp_install_buttons)
(add-hook 'slime-repl-mode-hook 'elisp_install_buttons)

(add-to-list 'load-path (f-join (getenv "GOPATH")
				"src/github.com/golang/lint/misc/emacs"))
