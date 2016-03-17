;;minibuffer-local-map
(load-file (f-join emacs-top "in-progress/python_buttons.el"))

(add-hook 'emacs-lisp-mode-hook 'elisp_install_buttons)
(add-hook 'ielm-mode-hook 'elisp_install_buttons)
(add-hook 'slime-repl-mode-hook 'elisp_install_buttons)

(when (getenv "GOPATH")
  (add-to-list 'load-path (f-join (getenv "GOPATH")
				"src/github.com/golang/lint/misc/emacs")))
(elisp_install_buttons read-expression-map)

(defun upcase-last ()
  (interactive)
  (save-excursion
    (upcase-region (point)
		 (progn
		   (backward-sexp)
		   (point)))))
