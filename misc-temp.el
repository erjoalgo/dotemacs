;;minibuffer-local-map
(load-file
 (f-join emacs-top "in-progress/python_buttons.el"))


(add-hook 'ielm-mode-hook 'elisp_install_buttons)
(add-hook 'slime-repl-mode-hook 'elisp_install_buttons)
(add-hook 'emacs-lisp-mode-hook 'elisp_install_buttons)
(add-hook 'lisp-mode-hook 'lisp_install_buttons)
(add-hook 'lisp-mode-hook 'slime-mode)
(elisp_install_buttons read-expression-map)

(add-hook 'shell-script-mode 'bash_install_buttons)



(defun upcase-last ()
  (interactive)
  (save-excursion
    (upcase-region (point)
		 (progn
		   (backward-sexp)
		   (point)))))
