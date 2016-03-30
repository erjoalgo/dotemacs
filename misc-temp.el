;;minibuffer-local-map
(load-file
 (f-join emacs-top "in-progress/python_buttons.el"))


(add-hook 'ielm-mode-hook 'elisp_install_buttons)
(add-hook 'slime-repl-mode-hook 'elisp_install_buttons)
(add-hook 'emacs-lisp-mode-hook 'elisp_install_buttons)
(add-hook 'lisp-mode-hook 'cl_install_buttons)

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

(defun bash-identifier-current-line  ()
  (let ((line
	(buffer-substring-no-properties
	 (point)
	 (line-beginning-position))))
    (and
     (string-match
      "^[[:space:]]*\\([^=]+\\)="
      line)
     (match-string 1 line))))



(defun firefox-new-tab-sh (url &optional unknown-arg)
  (let ((new-tab "new-tab"))
    (start-process new-tab new-tab
		 "firefox-new-tab.sh"
		 url)))
(setq browse-url-browser-function 'firefox-new-tab-sh)
