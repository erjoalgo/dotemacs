(defun autoindent-indent-whole-buffer ()
  (indent-region (point-min) (point-max)))

(defvar autoindent-modes-list '(emacs-lisp-mode lisp-mode)
  "Modes on which to auto-indent after save.")

(defun autoindent-save-hook ()
  (when (member major-mode autoindent-modes-list)
    (autoindent-indent-whole-buffer)))

(add-hook 'before-save-hook #'autoindent-save-hook)
