(define-minor-mode indent-on-save-mode
  "Mode to auto-indent on save" 0 nil nil
  (funcall (if indent-on-save-mode #'add-hook #'remove-hook)
           #'before-save-hook #'indent-on-save-indent))

(defun indent-on-save-indent ()
  "Indent the entire buffer."
  (indent-region (point-min) (point-max)))