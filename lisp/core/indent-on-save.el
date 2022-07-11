(define-minor-mode indent-on-save-mode
  "Mode to auto-indent on save"
  :init-value 0
  (funcall (if indent-on-save-mode #'add-hook #'remove-hook)
           #'before-save-hook #'indent-on-save-indent))

(defun indent-on-save-indent ()
  "Indent the entire buffer."
  (unless (bound-and-true-p python-mode)
    (indent-region (point-min) (point-max))))
