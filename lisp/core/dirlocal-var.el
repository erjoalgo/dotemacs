(cl-defmacro dirlocal-update (var-name fn &optional default)
  "Persistent (setq (funcall FN (or VAR-NAME DEFAULT)))."
  `(let ((mode "."))
     (unless (boundp ',var-name)
       (add-dir-local-variable mode ',var-name ,default)
       (setq ,var-name ,default))
     (let* ((old-val (symbol-value ',var-name))
            (new-val (funcall ,fn old-val)))
       (modify-dir-local-variable "." ',var-name new-val 'add-or-replace)
       (setq ,var-name new-val)
       new-val)))

(cl-defmacro dirlocal-get (var-name &optional default)
  "Persistent (or VAR-NAME DEFAULT)."
  `(dirlocal-update ,var-name (function identity) ,default))

(cl-defmacro dirlocal-set (var-name value)
  "Persistent (setq VAR-NAME VALUE)."
  `(dirlocal-update ,var-name ,value (lambda (_) ,value)))

(provide 'dirlocal-var)
