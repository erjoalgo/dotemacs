(defun advice-unadvice (sym)
  ;; from somewhere on the internet
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defmacro mock (fn-spec &rest body)
  (cl-destructuring-bind (fn-sym-spec arg-spec . new-def) fn-spec
    (cl-destructuring-bind (fn-sym optional-new-name)
        (if (listp fn-sym-spec) fn-sym-spec (list fn-sym-spec nil))
      `(cl-letf* (,@(when optional-new-name
                      `(((symbol-function ',optional-new-name)
                         (symbol-function ',fn-sym))))
                  ((symbol-function ',fn-sym)
                   (lambda ,arg-spec
                     ,(format "temporary dynamic redefintion of %s" fn-sym)
                     ,@new-def)))
         ,@body))))

(mock (expt (b e)
            (message "inside mock %s %s" b e)
            9)
      (expt 1 0)) ;; => 9

(mock ((expt orig-expt) (b e)
       (message "inside mock %s %s" b e)
       9)
      (list (expt 1 0)
            (orig-expt 1 0)));; => (9 1)
