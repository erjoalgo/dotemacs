'(defalias-tmp ((tmp 'message)
               (ins 'insert-rec-template))
  (tmp "hola mundo")
  (ins "for (int {0} = 0; {0} < {}; {0}++)"))

;; (tmp "hola mundo")


;; (buttons+)
'(macroexpand-1
 '(macrolet+ ((ins buttons-insert-rec-template))
             (ins "for (int {0} = 0; {0} < {}; {0}++)")
             'nice))
(defmacro macrolet+ (lets &rest body)
      `(macrolet
           ;; list
           ,(cl-loop for (name arglist . body) in lets
                  collect (if (atom arglist)
                              (let ((alias arglist))
                                (cl-assert(null body))
                                ;; (backquote ,(list name (list (quote &rest) (quote body)) (list alias ``,@body)))
                                `(,name (&rest body) (,alias ,@body))
                                )
                            `(,name ,arglist ,body)))
         ,@body))

(defmacro my-fancy-insert-macro (&rest args)
  `(progn
     (message "this is a fancy insert macro")
     (insert ,@args)))

(macroexpand
        '(defalias-tmp ((msg 'message)
                        (ins 'my-fancy-insert-macro))
           (msg "hello world")
           (ins "for (int {0} = 0; {0} < {}; {0}++)")
           'retval))


'(progn
  (defalias 'original-182 'ins)
  (defalias 'msg 'message)
  (defalias 'ins 'my-fancy-insert-macro)
  (prog1
      (progn
        (msg "hello world")
        (ins "for (int {0} = 0; {0} < {}; {0}++)")
        'retval)
    (fmakunbound 'msg)
    (defalias 'ins 'original-182)
    (fmakunbound 'original-182)))
