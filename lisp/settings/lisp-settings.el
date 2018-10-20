(defvar vom-log-levels
  '(

    "*config*"	"*log-formatter*"	"*log-hook*"
    "*log-stream*"	"*time-formatter*"	"alert"
    "config"	"crit"	"debug"
    "debug1"	"debug2"	"debug3"
    "debug4"	"emerg"	"error"
    "info"	"notice"	"warn"
    ))

(defmacro safe-wrap (fn &rest clean-up)
  "from
  https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/"
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defmacro with-highlight-region (a b body)
  (let ((overlay (gensym "overlay")))
    `(let ((,overlay (make-overlay ,a ,b)))
       (overlay-put ,overlay 'priority 1001)
       (overlay-put ,overlay 'face 'query-replace)
       (safe-wrap ,body (delete-overlay ,overlay)))))

(defun vom-define-log-levels ()
  (interactive)
  '(replace-regexp "format t\\|vom:[*a-z-0-9]+"
                   "vom:,\\(comp vom-log-levels \\1)")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "format t\\|vom:[*a-z-0-9]+" nil t)
      ;; (replace-highlight)
      (with-highlight-region
       (match-beginning 0) (match-end 0)
       (let ((rep (comp vom-log-levels)))
         (when rep
           (replace-match (concat "vom:" rep))))))))
