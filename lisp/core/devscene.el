(defmacro def-devscene (name buffers)
  (declare (indent 1))
  `(defun ,(intern (format "devscene-%s" name)) ()
     (interactive)
     (message "DDEBUG kwur ,buffers: %s" ',buffers)
     (devscene-load ',buffers)))

(defun devscene-load (buffers)
  (cl-loop for spec in buffers
           do (message "DDEBUG 9clp spec: %s" spec)
           do (cl-destructuring-bind (buffer action)
                  (cond
                   ((atom spec) (list spec nil))
                   ((null (cdr spec)) (list (car spec) nil))
                   (t spec))
                (cond
                 ((s-starts-with-p "http" buffer)
                  (browse-url buffer))
                 (t (find-file buffer)))
                (when action
                  (cond
                   ((stringp action) (compile action))
                   (t (funcall action)))))))

(def-devscene brainink
  (
   ("/home/ealfonso/git/brainink/service/src/clj/service/core.clj"
    (lambda ()
      (setq cider-post-connect-eval-sexp "(do (ns service.core) (start-app []))")
      (call-interactively #'cider-jack-in)))

   ("/home/ealfonso/git/brainink/ui/" "npm run watch")

   ("/home/ealfonso/git/brainink/ui/src/ink/core.cljs")

   ("http://localhost:8280/" )
   ("http://localhost:3000/" )))


