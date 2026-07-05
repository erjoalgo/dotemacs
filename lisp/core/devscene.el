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

(def-devscene chess
  (
   ("/home/ealfonso/git/chess-tactics/service/src/clj/chess/core.clj"
    (lambda ()
      (setq cider-post-connect-sexp "(do (ns chess.core) (start-app []))")
      (call-interactively #'cider-jack-in)))

   ("/home/ealfonso/git/chess-tactics/liquibase/src/db.changelog.xml")

   ("http://localhost:3000/" )))

(def-devscene brainink
  (
   ("/home/ealfonso/git/brainink/service/src/clj/service/core.clj"
    (lambda ()
      (setq cider-post-connect-sexp "(do (ns service.core) (start-app []))")
      (call-interactively #'cider-jack-in)))

   ("/home/ealfonso/git/brainink/ui/" "npm run watch")

   ("/home/ealfonso/git/brainink/ui/src/ink/core.cljs")

   ("http://localhost:8280/" )
   ("http://localhost:3000/" )))

(def-devscene myhomedates
  (
   ("/home/ealfonso/git/myhomedates/service/src/clj/service/core.clj"
    (lambda ()
      (setq cider-post-connect-sexp "(do (ns service.core) (start-app []))")
      (call-interactively #'cider-jack-in)))

   ("/home/ealfonso/git/myhomedates/service/src/clj/service/routes/app.clj")
   ("/home/ealfonso/git/myhomedates/ui/" "npm run dev")

   ("/home/ealfonso/git/myhomedates/ui/app/page.tsx")

   ("http://localhost:3000/" )))
