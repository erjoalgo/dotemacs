(defun modify-dir-local-variable (mode variable value op &optional nosave)
  "Modify directory-local VARIABLE in .dir-locals.el depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new directory-local VARIABLE
with VALUE to the MODE alist where MODE can be a mode name symbol or
a subdirectory name.

If .dir-locals.el was not found and OP is not `delete' then create
this file in the current directory.

If OP is `delete' then delete all existing settings of VARIABLE
from the MODE alist ignoring the input argument VALUE."
  (catch 'exit
    (unless enable-local-variables
      (throw 'exit (message "Directory-local variables are disabled")))
    (let* ((dir-or-cache (and (buffer-file-name)
                              (not (file-remote-p (buffer-file-name)))
                              (dir-locals-find-file (buffer-file-name))))
           (variables-file
            ;; If there are several .dir-locals, the user probably
            ;; wants to edit the last one (the highest priority).
            (cond ((stringp dir-or-cache)
                   (car (last (dir-locals--all-files dir-or-cache))))
                  ((consp dir-or-cache) ; result from cache
                   ;; If cache element has an mtime, assume it came
                   ;; from a file.  Otherwise, assume it was set
                   ;; directly.
                   (if (nth 2 dir-or-cache)
                       (car (last (dir-locals--all-files (car dir-or-cache))))
                     (cadr dir-or-cache)))
                  ;; Try to make a proper file-name.
                  (t (expand-file-name dir-locals-file))))
           variables)
      ;; I can't be bothered to handle this case right now.
      ;; Dir locals were set directly from a class.  You need to
      ;; directly modify the class in dir-locals-class-alist.
      (and variables-file (not (stringp variables-file))
           (throw 'exit (message "Directory locals were not set from a file")))
      ;; Don't create ".dir-locals.el" for the deletion operation.
      (and (eq op 'delete)
           (or (not variables-file)
               (not (file-exists-p variables-file)))
           (throw 'exit (message "No .dir-locals.el file was found")))

      (let ((auto-insert nil))
        (with-current-buffer (find-file-noselect variables-file)
      (widen)
      (goto-char (point-min))

      ;; Read alist of directory-local variables.
      (ignore-errors
        (delete-region
         (prog1 (point)
           (setq variables (let ((read-circle nil))
                             (read (current-buffer)))))
         (point)))

      ;; Add or replace variable in alist of directory-local variables.
      (let ((mode-assoc (assoc mode variables)))
        (if mode-assoc
            (setq variables
                  (cons (cons mode
                              (if (eq op 'delete)
                                  (assq-delete-all variable (cdr mode-assoc))
                                (cons
                                 (cons variable value)
                                 (if (memq variable '(mode eval))
                                     (cdr mode-assoc)
                                   (assq-delete-all variable (cdr mode-assoc))))))
                        (assq-delete-all mode variables)))
          (setq variables
                (cons `(,mode . ((,variable . ,value)))
                      variables))))

      ;; Insert modified alist of directory-local variables.
      (insert ";;; Directory Local Variables\n")
      (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
          (prog1
      (pp (sort variables
                (lambda (a b)
                  (cond
                   ((null (car a)) t)
                   ((null (car b)) nil)
                   ((and (symbolp (car a)) (stringp (car b))) t)
                   ((and (symbolp (car b)) (stringp (car a))) nil)
                   (t (string< (car a) (car b))))))
                  (current-buffer))
            (unless nosave (save-buffer))))))))
