(defun sexp-db-put (fn key val)
  "persistently put (key val) into alist"
  (interactive (list
   (read-file-name "enter filename: ")
   (read-string "enter key: ")
   (read-expression "enter expression")))
  (let* ((db-val (cons key val)))
    (sexp-db-append-sexp-to-file fn db-val )
    (add-to-list (sexp-db-sym-from-fn fn) db-val)))

(defun sexp-db-append-sexp-to-file (fn sexp)
  (find-file-noselect fn)
  (set-buffer (find-buffer-visiting fn))
  (goto-max)
  (print sexp (current-buffer))
  (save-buffer))

(defun sexp-db-sym-from-fn (fn)
  (intern (format "sexp-db-%s-alist" (basename-name (basename fn)))))

(defun sexp-db-alist-from-fn (fn)
  (let* ((sym (sexp-db-sym-from-fn fn)))
    (unless (boundp sym) (set sym nil ))
    (symbol-value  sym)))

(defun sexp-db-get (fn name)
  (let* ((sym (sexp-db-sym-from-fn fn))
	 (alist (symbol-value sym)))
    (cdr (assoc name alist))))

(defun sexp-db-load (fn)
  (interactive)
  (if (not (file-exists-p fn))
      (shell-command (format "touch '%s'" fn)))
  
  (let* ((text (get-string-from-file fn))
	 (sym (sexp-db-sym-from-fn fn)))
    (set sym
	 ;;reverse so that entries added later in the file appear earlier in alist
	 (reverse (read (format "(%s)" text))))))
