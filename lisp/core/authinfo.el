(defun authinfo-parse (&optional authinfo-filename)
  (let ((filename (or authinfo-filename (expand-file-name "~/.authinfo"))))
    (if (not (file-exists-p filename))
        (warn "authinfo doesn't exist: %s" filename)
      (cl-loop with contents = (debian-file->string filename)
               for line in (s-split "\n" contents t)
               collect
               (cl-loop for (key val . rest)
                        on (s-split " " line t) by #'cddr
                        collect (cons (intern key) val))))))


(defun authinfo-get (host)
  (cl-loop for alist in (authinfo-parse)
           thereis (when (equal host (alist-get 'machine alist))
                     alist)))
