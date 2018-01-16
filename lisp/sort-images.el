(defun sort-images (top)
  (interactive "Denter directory of duplicated photos: ")
  (let ((ok-dir (f-join top "../ok"))
	(delete-dir (f-join top "../delete")))
    (unless (file-exists-p ok-dir)
      (make-directory ok-dir))
    (unless (file-exists-p delete-dir)
      (make-directory delete-dir))

    ;;start making thumbs in the bg
    (let* (fns cmds)
      (walk-dir-tree top
		     (lambda (fn)
		       (when
			   (image-filename-p fn)
			 (push fn fns))
		       '(make-thumb fn nil t)
		       '(sit-for .5)))
      (setf cmds (remove-if-not 'identity
				(mapcar (lambda (fn)
					  (make-thumb fn nil "cmd"))
					fns)))
      (pmap-subprocess cmds 10))

    (let ((fun (lambda (fn)
		 (show-image fn)
		 (let* ((dest-dir
			 (if (y-or-n-p "delete? ")
			     delete-dir
			   ok-dir))
			(dest-file (f-join dest-dir
					   (f-filename fn))))
		   (unless (file-exists-p dest-file)
		     (rename-file fn dest-file))
		   (kill-buffer)))))
      (walk-dir-tree top fun))))

(defun image-filename-p (fn)
  (and (f-ext fn)
       (member (downcase (f-ext fn)) '("jpg" "jpeg" "png" ))))

(defun make-thumb (fn &optional dims async)
  (unless dims (setf dims "400x400"))
  (assert fn)
  (let* (
	 (fn (expand-file-name fn))
	 (thumb-top "/tmp/thumbs")
	 (thumb-fn
	  (f-join thumb-top
		  (concat
		   (f-base fn) "-THUMB."
		   (f-ext fn)))))

    (unless (file-exists-p thumb-top)
      (make-directory thumb-top))

    (if (file-exists-p thumb-fn)
	(unless (equal "cmd" async) thumb-fn)
      (cond ((null async)
	     (call-process "convert" nil "*thumb-conversion*" nil
			   fn "-resize" dims thumb-fn)
	     thumb-fn)

	    ((eq t async)
	     (start-process "*thumb-conversion*" "*thumb-conversion*"
			    "convert" fn "-resize" dims thumb-fn)
	     thumb-fn)

	    ((equal "cmd" async)
	     `((lambda () (not (file-exists-p ,thumb-fn)))
	       ("convert" ,fn "-resize" ,dims ,thumb-fn)
	       ;(lambda () (find-file-noselect ,thumb-fn))
	       nil
	       ))))))

(defun show-image (fn)
  (interactive "fenter image location: ")
  (let ((thumb-fn
	 (make-thumb fn)))
	(find-file thumb-fn)))

(defun pmap-subprocess (cmds-list &optional max-subproc)
  (unless max-subproc (setf max-subproc 10))
  (lexical-let ((counter 0)
		(chunks (chunk-list cmds-list max-subproc))
		(max-subproc max-subproc)
		(next-chunk (gensym "next-chunk")))
    (fset next-chunk
	  (lambda
	    ()
	    (setf counter 0)
	    ;(message "starting next chunk: %s" next-chunk)
	    (mapc (lambda (cmd)
		    ;(message "cmd is %s" cmd)
		    (destructuring-bind
			(precond cmd post-action) cmd
		      (lexical-let ((precond precond)
				    (post-action post-action))
			(let ((proc (when (or (not precond)
					      (funcall precond))
				      (apply 'start-process "proc-chunk"
					     "*proc-chunk*" cmd)))
			      (sentinel (lambda (proc change)
					  ;(message "change is %s " change)
					  (when (or
						 (s-contains? "finished" change)
						 (s-contains? "exited" change))
					    (incf counter)
					    ;(message "counter is %s" counter)
					    (when post-action (funcall post-action))
					    (when (= counter max-subproc)
					      (funcall next-chunk))))))
			  (if proc (set-process-sentinel proc sentinel)
			    (funcall sentinel nil "finished"))))))
		  (pop chunks))))
    (funcall next-chunk)))

(defun chunk-list (l n)
  (let (chunks next-chunk)
    (loop for elm in (reverse l)
	  for i from 0
	  do (progn
	       (push elm next-chunk)
	       (pop l)
	       (when (zerop (mod i n))
		 (push next-chunk chunks)
		 (setf next-chunk nil))))
    (when next-chunk (push next-chunk chunks))
    chunks))
