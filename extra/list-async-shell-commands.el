(let ((lexical-binding t))
  (defun list-async-shell-commands ()
    (interactive)
    (let ((list-asyncs-buf-name "*List of Async Shell Commands" ))
      (with-current-buffer-window
       list-asyncs-buf-name
       #'temp-buffer-show-function
       nil
       
       (loop for buf in (buffer-list) if
	     (and
	      (s-starts-with? "*Async Shell Command*"
			      (buffer-name buf))
	      (eq
	       (buffer-local-value 'major-mode buf)
	       'shell-mode))
	     do (let ((name (buffer-name buf))
		      (cmd (let ((proc (get-buffer-process buf)))
			     (when proc (process-command proc))))
		      (snippet (when (buffer-live-p buf)
				 (replace-regexp-in-string "\n" "\\n" (with-current-buffer buf
									(buffer-substring-no-properties
									 (point-min) (min (point-max) 100))) nil t)))
		      )
		  (princ (format "%s\t%s\t%s\n"
				 name cmd snippet))))
       (use-local-map (make-sparse-keymap))
       (cl-labels ((get-current-line-buffer ()
					    (get-buffer
					     (first (s-split "\t"
							     (buffer-substring-no-properties
							      (line-beginning-position)
							      (line-end-position)))))))
	 (local-set-key "n" 'next-line)
	 (local-set-key "p" 'previous-line)
	 (local-set-key "k" (lambda () (interactive)
			      (let ((buf (get-current-line-buffer))
				    proc)
				(when (and buf (setf proc (get-buffer-process buf)))
				  (kill-process proc))
				(when (buffer-live-p buf)
				  (kill-buffer buf))
				(read-only-mode 0)
				(delete-region (line-beginning-position)
					       (1+ (line-end-position)))
				(read-only-mode 1))
			      
					;(move-beginning-of-line nil)
					;(kill-line)
					;(kill-line)
			      ))

	 (local-set-key (kbd "RET") (lambda () (interactive)
				      (switch-to-buffer (get-current-line-buffer))))
	 ;;(read-only-mode 1)
	 (switch-to-buffer list-asyncs-buf-name))))))
