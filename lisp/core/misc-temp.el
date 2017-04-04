;;minibuffer-local-map

(add-hook 'lisp-mode-hook 'slime-mode)

;(add-hook 'shell-script-mode-hook 'bash_install_buttons)



(defun upcase-last ()
  (interactive)
  (save-excursion
    (upcase-region (point)
		 (progn
		   (backward-sexp)
		   (point)))))

(defun bash-identifier-current-line  ()
  (let ((line
	(buffer-substring-no-properties
	 (point)
	 (line-beginning-position))))
    (and
     (string-match
      "^[[:space:]]*\\([^=]+\\)="
      line)
     (match-string 1 line))))



(defun process-filter-line-buffer (real-filter)
	 (let ((cum-string-sym (gensym "proc-filter-buff"))
	       (newline (string-to-char "\n"))
	       (string-indexof (lambda (string char start)
				 (loop for i from start below (length string)
				       thereis (and (eq char (aref string i))
						    i)))))
	   (set cum-string-sym "")
	   `(lambda (proc string)
	      (setf string (concat ,cum-string-sym string))
	      (let ((start 0) new-start)
		(while (setf new-start
				(funcall ,string-indexof string ,newline start))

		  ;;does not include newline
		  (funcall ,real-filter proc (substring string start new-start))

		  (setf start (1+ new-start)));;past newline

		(setf ,cum-string-sym (substring string start))))))



(defun peek (str start max)
  "peek into str at most `max' characters"
  (substring str start (min (length str) (+ start max))))
