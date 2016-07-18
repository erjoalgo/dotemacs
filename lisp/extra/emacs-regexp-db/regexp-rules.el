(defun regexp-rules-add-new (name from to )
  (interactive (list
   (regexp-rules-read-rule-name)
   (read-string "enter from regexp: " (query-replace-descr (car query-replace-defaults)))
   (read-string "enter replacement: " (query-replace-descr (cdr query-replace-defaults)))
   ))
  (sexp-db-put *REGEXP-RULES-FN*
	       name (cons from to)))

(defun regexp-rules-candidate-names ()
  (mapcar 'car (sexp-db-alist-from-fn *REGEXP-RULES-FN*)))

(defun regexp-rules-read-rule-name ()
  (setq regexp-rules-last-entered-name
	(completing-read "enter name of (new)? rule: "
			  (regexp-rules-candidate-names)
			  nil nil 
			  regexp-rules-last-entered-name
			  nil )))

  

(defun regexp-rules-is-seq (rule)
      (eq 'SEQ (car rule)))

(defun regexp-rules-seq-from-rule (rule)
      (cdr rule))

(defun regexp-rules-flatten-seq (seq)
  (apply 'append (mapcar (lambda (name)
		    (let* ((rule (sexp-db-get *REGEXP-RULES-FN* name )))
		      (if (regexp-rules-is-seq rule)
			  (regexp-rules-flatten-seq (regexp-rules-seq-from-rule rule))
			(cons name nil ))
		      ))
		  seq)))
  
		      
		      
(defun regexp-rules-add-new-seq (name seq)
  (interactive
   (list (regexp-rules-read-rule-name)
	 (let* (el list cands)
	   (setq cands (cons "" (regexp-rules-candidate-names)))
	   (while (not (equal "" (setq el (completing-read
					   "enter name of rule(RET to finish): "
					   cands nil t))))
	     (setq list (cons el list)))
	   (reverse list))))
  (let* ((flat-seq (regexp-rules-flatten-seq seq)))
    
    (sexp-db-put *REGEXP-RULES-FN*
		 name (cons 'SEQ flat-seq)))
  ;;seq is a list of valid rule names, to be applied consecutively
  )
  

(defun regexp-rules-apply (name &optional ab)
  (interactive
   (list (regexp-rules-read-rule-name)))
  (let* (
	 (ab (or ab (if (region-active-p)
		 (cons (region-beginning) (region-end))
	       (cons (point-min) (point-max)))))
	 (rule (sexp-db-get *REGEXP-RULES-FN* name ))
	 )
    ;(destructuring-bind (from . to)
    (if (regexp-rules-is-seq rule)
	(progn
	  (mapc (lambda (name)
		  (regexp-rules-apply name ab)
		  ;(y-or-n-p (format "applied %s. continue? " name))
		  ) (cdr rule)))
	(let* ((from (car rule))
	   (to (cdr rule)))
      (setq to-rep (query-replace-compile-replacement to t))
      ;(wtf to to-rep)
      (replace-regexp from to-rep nil
		      (car ab) (cdr ab))
      ))
    )
  )



'(defun regexp-rules-load ()
  (if (not (file-exists-p *REGEXP-RULES-FN*))
      (shell-command (format "touch '%s'" *REGEXP-RULES-FN*)))
  
  (let* ((text (get-string-from-file *REGEXP-RULES-FN*)))
    (setq regexp-rules-alist
	(mapcar (lambda (line)
		  (if (not (equal "" line))
		      (destructuring-bind (name from to)
		      (split-string line "\t")
		      (cons (read name) (cons (read from) (read to))))))
		(split-string text "\n"))))
  )

(defun regexp-rules-load ()
  (interactive)
  (sexp-db-load *REGEXP-RULES-FN*)
  )



(defun regexp-rules-edit-file ()
  (interactive)
  (find-file *REGEXP-RULES-FN*))

(defun regexp-rules-message-rule (name)
  (interactive (list (regexp-rules-read-rule-name)))
  (message "%s-->%s" name (sexp-db-get *REGEXP-RULES-FN* name)))

(defun regexp-rules-define-map ()
  (setq regexp-rules-map (make-sparse-keymap))
  (mapcar (lambda (k-sym)
	    (define-key regexp-rules-map (kbd (car k-sym))  (cdr k-sym)))
	  '(
	    ("l" . regexp-rules-load)
	    ("L" . (lambda () (interactive)(load-file (path-join dotemacs "regexp-rules/regexp-rules.el"))))
	    ("z" . regexp-rules-apply)
	    ("a" . regexp-rules-add-new)
	    ("A" . regexp-rules-add-new-seq)
	    ("o" . regexp-rules-edit-file)
	    ("m" . regexp-rules-message-rule)
	    ("r" . query-kill-regexp)
	    
	    ))
  )


(defun regexp-rules-init ()
  (load-file (path-join dotemacs "regexp-rules/sexp-db.el"))
  ;(load-file "sexp-db.el")
  (setq *REGEXP-RULES-FN* (path-join dotemacs "regexp-rules/regexp-rules-data.lisp")
	regexp-rules-last-entered-name nil )
  (sexp-db-load *REGEXP-RULES-FN*)

  (regexp-rules-define-map)
  (define-key command-mode-map (kbd "M-r") regexp-rules-map))

(regexp-rules-init)
