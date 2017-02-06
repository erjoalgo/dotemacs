(defun indent-mode-shift-up (&optional )
  (interactive)
  (indent-mode-shift t))

(defun indent-mode-shift-down (&optional )
  (interactive)
  (indent-mode-shift nil))

(defun indent-mode-set-string (&optional str)
  (interactive "sEnter indent-mode string: ")
  (setq str (or str erjoalgo-indent-mode-string)
	erjoalgo-indent-mode-string str
	erjoalgo-indent-mode-length (length str)
	;;erjoalgo-indent-mode-looking-at (format "$\\|%s" (regexp-quote str))
	erjoalgo-indent-mode-looking-at (regexp-quote str)))

(defun indent-mode-shift (up &optional line-a line-b)
  (interactive)
  (when (and (region-active-p) (not line-a) (not line-b))
    (setq line-a (line-number-at-pos (region-beginning))
	  line-b (line-number-at-pos (region-end))))


  (if (and line-a line-b)
      (let* (
	     (count (1+ (abs (- line-a line-b))))
	     (line-start (min line-a line-b))
	     (line-end (max line-a line-b))
	     (next-line-fun (if (eq line-end (line-number-at-pos (point)))
				'previous-line
			      'next-line))
	     (mark-active nil)
	     )
	(loop for _ from 0 below count
	      do
	      (beginning-of-line)
	      (if up (indent-mode-shift-up) (indent-mode-shift-down))
	      (funcall next-line-fun)
	      )
	)

    (if up
	(if (eq (line-beginning-position) (line-end-position))
	    (insert erjoalgo-indent-mode-string)
	  (save-excursion
	    (beginning-of-line)
	    (insert erjoalgo-indent-mode-string)))
      ;;(when (looking-at (format "$\\|%s" (regexp-quote erjoalgo-indent-mode-char))) (delete-char 1))
      (save-excursion
	(when (>= (- (line-end-position) (line-beginning-position))
		  (length erjoalgo-indent-mode-string))
	  ;;(when (looking-at "$")  (backward-char (length erjoalgo-indent-mode-string)))
	  ;;(when (looking-at "$")  (backward-char (length erjoalgo-indent-mode-string)))
	  (beginning-of-line)
	  (when (looking-at erjoalgo-indent-mode-looking-at)
	    (delete-char erjoalgo-indent-mode-length)
	    )
	  )
	)
      )
    )
  )

(defun indent-mode-shift-up-from-blocks (start-tag end-tag same-line-rep)
  (interactive "senter start tag:
senter end tag:
senter same-line replacement")
  (let* (
	 (regexp (format "%s\\(\\(.\\|[\n]\\)*?\\)%s"
			(regexp-quote start-tag) (regexp-quote end-tag)))
	 )

    (save-excursion
      (while (re-search-forward regexp nil t)
	(let* (
	       (a (line-number-at-pos (match-beginning 1)))
	       (b (line-number-at-pos (match-end 1)))
	       )

	  (if (eq a b)
	      (replace-match
	       (format "%s%s%s" same-line-rep (match-string 1)
		       same-line-rep)
	       t t)

	    (replace-match (match-string 1) t t)
	    (indent-mode-shift t a b)

	    ))
	(y-or-n-p "")
	)
      )
    )
  )

(defun multiply-string (string n)
  (apply 'concat (loop for i from 1 to n
		collect string)))

(defun indent-mode-line-indent-level ()
  (save-excursion
    (beginning-of-line)
    (loop while (looking-at erjoalgo-indent-mode-string)
	  do (forward-char (length erjoalgo-indent-mode-string))
	  count t
	  )
    )
  )

(defun break-to-substrings-max-len (string max-len &optional token-regexp)
  (unless token-regexp (setq token-regexp "."))
  (if (or (not (>= max-len 0 )) )
      (error "")
    (let* (a b c lines)
      (setq a 0
	    c 0
	    )
      (message "before outer cond")
      (while (or (not b) (< b (length string)))
	(message "before inner cond")
	(while (and (< c (length string)) (<= (- c a) max-len))
	  (message "past inner cond")
	  (setq
	   b c
	   c (or
	      (and (string-match token-regexp string b) (match-end 0))
	      (length string))
	   )
	  ;;(when  (error (format "infinite loop for regexp: %s " token-regexp)))
	  )
	(message "done with inner")
	(setq
	 b (if (or (not b) (eq a b) (<= (- c a) max-len)) c b)
	 lines (cons (substring string a b) lines)
	 a b
	 )
	(message "end of inner")
	)
      (unless (eq b c) (setq lines (cons (substring string b c) lines)))
      (reverse lines)
      )
    )
  )

(defun indent-mode-auto-fill (&optional a b)
  (interactive "r")
  (unless (and a b)
    (setq a (point-min)
	  b (point-max)))
  (save-excursion
    (goto-char a)
    (while t
      (let* (line-len indent-level line-prefix broken-lines)

	(setq line-len (- (line-end-position) (line-beginning-position)))

	(when (>= line-len fill-column)
	  (setq indent-level (indent-mode-line-indent-level)
		line-prefix (multiply-string erjoalgo-indent-mode-string indent-level)
		line-start (line-beginning-position)
		line-end (line-end-position)
		broken-lines (break-to-substrings-max-len
			      (buffer-substring-no-properties line-start line-end)
			      fill-column
			      "[^[:space:]]*[[:space:]]+")

		)
	  (delete-region line-start line-end)
	  (insert (car broken-lines) )
	  (mapcar (lambda (sub-line)
		    (insert "\n" (concat line-prefix sub-line))
		    )
		  (cdr broken-lines))
	  )
	(next-logical-line)
	)
      )
    )
  )



(define-minor-mode erjoalgo-indent-mode
  "minor mode to indent up and down"
  nil "-INDENT" (make-sparse-keymap)
  ;;(indent-mode-set-string)

  (if (and (boundp 'erjoalgo-indent-mode-string)
	   erjoalgo-indent-mode-string)
      (indent-mode-set-string erjoalgo-indent-mode-string)
      (call-interactively 'indent-mode-set-string))
  )
(defadvice newline-and-indent (around fix-nli activate)
  (if (null erjoalgo-indent-mode)
      ad-do-it
    (erjoalgo-indent-mode-newline-and-indent)))


(make-variable-buffer-local 'erjoalgo-indent-mode-string)
(set-default 'erjoalgo-indent-mode-string "\t")
(define-key erjoalgo-indent-mode-map (kbd "s-]") 'indent-mode-shift-up)
(define-key erjoalgo-indent-mode-map (kbd "s-[") 'indent-mode-shift-down)
;;(add-hook 'erjoalgo-indent-mode-hook 'indent-mode-set-string)
;;(remove-hook 'erjoalgo-indent-mode-hook 'indent-mode-set-string)
;;(add-hook 'erjoalgo-indent-mode-hook 'indent-mode-set-string)
;;(add-hook 'erjoalgo-indent-mode-hook 'erjoalgo-indent-mode-fix-newline-indent)



(defun indent-mode-line-level (&optional n)
  (let* ((start-line (line-number-at-pos (point))))

    (save-excursion
      (if n (next-line n))
      (beginning-of-line)
      (loop for i from 0
	    while (and
		   (re-search-forward (regexp-quote erjoalgo-indent-mode-string) nil t)
		   (eq (line-number-at-pos (point)) start-line))
	    finally return i))
    )

  )

(defun indent-mode-line-contents (&optional n)
  (let* ((start-line (line-number-at-pos (point))))

    (save-excursion
    (if n (next-line n))
    (beginning-of-line)
    (re-search-forward
     (format "\\(%s\\)*" (regexp-quote erjoalgo-indent-mode-string))
     nil t)
    (buffer-substring-no-properties (point) (line-end-position)))
    )
  )


(defun erjoalgo-indent-mode-newline-and-indent ()
  (let* (
	 (current-level (indent-mode-line-level))
	 (current-contents (indent-mode-line-contents))
	 )

    (if (< 0 (length current-contents))
	(progn
	  (message "curr level is %d " current-level)
	  (newline)
	  (loop for i from 0 below current-level
		do (insert erjoalgo-indent-mode-string)))
      (progn
	(delete-region (line-beginning-position ) (line-end-position))
	(newline)
	)
      )
    )
  )


(defun debian-to-soverflow ()
  (interactive)
  (indent-mode-shift-up-from-blocks "[code]" "[/code]" "`"))

(provide 'erjoalgo-indent-mode)
