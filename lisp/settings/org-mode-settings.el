(require  'org)

(defmacro define-keys (kmap &rest bindings)
  `(progn ,@(loop for (key cmd) in bindings collect
		  `(define-key ,kmap
		     ,(if (stringp key)
			  `(kbd ,key) ,key)
		     ,cmd))))

(define-keys org-mode-map
  ("M-P" 'org-metaup);;move up
  ("M-N" 'org-metadown);;move down

  ("M-]" 'org-metaright);;promote
  ("M-[" 'org-metaleft);;demote


  ("C-M-]" 'org-demote-subtree);;promote
  ("C-M-[" 'org-promote-subtree);;demote

  ("RET" 'org-meta-return);;insert new
					;use C-j to add text

  ("s-1" (lambda () (interactive) (org-todo 1)));;tag TODO
  ("s-2" (lambda () (interactive) (org-todo 2)));;tag DONE
  ("s-d" 'org-deadline)

  ("s-l" 'org-insert-link)
  ("s-0" 'org-ctrl-c-ctrl-c)
  ;; ("<s-return>" 'browse-url-at-point)
  ("s-s" 'org-insert-last-scrot)
  ("s-[" 'my-org-shift-left)
  ("s-]" 'my-org-shift-right))

(setq org-blank-before-new-entry
      ;;don't add extra newlines
      '((heading . nil)
	(plain-list-item . nil))
      ;;don't cut the current line
      org-M-RET-may-split-line '((default . nil )))

(setq org-startup-folded nil)

(defvar org-top-dir
  (f-expand "~/git/org"))

(defvar *org-todo-first-todo-line-number* 3)

(defun org-todo-promote-top ()
  (interactive)
  (save-excursion
    (move-line-up (- (line-number-at-pos)
		     *org-todo-first-todo-line-number*))))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "s-q") 'org-todo-promote-top)
  (define-keys org-agenda-mode-map
    ("s-1" (lambda () (interactive) (org-agenda-todo 1)));;tag TODO
    ("s-2" (lambda () (interactive) (org-agenda-todo 2)));;tag DONE
    ))


(setf search-invisible nil)
(setf org-hide-leading-stars t)

(defun org-export-mine ()
  (interactive)
  (unless (and (boundp 'org-exporting-mine)
	       org-exporting-mine)
    (let* ((fn (org-html-export-to-html))
	   (url (format "file://%s" (f-full fn)))
	   (org-exporting-mine t))
      (org-latex-export-to-pdf)
      '(browse-url url))))

(defun org-listify-region (a b)
  (interactive "r")
  (save-excursion
    (goto-char a)
    (goto-char (line-beginning-position))
    (setf b (save-excursion (goto-char b)
			    (line-end-position)))
    (while (and (re-search-forward "^\\([-*]+\\)" nil t)
		(< (match-end 0) b))
      (replace-match
       (concat (make-string (1- (length (match-string 1))) 32) "-")))))

(define-key org-mode-map (kbd "M-c") 'org-export-mine)
(setq org-html-validation-link nil)

(make-file-local-variable-flag-toggle-command
 org-treat-as-readme-p)

(defun maybe-export-to-markdown ()
  (when (and (eq major-mode 'org-mode)
	     (or (equal (f-filename (buffer-file-name))
			"README.org")
		 (and (boundp 'org-treat-as-readme-p)
		      org-treat-as-readme-p)))
    (let ((dont-ask-user-about-supersession-threat t))
      (org-md-export-to-markdown)
      (org-html-export-to-html))))

(add-hook 'after-save-hook 'maybe-export-to-markdown)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance
      '("crypt"))
(require 'epa)
(setq org-crypt-key "AFF54E1E");; erjoalgo@gmail.com



(when (file-exists-p org-top-dir)
  (push org-top-dir org-agenda-files)
  '(org-todo-list org-match)

  (setq initial-buffer-choice
	(lambda ()
	  (call-interactively 'org-agenda-list)
	  (get-buffer "*Org Agenda*")))

  '(switch-to-buffer "*Org Agenda*")
  '(delete-other-windows))

(defun org-archive-done-tasks (arg)
  ;;taken from
  ;;http://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  (interactive "P")
  (let ((scope (if arg 'file 'agenda)));('tree 'file 'agenda)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setf org-map-continue-from (outline-previous-heading)))
     "/DONE" scope)
    (when (fboundp 'check-unsaved-buffers)
      (check-unsaved-buffers))))

(defun org-insert-inline-image (caption filename)
  (interactive "senter caption for image: \nfenter image filename: ")
  "     #+CAPTION: This is the caption for the next figure link (or table)
     #+NAME:
     [[./img/a.jpg]]"
  (insert "#+CAPTION: " caption) (newline-and-indent)
  (insert "#+NAME: fig:SED-HR4049") (newline-and-indent)
  (insert (format "[[%s]]" filename)) (newline-and-indent)
  )

(defun file-modification-timestamp (filename)
  (string-to-number
   (shell-command-to-string
    (format "stat '%s' -c '%%Y'" filename))))

(defun last-file-name (files)
  (let* ((files-modified-alist
	  (mapcar (lambda (file)
		    (cons file (file-modification-timestamp file)))
		  files)))
    (caar (sort files-modified-alist (lambda (a b) (> (cdr a) (cdr b)))))))

(defvar auto-scrots-dir
  (f-expand "~/pictures/auto-scrots"))


(defun directory-files-exclude-dots (top)
  (remove-if (lambda (filename) (member filename '("." "..")))
	       (directory-files top)))

(defun last-file-name-in-directory (top)
  (last-file-name (mapcar (lambda (basename) (f-join top basename))
			  (directory-files-exclude-dots top))))

(defun last-scrot-filename ()
  (last-file-name-in-directory auto-scrots-dir))

(defun org-insert-last-scrot (&optional caption)
  "also move last scrot to current directory"
  (interactive (list (read-string "enter caption for image: "
				  (x-get-clipboard))))
  (let* ((filename (last-scrot-filename))
	 (basename (f-filename filename))
	 (basename-noext (f-no-ext basename))
	 (caption (or caption
		      (replace-regexp-in-string "-" " " basename-noext)))
	 (destination "images")
	 (org-image-filename (concat "./"
				     (f-join "./" destination basename))))
    (unless (member (downcase (f-ext filename)) '("jpeg" "jpg" "png" "gif"
						  "svg"))
      (error "last file is not an image: %s" filename))
    (unless (file-exists-p destination)
      (make-directory destination))
    (unless (zerop (call-process "mv" nil t nil "-t" destination filename))
      (error "failed to  move %s to %s" filename default-directory))
    (org-insert-inline-image caption org-image-filename)))


(defun my-org-shift-right (arg a b)
  (interactive "p\nr")
  (save-excursion
    (replace-regexp "^"
		    (make-string (or arg 1) (string-to-char " "))
		    nil a b)))

(defun my-org-shift-left (arg a b)
  (interactive "p\nr")
  (save-excursion
    (replace-regexp (format "^ \\{%d\\}" (or arg 1))
		    ""
		    nil a b)))

(defun org-texinfo-export-to-texinfo-and-html ()
  (interactive)
  (org-texinfo-export-to-texinfo)
  (let ((texi-file (concat
		    (f-base (buffer-file-name)) ".texi")))
    (shell-command (format (concat "texi2html " texi-file
				   " --no-number-sections"
				   " --split node"
				   " --css-include img.css")
			   ))
    ))

(defun my-org-redisplay-inline-images (limit)
  (when (and org-inline-image-overlays
	     (eq 'org-self-insert-command this-command)
	     (eql 91 (char-after)))
    (org-redisplay-inline-images)))

(add-hook 'org-font-lock-hook
	  'my-org-redisplay-inline-images)

(defun org-toggle-list-heading ()
  (interactive)
  (let* ((elm-at-point (org-element-at-point))
	 (at-list-p (case (car elm-at-point)
		      ((item paragraph) t)
		      (headline nil)
		      (t (error "unknown element %s: "
				(car elm-at-point))))))
    (save-excursion
      (save-match-data
	(re-search-backward "^\\([*]+\\|\\([ ]+[-]\\)\\)"
			    (line-beginning-position))
	(let* ((matched-len (length (match-string 0)))
	       (new-string (if at-list-p
			       (make-string matched-len
					    (string-to-char "*"))
			     (concat
			      (make-string (1- matched-len)
					   (string-to-char " ")) "-"))))
	  (replace-match new-string t t))))))
