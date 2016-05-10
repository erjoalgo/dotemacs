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
  ("s-0" 'org-ctrl-c-ctrl-c))

(setq org-blank-before-new-entry
      ;;don't add extra newlines
      '((heading . nil)
	(plain-list-item . nil))
      ;;don't cut the current line
      org-M-RET-may-split-line '((default . nil )))

(setq org-startup-folded nil)

(defvar notes-file
  (f-join emacs-top "org"))

(defvar *org-todo-first-todo-line-number* 3)

(defun org-todo-promote-top ()
  (interactive)
  (save-excursion
    (move-line-up (- (line-number-at-pos)
		     *org-todo-first-todo-line-number*))))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "s-q") 'org-todo-promote-top))


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

(add-hook 'after-save-hook (lambda ()
			     (when (and (eq major-mode 'org-mode)
					(equal (f-filename (buffer-file-name))
					       "README.org"))
			       (org-md-export-to-markdown))))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance
      '("crypt"))
(require 'epa)
(setq org-crypt-key "AFF54E1E");; erjoalgo@gmail.com

(define-keys org-agenda-mode-map
  ("s-1" (lambda () (interactive) (org-agenda-todo 1)));;tag TODO
  ("s-2" (lambda () (interactive) (org-agenda-todo 2)));;tag DONE
  )

(when (file-exists-p notes-file)
  (push notes-file org-agenda-files)
  (org-agenda-list nil)
  (setq initial-buffer-choice
	(lambda ()
	  (org-todo-list org-match)))
  (org-todo-list org-match))
