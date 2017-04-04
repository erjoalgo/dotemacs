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
  ("<s-return>" 'browse-url-at-point))

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

(defun maybe-export-to-markdown ()
  (when (and (eq major-mode 'org-mode)
	     (equal (f-filename (buffer-file-name))
		    "README.org"))
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
