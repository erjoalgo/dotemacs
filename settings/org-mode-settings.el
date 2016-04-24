(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-P") 'org-metaup);;move up
  (define-key org-mode-map (kbd "M-N") 'org-metadown);;move down
  
  (define-key org-mode-map (kbd "M-]") 'org-metaright);;promote
  (define-key org-mode-map (kbd "M-[") 'org-metaleft);;demote


  (define-key org-mode-map (kbd "C-M-]") 'org-demote-subtree);;promote
  (define-key org-mode-map (kbd "C-M-[") 'org-promote-subtree);;demote
  
  (define-key org-mode-map (kbd "RET") 'org-meta-return);;insert new
					;use C-j to add text
  
  (define-key org-mode-map (kbd "s-1")
    (lambda () (interactive) (org-todo 1)));;tag TODO
  (define-key org-mode-map (kbd "s-2")
    (lambda () (interactive) (org-todo 2)));;tag DONE
  (define-key org-mode-map (kbd "s-d")
    'org-deadline)

  (define-key org-mode-map (kbd "s-l")
    'org-insert-link)
  
  (setq org-blank-before-new-entry
					;don't add extra newlines
	'((heading . nil)
	  (plain-list-item . nil))
	;;don't cut the current line
	org-M-RET-may-split-line '((default . nil )))

  (setq org-startup-folded nil))

(defvar notes-file
  (f-join emacs-top "org"))

(when (file-exists-p notes-file)
  (push notes-file org-agenda-files)
  (org-todo-list org-match)
  (org-agenda-list nil))

(defvar *org-todo-first-todo-line-number* 3)
(defun org-todo-promote-top ()
  (interactive)
  (save-excursion
    (move-line-up (- (line-number-at-pos) *org-todo-first-todo-line-number*))))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "s-q") 'org-todo-promote-top))


(setf search-invisible nil)
(setf org-hide-leading-stars t)

(with-eval-after-load "org"
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance
	'("crypt"))
  (require 'epa)
  (setq org-crypt-key "AFF54E1E");; erjoalgo@gmail.com
  )
