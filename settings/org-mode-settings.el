(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-P") 'org-metaup);;move up
  (define-key org-mode-map (kbd "M-N") 'org-metadown);;move down
  
  (define-key org-mode-map (kbd "M-]") 'org-metaright);;promote
  (define-key org-mode-map (kbd "M-[") 'org-metaleft);;demote


  (define-key org-mode-map (kbd "C-M-]") 'org-demote-subtree);;promote
  (define-key org-mode-map (kbd "C-M-[") 'org-promote-subtree);;demote
  
  (define-key org-mode-map (kbd "RET") 'org-meta-return);;insert new
					;use C-j to add text
  
  (define-key org-mode-map (kbd "s-t")
    (lambda () (interactive) (org-todo 1)));;tag TODO
  (define-key org-mode-map (kbd "s-T")
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
  (f-join emacs-top "org" "notes.org"))
(when (file-exists-p notes-file)
  (find-file notes-file))
