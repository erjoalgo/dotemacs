
(with-eval-after-load 'org
  '
  (define-key org-mode-map (kbd "M-]") 'org-metaright);;promote
  (define-key org-mode-map (kbd "M-[") 'org-metaleft);;demote
  (define-key org-mode-map (kbd "RET") 'org-meta-return);;insert new
  
  (define-key org-mode-map (kbd "s-t")
    (lambda () (interactive) (org-todo 1)));;TODO
  (define-key org-mode-map (kbd "s-T")
    (lambda () (interactive) (org-todo 2)));;DONE
  (define-key org-mode-map (kbd "s-d")
    'org-deadline)
  
  (setq org-blank-before-new-entry
					;don't add extra newlines
	'((heading . nil)
	  (plain-list-item . nil))
	;;don't cut the current line
	org-M-RET-may-split-line '((default . nil )))
  )
