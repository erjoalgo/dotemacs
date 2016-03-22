(require 'cl);;for destructuring-bind
(defun debian-forum-post-wrap-tag-command (tag)
  `(lambda () 
    (interactive)
    (let* (
	   (tag-start (format "[%s]" ,tag))
	   (tag-end (format "[/%s]" ,tag))
	   )
      (if (not (region-active-p))
	  (progn (insert tag-start)
		 (recursive-edit)
		 (insert tag-end))
	(save-excursion
	  (destructuring-bind (a b) (sort (list (region-beginning) (region-end)) '<)
	    (goto-char b)
	    (insert tag-end)
	    (goto-char a)
	    (insert tag-start)
	    ))))
    ))

(define-minor-mode debian-forum-post-mode
  "minor mode to indent up and down"
  nil "-DEBIAN" (make-sparse-keymap))

(loop for (key tag) in
      `((,(kbd "s-c") "code" )
	(,(kbd "s-l") "list")
	
	(,(kbd "s-q") "quote"))
      do (define-key debian-forum-post-mode-map
	      key
	      (debian-forum-post-wrap-tag-command tag)))

;;(define-key debian-forum-post-mode-map "s" nil )
(provide 'debian-forum-post-mode)
