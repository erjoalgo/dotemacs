;works after (ql:quickload "quicklisp-slime-helper")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq slime-contribs '(slime-fancy))
(require 'slime-autoloads)
(add-hook 'lisp-mode-hook 'slime-mode)

(defun find-buffer-by-starts-with (prefix)
  (loop for buff in (buffer-list)
	thereis (and (s-starts-with-p
		      prefix
		      (buffer-name buff))
		     buff)))

(with-eval-after-load "slime-repl"
  (define-key slime-repl-mode-map (kbd "M-{") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "M-}") 'slime-repl-next-prompt)

  (define-key slime-repl-mode-map (kbd "M-p") (lambda () (interactive)
						(slime-repl-history-replace 'backward nil)))

  (define-key slime-repl-mode-map (kbd "M-n") (lambda () (interactive)
						(slime-repl-history-replace 'forward nil))))

(add-hook 'sldb-hook 'visual-line-mode)
(add-hook 'sldb-hook 'beginning-of-buffer)
(define-key slime-repl-mode-map (kbd "s-h") slime-doc-map)
(setf slime-load-failed-fasl 'never)
