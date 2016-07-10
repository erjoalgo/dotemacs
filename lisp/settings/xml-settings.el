(with-eval-after-load "nxml-mode"
  (setf nxml-sexp-element-flag t)
  (define-key nxml-mode-map (kbd "C-M-f") 'nxml-forward-balanced-item)
  (define-key nxml-mode-map (kbd "C-M-b") (lambda () (interactive)
					    (nxml-forward-balanced-item -1))))
