(add-hook 'cider-repl-mode-hook
	  (lambda () (define-key cider-repl-mode-map (kbd "s-h") cider-doc-map)))

(setenv "CIDER_REPL" "true")
