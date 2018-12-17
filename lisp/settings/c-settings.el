(add-hook 'c-mode-hook (lambda () (setf comment-start "// ")))

(def-file-local-toggle-command c-ofast-compilation)
