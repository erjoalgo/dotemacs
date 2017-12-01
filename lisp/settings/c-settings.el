(add-hook 'c-mode-hook (lambda () (setf comment-start "// ")))

(make-file-local-variable-flag-toggle-command
 c-ofast-compilation)
