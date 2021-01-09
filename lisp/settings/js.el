(with-eval-after-load "flycheck"
  (flycheck-define-checker jsl
    "jsl"
    :command
    ("jsl" "-process" source)
    :error-patterns
    ((error line-start (file-name) "(" line "):" (message) line-end))
    :modes (js-mode))
  (add-to-list 'flycheck-checkers 'jsl))

(defun js-autodetect-indent-level ()
  (setq js-indent-level
        (or
         (alist-get 'js-indent-mode file-local-variables-alist)
         (detect-indent-level)
         4)))

(add-hook 'js-mode-hook 'js-autodetect-indent-level)
