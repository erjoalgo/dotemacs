(with-eval-after-load "flycheck"
  (flycheck-define-checker jsl
    "jsl"
    :command
    ("jsl" "-process" source)
    :error-patterns
    ((error line-start (file-name) "(" line "):" (message) line-end))
    :modes (js-mode))
  (add-to-list 'flycheck-checkers 'jsl))
