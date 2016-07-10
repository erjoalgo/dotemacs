(with-eval-after-load "text-mode"
  (define-key text-mode-map (kbd "M-c") 'with-editor-finish)
  (define-key text-mode-map (kbd "C-x #") nil))

(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "M-c") 'server-edit)
  (define-key diff-mode-map (kbd "C-x #") nil))


