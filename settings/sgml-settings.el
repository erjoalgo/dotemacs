(with-eval-after-load "sgml-mode"
  (define-key sgml-mode-map (kbd "C-M-f") 'sgml-skip-tag-forward)
  (define-key sgml-mode-map (kbd "C-M-b") 'sgml-skip-tag-backward))
