(add-hook-to-modes 'company-mode '(emacs-lisp-mode slime-mode))
(define-key emacs-lisp-mode-map (kbd "TAB") 'company-complete)
(define-key slime-mode-map (kbd "TAB") 'company-complete)

(define-key company-active-map (kbd "<f1>") nil)
(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(dotimes (i 10)
  (define-key company-active-map (read-kbd-macro (format "<s-%d>" i))
    'company-complete-number))
