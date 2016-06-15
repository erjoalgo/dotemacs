(add-hook-to-modes 'company-mode '(emacs-lisp-mode slime-mode go-mode))
(define-key emacs-lisp-mode-map (kbd "TAB") 'company-complete)
(with-eval-after-load "slime"
(define-key slime-mode-map (kbd "TAB") 'company-complete))

(define-key company-active-map (kbd "<f1>") nil)
(with-eval-after-load "go-mode"
(define-key go-mode-map (kbd "TAB") 'company-complete))

(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(dotimes (i 10)
  (define-key company-active-map (read-kbd-macro (format "<s-%d>" i))
    `(lambda () (interactive) (company-complete-number ,i))))


(global-set-key (kbd "<s-tab>") 'indent-according-to-mode)
