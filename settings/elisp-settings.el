

(defun elisp-tab ()
  "Indent or complete."
  ;;based on ielm-tab
  (interactive)
  (if (or (eq (preceding-char) ?\n)
          (eq (char-syntax (preceding-char)) ?\s))
      (indent-for-tab-command)
    (completion-at-point)))
(define-key emacs-lisp-mode-map (kbd "TAB") 'elisp-tab)
