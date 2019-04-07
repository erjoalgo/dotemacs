(defun my-upcase-region (a b &optional downcase)
  "Upcase or downcase region A, B based on DOWNCASE, falling back to the current sexp."
  (interactive "r")
  (unless (region-active-p)
    (setq a (save-excursion (backward-sexp) (point))
          b (point)))
  (let ((fn (if downcase 'downcase-region 'upcase-region)))
    (funcall fn a b)))

(defun my-downcase-region (a b)
  "(my-upcase-region A B t) ."
  (interactive "r")
  (my-upcase-region a b t))


(defun my-next-error (&optional prev)
  (interactive)
  (call-interactively
   (cond
    ((get-buffer "*compilation*") (if prev 'previous-error 'next-error))
    ((and (bound-and-true-p flycheck-mode)
          (or (flycheck-next-error-pos 1)
              (flycheck-next-error-pos -1)))
     (if prev 'flycheck-previous-error 'flycheck-next-error))
    ((bound-and-true-p flymake-mode) (if prev 'flymake-goto-prev-error 'flymake-goto-next-error))
    (t (if prev 'previous-error 'next-error)))))

(defun my-prev-error ()
  (interactive)
  (my-next-error t))

(defun git-hunk-toggle-cmd (dest-indicator)
  `(lambda (a b)
     ,(format "make region hunk lines start with '%s'" dest-indicator)
     (interactive (if (region-active-p)
                      (list (region-beginning) (region-end))
                    (list (line-beginning-position)
                          (line-end-position
                           (when (numberp current-prefix-arg) current-prefix-arg)))))
     (save-excursion
       (goto-char a)
       (while (re-search-forward "^[-+ ]" b t nil)
         (replace-match ,dest-indicator t)))))
