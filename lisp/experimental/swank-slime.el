;; https://emacs.stackexchange.com/questions/42257/debugging-a-recalcitrant-eldoc-function

;; fix for slime <v2.21 in emacs 26 (eldoc-message now only takes 1 arg)
(when (version< slime-version "2.21")
  (defun slime-autodoc--async%-fixed (context multilinep doc)
    (cl-destructuring-bind (doc cache-p) doc
      (unless (eq doc :not-available)
        (when cache-p
          (slime-autodoc--cache-put context doc))
        ;; Now that we've got our information,
        ;; get it to the user ASAP.
        (when (eldoc-display-message-p)
          (eldoc-message (slime-autodoc--format doc multilinep))))))

  (advice-add 'slime-autodoc--async% :override #'slime-autodoc--async%-fixed))

;; The above will fix the freezing when moving around when connected to a SWANK server, if you want to fix all instances of the bug you'll need to override a couple more functions:

;; fix for slime <v2.21 in emacs 26 (eldoc-message now only takes 1 arg)
(when (version< slime-version "2.21")
  (defun slime-autodoc--async%-fixed (context multilinep doc)
    (cl-destructuring-bind (doc cache-p) doc
      (unless (eq doc :not-available)
        (when cache-p
          (slime-autodoc--cache-put context doc))
        ;; Now that we've got our information,
        ;; get it to the user ASAP.
        (when (eldoc-display-message-p)
          (eldoc-message (slime-autodoc--format doc multilinep))))))

  (defun slime-autodoc-manually-fixed ()
    "Like autodoc informtion forcing multiline display."
    (interactive)
    (let ((doc (slime-autodoc t)))
      (cond (doc (eldoc-message doc))
            (t (eldoc-message nil)))))

  (defun slime-autodoc-space-fixed (n)
    "Like `slime-space' but nicer."
    (interactive "p")
    (self-insert-command n)
    (let ((doc (slime-autodoc)))
      (when doc
        (eldoc-message doc))))

  (advice-add 'slime-autodoc--async% :override #'slime-autodoc--async%-fixed)
  (advice-add 'slime-autodoc-manually :override #'slime-autodoc-manually-fixed)
  (advice-add 'slime-autodoc-space :override #'slime-autodoc-space-fixed))
