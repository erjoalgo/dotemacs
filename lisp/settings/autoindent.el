(defun autoindent-indent-whole-buffer ()
  (indent-region (point-min) (point-max)))

(setq autoindent-modes-list
      '(emacs-lisp-mode lisp-mode clojure-mode
                        js-mode html-mode mhtml-mode
                        css-mode
                        nxml-mode
                        ))
;; "Modes on which to auto-indent after save.")

(defun autoindent-save-hook ()
  (when (member major-mode autoindent-modes-list)
    (autoindent-indent-whole-buffer)))

(add-hook 'before-save-hook #'autoindent-save-hook)


;; (define-minor-mode indent-on-save-mode
;;   "Mode to auto-indent on save"
;;   :init-value 0
;;   (funcall
;;    (if indent-on-save-mode #'add-hook #'remove-hook)
;;    #'before-save-hook #'autoindent-indent-whole-buffer))
