(add-hook 'cider-repl-mode-hook
	  (lambda () (setenv "CIDER_REPL" "true")))

(defun clojure-current-namespace ()
  (when (eq major-mode 'clojure-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^(ns \\(.*\\)")
        (match-string 1)))))

(defun cider-maybe-switch-to-current-ns ()
  (when-let ((ns (clojure-current-namespace)))
    (cider-interactive-eval (format "(ns %s)" ns))))

(add-hook 'cider-connected-hook #'cider-maybe-switch-to-current-ns)
