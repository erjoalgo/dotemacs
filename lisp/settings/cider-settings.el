(add-hook 'cider-repl-mode-hook
	  (lambda () (setenv "CIDER_REPL" "true")))

(defun clojure-current-namespace ()
  (when (eq major-mode 'clojure-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^(ns \\(.*\\)")
        (match-string 1)))))

(defvar clojure-known-namespaces nil)

(defun cider-add-to-know-namespaces ()
  (when-let ((ns (clojure-current-namespace)))
    (cl-pushnew ns clojure-known-namespaces  :test #'equal)))

(add-hook 'clojure-mode-hook #'cider-add-to-know-namespaces)

(defun cider-maybe-switch-to-current-ns ()
  (when-let ((ns (and clojure-known-namespaces
                      (selcand-select clojure-known-namespaces
                                      :prompt "select among known namespaces: "
                                      :autoselect-if-single t)))
             (ns-form (format "(ns %s)" ns)))
    (if (eq major-mode 'cider-repl-mode)
        (progn (insert ns-form)
               (cider-repl-return))
      (cider-interactive-eval ns-form))))

(add-hook 'cider-connected-hook #'cider-maybe-switch-to-current-ns)
(add-hook 'cider-repl-mode #'cider-maybe-switch-to-current-ns)
