(with-eval-after-load "flycheck"
  (flycheck-define-checker jsl
    "jsl"
    :command
    ("jsl" "-process" source)
    :error-patterns
    ((error line-start (file-name) "(" line "):" (message) line-end))
    :modes (js-mode))
  (add-to-list 'flycheck-checkers 'jsl))

(defun js-autodetect-indent-level ()
  (setq js-indent-level
        (or
         (alist-get 'js-indent-mode file-local-variables-alist)
         (detect-indent-level)
         4)))

(add-hook 'js-mode-hook 'js-autodetect-indent-level)

(add-to-list 'auto-mode-alist '("\\.mjs$" . js-mode))

(defun img-src-graphics-make-local ()
  (interactive)
  (let ((dest "./resources"))
    (unless (file-exists-p dest)
      (make-directory dest))
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (let ((case-fold-search t))
                       (re-search-forward "src=\"\\([^\"]+\\(jp?eg\\|png\\|gif\\)\\)\"" nil t))
               as url = (match-string 1)
               as base = (f-filename url)
               as local = (f-join dest base)
               do (progn
                    (if (file-exists-p local)
                      (message "skipping already downloaded file: %s" local)
                    (call-process
                     "curl"
                     nil
                     (get-buffer-create "*img-src-graphics-make-local*")
                     t
                     url "-Lo" local))
                    (replace-match local t t nil 1))))))
