(defun maybe-enable-nginx-mode ()
  (when (and (stringp buffer-file-name)
             (string-match
              "/etc/nginx/sites-\\(enabled\\|available\\).*"
              buffer-file-name))
    (nginx-mode)))

(add-hook 'find-file-hook 'maybe-enable-nginx-mode)
