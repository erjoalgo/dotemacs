(let ((filename (expand-file-name "~/git/auth-source-xoauth2")))
  (when (file-exists-p filename)
    (add-to-list 'load-path filename)))
