(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

(add-to-list 'sql-connection-alist
             `(pgsql-local (sql-product 'postgres)
	                   (sql-port 5432)
	                   (sql-server "localhost")
	                   (sql-user "")
	                   (sql-password "")
	                   (sql-database "")))

(defun sql-connect-maybe-autoselect ()
  (interactive)
  (if (eq (length sql-connection-alist) 1)
      (let* ((conn (car sql-connection-alist))
             (pass (car (alist-get 'sql-password conn)))
             (product (cadar (alist-get 'sql-product conn)))
             (name (car conn)))
        (setq wtf product)
        (when (and pass (eq product 'postgres))
          (setenv "PGPASSWORD" pass))
        (sql-connect name))
    (call-interactively #'sql-connect)))