

(defun apply-eshell-alias-no-save (alias &rest definition)
  "basically taken from eshell/alias function, which does not separate applying alias from storing it persistently"
  (if (not definition)
      (setq eshell-command-aliases-list
	    (delq (assoc alias eshell-command-aliases-list)
		  eshell-command-aliases-list))
    (and (stringp definition)
	 (set-text-properties 0 (length definition) nil definition))
    (let ((def (assoc alias eshell-command-aliases-list))
	  (alias-def (list alias
			   (eshell-flatten-and-stringify definition))))
      (if def
	  (setq eshell-command-aliases-list
		(delq def eshell-command-aliases-list)))
      (setq eshell-command-aliases-list
	    (cons alias-def eshell-command-aliases-list)))))


(unless (boundp 'debian-file->string)
  (defun debian-file->string (filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (buffer-string))))

(defun eshell-load-bashrc-aliases ()
  (interactive)
  (mapc (lambda (alias-def)
	  (apply 'apply-eshell-alias-no-save
		 (cdr alias-def)))
	(s-match-strings-all
	 "^alias \\([^=]+\\)='?\\(.+?\\)'?$"
	 (debian-file->string
	  (f-join (getenv "HOME") ".bash_aliases")))))

(add-hook 'eshell-mode-hook 'eshell-load-bashrc-aliases)
