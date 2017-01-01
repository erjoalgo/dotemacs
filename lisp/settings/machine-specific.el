(defmacro case-equal (expr-form &rest cases)
  (when cases
    `(if (equal ,expr-form ,(caar cases))
	 (progn ,@(cdar cases))
       (case-equal ,expr-form ,@(cdr cases)))))

(case-equal
  system-name
  ("SFO1212556701M"
   (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
   (setq proxy-mode-proxy company-proxy))

  ("debian-vm"
   (setq proxy-mode-proxy company-proxy)))
