(defmacro case-equal (expr-form &rest cases)
  (when cases
    `(if (member ,expr-form (list ,@(caar cases)))
	 (progn ,@(cdar cases))
       (case-equal ,expr-form ,@(cdr cases)))))

(case-equal
  system-name
 (("SFO1212556701M" "sfo1212556701m.attlocal.net")
  (push "/usr/local/bin" exec-path)
   (setq proxy-mode-proxy company-proxy))

 (("debian-vm")
   (setq proxy-mode-proxy company-proxy)))
