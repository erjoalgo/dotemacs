(defun maybe-export-dot-file ()
  (when (or (cl-find major-mode '(dot-mode graphviz-mode))
            (equal (f-ext (buffer-file-name))
		   "dot"))
    (autobuild--run-action (autobuild-dot-to-ps))))

(add-hook 'after-save-hook 'maybe-export-dot-file)
