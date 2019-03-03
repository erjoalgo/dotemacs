(defmacro push-last (elt place)
  `(setf ,place (append ,place (list ,elt))))

(autobuild-define-rule
 predix-appengine-runtemplate
 (nxml-mode)
 (let* ((director-services-url
	 "https://192.168.24.10:18443/director-services")
	(run-template-prefix
	 ;; "/SystemServices/main?system:runTemplate="
	 (concat director-services-url "/SystemServices/main?system:runDebugger="))
	(url
	 (->> (buffer-file-name)
	      sanitize-filename
	      (concat run-template-prefix))))
   (apply-partially #'browser-new-tab url)))

(add-to-list 'auto-mode-alist '("[.]mx[hd]$" . nxml-mode))

(defun bitstew-hard-link-template-to-test-dir (filename &optional test-dir)
  (interactive (list (buffer-file-name nil)))
  (setf test-dir (or test-dir "/repos/grid-director/test/"))
  (shell-command-to-string-message
   (format "sudo ln %s %s" filename test-dir))
  (let ((find-file-suppress-same-file-warnings t)
	(find-file-existing-other-name nil))
    (find-file (f-join test-dir (f-filename filename)))))

(defun jmx-to-xml ()
  (interactive)
  (multi-regexp-replace '(("&lt;" "<")
			  ("&gt;" ">")
			  ("&quot;" "\"")
			  ("&#xd;" "")
			  ("&apos;" "'")
			  ("__UUID()" "nQeP2xMTsxz0V")
			  ("__time(yyyy-MM-dd'T'HH:mm:ssZ)};"
			   "2017-08-28T0:17:38UTC")))

  (goto-char (point-min))
  (while (re-search-forward "[$]{.*?}" nil t)
    (replace-match (int-to-string (random 900)))))
