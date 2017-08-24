
(push
 (buffer-major-mode-matcher
  'nxml-mode
  (lambda ()
    (let* ((director-services-url
	    "https://192.168.24.10:18443/director-services")
	   (run-template-prefix
	    ;; "/SystemServices/main?system:runTemplate="
	    (concat director-services-url "/SystemServices/main?system:runDebugger="))
	   (url
	    (->> (buffer-file-name)
		 sanitize-filename
		 (concat run-template-prefix))))
      (firefox-new-tab url))))
 erjoalgo-compile-cmd-for-buffer)

(add-to-list 'auto-mode-alist '("[.]mx[hd]$" . nxml-mode))
