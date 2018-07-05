(defun uaa-set-target (base-url user pass)
  (interactive "sbase url: \nsuser: \nspass: ")
  (setf uaa-base-url base-url
	uaa-token-path token-path
	uaa-user user
	uaa-pass pass))

(defun uaa-token ()
  (let ((id-pass-64 (base64-encode-string (concat uaa-user ":" uaa-pass))))
    (let ((resp (call-process-to-string "curl" uaa-base-url
    )
