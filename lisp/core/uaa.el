(require 's)
(require 'dash)
(require 'dash-functional)

(mapcar (-cut concat <> ": ") '("a" "b" "c"))

(setf uaa-base-url "http://localhost:8080")
;(setf uaa-token-path "/uaa/login")
(setf uaa-token-path "/uaa/oauth/token")
(setf uaa-user "marissa")
(setf uaa-pass "koala")
(defun uaa-set-target (base-url token-path user pass)
  (interactive "sbase url: \nstoken path: \nsuser: \nspass: ")
  '(interactive (->> '("base url" "token path" "user" "pass")
		   (mapcar (-cut concat "s" <> ": "))
		   (s-join "\n")))
  (setf uaa-base-url base-url
	uaa-token-path token-path
	uaa-user user
	uaa-pass pass))


(defun call-process-to-string (program &rest args)
  (message "command is %s" args)
  (with-temp-buffer (apply 'call-process program nil t nil args)
		    (buffer-string)))

(defun uaa-token ()
  (interactive)
  ;;curl -v -XPOST -H"Application/json" -u "cf:"
  ;;--data "username=<username>&password=<password>&client_id=cf&grant_type=password&response_type=token"
  ;;https://login.run.pivotal.io/oauth/token

  (let ((id-pass-64 (base64-encode-string (concat uaa-user ":" uaa-pass))))
    (let ((resp (call-process-to-string "curl" "-sS" "-L"
					(concat uaa-base-url uaa-token-path)
					"-XPOST"
					"-u" "cf:"
					"--data" (->> `(
							("password" ,uaa-pass)
							("client_id" "cf")
							("grant_type" "password")
							("response_type" "token"))
						      (mapcar (apply-partially 's-join "="))
						      (s-join "&"))

					;"-F" (s-join "=" `("username" ,uaa-user))
					;"-F" (s-join "=" `("password" ,uaa-pass))
					;"-F" (s-join "=" `("grant_type" "password"))
					"-H" "Accept: application/json")))
      resp)))

;(defun uaa-curl)
;(concat "-HAuthorization: bearer" )
