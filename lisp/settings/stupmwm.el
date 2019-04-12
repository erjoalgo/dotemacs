(defun slime-sbcl (arg)
  (interactive "P")
  (require 'slime)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer
	  (find-buffer-by-prefix slime-sbcl-buffer-name)))

    (if (and (null arg) slime-sbcl-buffer)
	(switch-to-buffer slime-sbcl-buffer)
      ;;(add-hook 'slime-connected-hook 'load-compiler-hook)
      (slime))))

(defvar *stumpwm-swank-port* 4005)

(defun slime-stumpwm (&optional arg)
  "switch to a stumpwm slime buffer. if ‘arg' is non-nil, force a new connection"
  (interactive "P")
  (require 'slime)
  (let ((slime-stumpwm-buffer
	 (find-buffer-by-prefix "*slime-repl sbcl")))
    (if (and (not arg) slime-stumpwm-buffer)
	(switch-to-buffer slime-stumpwm-buffer)

      ;;doesn't work since slime-connect does async stuff
      ;;dynamic binding won't reach slime-stumpwm-connection-hook
      (let ((slime-stumpwm-connection-p t))
	(add-hook 'slime-editing-mode-hook
		  'slime-stumpwm-connection-hook)
	(slime-connect "localhost" *stumpwm-swank-port*)))))

(defun slime-stumpwm-connection-hook ()
  (slime-repl-set-package "STUMPWM")
  (remove-hook 'slime-editing-mode-hook
	       'slime-stumpwm-connection-hook))

(defun stumpwm-visible-window-ids (&optional pid)
  "Return a list of the parent process pids of all visible windows
in the current STUMPWM group/workspace."
  (-> (stumpwm-request "/visible-window-pids")
    (s-split "\n")))

(defun stumpwm-message (text &optional color host ports)
  (let ((url-request-data text)
        (url-request-method "post")
        (url-request-extra-headers
         (when color
           `(("STUMPWM-MESSAGE-COLOR" . ,(prin1-to-string color))))))
    (stumpwm-request "/notify")))

(defun stumpwm-request (path &optional host ports)
  (let* ((host (or host "localhost"))
         (ports (or (if (numberp ports) (list ports) ports)
                    '(1959 1960 1961))))
    (loop for port in ports
          as url = (format "http://%s:%s%s" host port path)
          do (url-retrieve
              url
              (lambda (status url)
                (when (and status
                           (not (s-contains-p "connection-failed"
                                              (prin1-to-string status))))
                  (message "ERROR in stumpwm x-service request: %s %s %s"
                           url status (buffer-string))))
              (list url)
              t))))
