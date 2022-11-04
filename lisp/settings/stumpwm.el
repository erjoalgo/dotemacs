(require 'cl-lib)

(defun slime-sbcl (arg)
  "Start an sbcl repl via slime or switch to an existing repl buffer."
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

(defun stumpwm-find-slime-buffer ()
  (find-buffer-by-prefix "*slime-repl sbcl"))

(defun slime-stumpwm (&optional force-new-connection)
  "Switch to a stumpwm slime buffer, or FORCE-NEW-CONNECTION if non-nil."
  (interactive "P")
  (require 'slime)
  (let ((slime-stumpwm-buffer (stumpwm-find-slime-buffer)))
    (if (and (not force-new-connection) slime-stumpwm-buffer)
	(switch-to-buffer slime-stumpwm-buffer)

      ;;doesn't work since slime-connect does async stuff
      ;;dynamic binding won't reach slime-stumpwm-connection-hook
      (let ((slime-stumpwm-connection-p t))
	(add-hook 'slime-editing-mode-hook
		  'slime-stumpwm-connection-hook)
	(slime-connect "localhost" *stumpwm-swank-port*)))))

(defun slime-stumpwm-connection-hook ()
  "Function run upon connecting to slime.")

(defun stumpwm-visible-window-pids ()
  "List parent process pids of all currently-visible stumpwm windows."
  (slime-eval '(STUMPWM::visible-window-pids)))

(defun stumpwm-is-emacs-window-visible ()
  (find (emacs-pid) (stumpwm-visible-window-pids)))

(defun stumpwm-message (text &optional color host ports)
  "Send a message notification TEXT to stumpwm COLOR HOST PORTS."
  (let ((url-request-extra-headers
         (when color
           `(("STUMPWM-MESSAGE-COLOR" . ,(prin1-to-string color))))))
    (stumpwm-request-post "/notify" text)))


(defun stumpwm-request-sync (path &optional host ports)
  "Send a synchronous http request PATH to stumpwm.  HOST PORTS."
  (let* ((host (or host "localhost"))
         (ports (or (if (numberp ports) (list ports) ports)
                    '(1959 1960 1961 1962))))
    (cl-assert (or url-request-data (not (equal url-request-method "post"))))
    (cl-loop for port in ports
          as url = (format "http://%s:%s%s" host port path)
          do (with-elapsed-time
              elapsed-ms
              (url-retrieve
               url
               (lambda (status url)
                 (when (and status
                            (not (s-contains-p "connection-failed"
                                               (prin1-to-string status))))
                   (message "ERROR in stumpwm x-service request: %s %s %s"
                            url status (buffer-string))))
               (list url)
               t)
              (message "%sms for %s %s"
                       elapsed-ms
                       (or (bound-and-true-p url-request-method)
                           "GET")
                       url)))))

(defun stumpwm-request-subprocess (path &optional extra-headers data use-stdin)
  "Send a stumpwm request via a subprocess.  PATH HOST PORTS"
  (let* ((proc-name "*x-service-request*")
         (extra-headers (or extra-headers url-request-extra-headers))
         (data (or data url-request-data))
         (args `(,path
                 ,@(when data
                     (if use-stdin
                         (list "-d" "@-")
                       (list "-d" data)))
                 ,@(cl-loop for (k . v) in extra-headers
                            append (list "-H" (format "%s: %s" k v))))))
    ;; TODO display errors
    (let ((proc
           (apply #'start-process proc-name proc-name "x-service-curl" args)))
      (when (and data use-stdin)
        (process-send-string proc data)
        (while (process-live-p proc)
          (process-send-eof proc)
          (sit-for 1))))))



(defun stumpwm-request (path &rest args)
  "Send a stumpwm request.  PATH ARGS"
  (stumpwm-request-subprocess path))

(defun stumpwm-request-post (path data &optional host ports)
  (cl-assert data)
  ;; use the x-service-curl client instead
  '(let ((url-request-data (encode-coding-string data 'utf-8))
        (url-request-method "post"))
     (stumpwm-request path))
  (stumpwm-request-subprocess path host data t))

(defun stumpwm-browse-url (url)
  (message "browsing %s" url)
  (stumpwm-request-post "/browse" url))

(defun browse-url--stumpwm (orig url &rest args)
  (when (and args (or (car args) (cdr args)))
    (warn "extra browse-url args ignored: %s" args))
  (stumpwm-browse-url url))

(advice-add #'browse-url :around #'browse-url--stumpwm)

(defun stumpwm-clipboard-set (text)
  (stumpwm-request-post "/clipboard" text))

(defun gui-select-text--stumpwm (text &rest args)
  (when args
    (warn "extra interprogram-cut-function args ignored"))
  (unless (zerop (length text))
    (stumpwm-clipboard-set text)))

(defun stumpwm-search-engine-search (query engine-letter)
  (interactive
   (list
    (or (bound-and-true-p search-engine-query)
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "enter search query: "
                   (or (car kill-ring) (x-get-clipboard)))))
    (read-char "enter search engine letter: ")))
  (let ((url-request-extra-headers
         `(("ENGINE-LETTER" . ,(char-to-string engine-letter)))))
    (stumpwm-request-post "/search" query)))

(defun stumpwm-search-engine-search-clipboard ()
  (interactive)
  (let ((search-engine-query
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (car kill-ring))))
    (call-interactively #'stumpwm-search-engine-search)))

(advice-add #'gui-select-text :after #'gui-select-text--stumpwm)

(stumpwm-message (format "connected to emacs on %s" system-name) 'green)

(defun slime-connected-hook-switch-to-stumpwm-package ()
  (condition-case err
      (slime-repl-set-package "STUMPWM")
    (error
     (message "failed to set package to stumpwm: %s" err))))

(add-hook #'slime-editing-mode-hook
          #'slime-connected-hook-switch-to-stumpwm-package)
