(require 'cl-lib)

(defun slime-sbcl (force-start)
  "Start an sbcl repl via slime or switch to an existing repl buffer."
  (interactive "P")
  (require 'slime)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer
	  (find-buffer-by-prefix slime-sbcl-buffer-name)))
    (if (and (null force-start) slime-sbcl-buffer)
	(switch-to-buffer slime-sbcl-buffer)
      ;;(add-hook 'slime-connected-hook 'load-compiler-hook)
      (setq slime-auto-package nil)
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
        ;; TODO avoid use of global variable
        (setq slime-auto-package "STUMPWM")
	(slime-connect "localhost" *stumpwm-swank-port*)))))


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

(defun x-service-curl (path headers &optional data use-stdin)
  (let* ((proc-name "*x-service-request*")
         (args `(,path
                 ,@(when data
                     (if use-stdin (list "-i")
                       (list "-d" data)))
                 ,@(cl-loop for (k . v) in headers
                            append (list "-H" (format "%s:%s" k v)))))
         (old-max-point (with-current-buffer (get-buffer-create "*x-service-request*")
                          (point-max))))
    (let ((proc
           (apply #'start-process proc-name proc-name "x-service-curl" args)))
      (set-process-sentinel proc
                            `(lambda (proc change)
                               (when (s-starts-with-p "exited abnormally" change)
                                 (warn "x-service failed: %s %s"
                                       change
                                       (with-current-buffer ,proc-name
                                         (buffer-substring ,old-max-point (point-max)))))))
      (when (and data use-stdin)
        (process-send-string proc data)
        (process-send-eof proc)))))

(defun stumpwm-request-subprocess (path &optional extra-headers data use-stdin)
  "Send a stumpwm request via a subprocess.  PATH HOST PORTS"
  (let* ((extra-headers (or extra-headers url-request-extra-headers))
         (data (or data url-request-data)))
    (x-service-curl path extra-headers data use-stdin)))


(defun stumpwm-request (path &rest args)
  "Send a stumpwm request.  PATH ARGS"
  (stumpwm-request-subprocess path))

(defun stumpwm-request-post (path data &optional host ports)
  (cl-assert data)
  ;; use the x-service-curl client instead
  '(let ((url-request-data (encode-coding-string data 'utf-8))
         (url-request-method "post"))
     (stumpwm-request path))
  (stumpwm-request-subprocess path host data))

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

(defun stumpwm-url-launcher-put (url alias)
  (let ((url-request-extra-headers
         `(("ALIAS" . ,alias)
           ("URL" . ,url))))
    (stumpwm-request-post "/url-launcher-put" "")))

(defun stumpwm-url-launcher-put-local-file (filename alias)
  (interactive
   (let*
       ((filename (cond
                   ((eq major-mode 'dired-mode)
                    (expand-file-name
		     (or (dired-file-name-at-point)
		         default-directory)))
                   (t (buffer-filename (current-buffer)))))
        (_ (cl-assert filename))
        (alias (read-string (format "enter alias for file %s: " filename))))
     (list filename alias)))
  (stumpwm-url-launcher-put
   (format "file://%s" filename)
   alias))

(defun stumpwm-raise (regexp)
  (x-service-curl "/raise-window" `(("REGEXP" . regexp))))

(advice-add #'gui-select-text :after #'gui-select-text--stumpwm)

(defvar slime-auto-package nil
  "If non-nil, switch to the given package upon connecting to the inferior lisp.")

(defun slime-connected-hook-switch-to-auto-package ()
  '(when slime-auto-package
     (condition-case err
         (slime-repl-set-package slime-auto-package)
       (error
        (message "failed to set package to %s %s" sbcl-auto-set-package err)))))

(add-hook #'slime-editing-mode-hook
          #'slime-connected-hook-switch-to-auto-package)

(stumpwm-message (format "connected to emacs on %s" system-name) 'green)
