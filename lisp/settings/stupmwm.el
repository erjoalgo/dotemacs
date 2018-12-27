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

(defun stumpwm-eval (form &optional on-ok on-abort)
  "Eval FORM on stumpwm.
ON-OK is a function that is invoked upon execution with
FORM's value as the sole argument.
ON-ABORT is a function that is invoked on error with
the error condition as the sole argument.

Example: (stumpwm-eval '(STUMPWM::message \"hello\"))."
  ;; TODO save-window-excursion won't work since connection is async
  (save-window-excursion
    ;; TODO use slime connections as indicator, not existence of buffer
    (slime-stumpwm)
    (lexical-let ((on-ok on-ok)
                  (on-abort on-abort))
      (slime-rex ()
          (`(swank-repl:listener-eval ,(prin1-to-string form))
           "STUMPWM")
        ;; todo not working: result always nil
        ((:ok result) (when on-ok (funcall on-ok result)))
        ((:abort condition) (when on-abort (funcall on-ok condition)))))))

(defun stumpwm-visible-window-ids (&optional pid)
  "Return a list of the parent process pids of all visible windows
in the current STUMPWM group/workspace."
  ;; emacs' frame-visible-p does not seem to account
  ;; for another window raised on top of the emacs frame
  '(slime-eval `(CL:mapcar
                ',(if pid
                   'STUMPWM::window-pid
                   'STUMPWM::window-id)
                (CL:remove-if-not
                 'STUMPWM::window-visible-p
                 (STUMPWM::group-windows (STUMPWM:current-group)))))
  nil)

(defun stumpwm-message (text &optional color host ports)
  (let* ((host (or host 'local))
         (ports (or (if (numberp ports) (list ports) ports)
                    '(1959 1960 1961 1962)))
         (colored (if color (stumpwm-color text color)
                    text))
         (proc ))
    (loop for port in ports
          with success-count = 0
          do
          (incf success-count
                (condition-case err
                    (let ((proc
                           (make-network-process :host host
                                                 :name "*stumpwm-msg*"
                                                 :service port)))
                      (process-send-string proc colored)
                      (process-send-eof proc)
                      (delete-process proc)
                      1)
                  (error 0)))
          finally (when (zerop success-count)
                    (error "unable to send to any of the ports: %s" ports)))))

(defun stumpwm-auto-doc-el (in out)
  (interactive
   (let ((buffer-file-name (current-buffer)))
     (list filename (concat filename ".texi"))))

  (slime-eval `(STUMPWM::generate-manual
                :in ,(f-full in)
                :out ,(f-full out))))

(defvar stumpwm-colors
  '(black
    red
    green
    yellow
    blue
    magenta
    cyan
    white)
  "Stumpwm colors.")

(defun stumpwm-color (msg color)
  (or
   (loop for clr in stumpwm-colors
         for idx from 0
         thereis (when (eq color clr)
                   (format "^%d%s^*" idx msg)))
   (error "no such color: %s. options: %s" color stumpwm-colors)))
