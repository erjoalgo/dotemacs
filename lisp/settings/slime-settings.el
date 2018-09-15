;works after (ql:quickload "quicklisp-slime-helper")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq slime-contribs '(slime-fancy))
(require 'slime-autoloads)
(add-hook 'lisp-mode-hook 'slime-mode)

(defun find-buffer-by-starts-with (prefix)
  (loop for buff in (buffer-list)
	thereis (and (s-starts-with-p
		      prefix
		      (buffer-name buff))
		     buff)))

(defun slime-sbcl (arg)
  (interactive "P")
  (require 'slime)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer
	  (find-buffer-by-starts-with slime-sbcl-buffer-name)))

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
	 (find-buffer-by-starts-with "*slime-repl sbcl")))
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

(with-eval-after-load "slime-repl"
  (define-key slime-repl-mode-map (kbd "M-{") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "M-}") 'slime-repl-next-prompt)

  (define-key slime-repl-mode-map (kbd "M-p") (lambda () (interactive)
						(slime-repl-history-replace 'backward nil)))

  (define-key slime-repl-mode-map (kbd "M-n") (lambda () (interactive)
						(slime-repl-history-replace 'forward nil))))

(add-hook 'sldb-hook 'visual-line-mode)
(add-hook 'sldb-hook 'beginning-of-buffer)
(define-key slime-repl-mode-map (kbd "s-h") slime-doc-map)
(setf slime-load-failed-fasl 'never)

(defun stumpwm-eval (form)
  "example: (stumpwm-eval '(STUMPWM::message \"hello\"))"
  ;; TODO save-window-excursion won't work since connection is async
  (save-window-excursion
    ;; TODO use slime connections as indicator, not existence of buffer
    (slime-stumpwm)
    (slime-rex ()
        (`(swank-repl:listener-eval ,(prin1-to-string form))
         (slime-lisp-package))
      ((:ok result))
      ((:abort condition)))))

(defun stumpwm-visible-window-pids ()
  "return a list of the parent process pids of all visible windows in the current STUMPWM group/workspace"
  ;; emacs' frame-visible-p does not seem to account for another window raised on top of the emacs frame
  (slime-eval '(CL:mapcar 'STUMPWM::window-pid
                          (CL:remove-if-not 'STUMPWM::window-visible-p
                                            (STUMPWM::group-windows (STUMPWM:current-group))))))

(defun stumpwm-message (text)
  (let ((text-cl-escaped (replace-regexp-in-string
                          "[~]+"
                          (lambda (match)
                            (concat match
                                    (when (oddp (length match)) "~")))
                          text)))
    (slime-eval `(STUMPWM::message ,text-cl-escaped))))
