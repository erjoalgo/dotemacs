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

(defun slime-sbcl ()
  (interactive)
  (require 'slime)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer
	  (find-buffer-by-starts-with slime-sbcl-buffer-name)))

    (if slime-sbcl-buffer
	(switch-to-buffer slime-sbcl-buffer)
      ;;(add-hook 'slime-connected-hook 'load-compiler-hook)
      (slime))))

(defvar *stumpwm-swank-port* 4005)
(defun slime-stumpwm ()
  (interactive)
  (require 'slime)
  (let ((slime-stumpwm-buffer
	 (find-buffer-by-starts-with "*slime-repl sbcl")))
    (if slime-stumpwm-buffer
	(switch-to-buffer slime-stumpwm-buffer)
      (progn
	'(add-hook 'slime-connected-hook
		   'slime-stumpwm-connection-hook)
	(slime-connect "localhost" *stumpwm-swank-port*)))))

(defun slime-stumpwm-connection-hook ()
  '(slime-interactive-eval
    "(swank:set-package \"STUMPWM\")")
  (when (and slime-buffer-connection
	     (= *stumpwm-swank-port*
	   (second (process-contact slime-buffer-connection))))
    (slime-repl-set-package "STUMPWM")))



(with-eval-after-load "slime-repl"
  (define-key slime-repl-mode-map (kbd "M-{") 'slime-repl-previous-prompt)
  (define-key slime-repl-mode-map (kbd "M-}") 'slime-repl-next-prompt)

  (define-key slime-repl-mode-map (kbd "M-p") (lambda () (interactive)
						(slime-repl-history-replace 'backward nil)))

  (define-key slime-repl-mode-map (kbd "M-n") (lambda () (interactive)
						(slime-repl-history-replace 'forward nil)))

  )
;;the point of this was to switch back to command mode
;;after entering sldb choice
;;(add-hook 'sldb-hook 'switch_back_sldb)


(add-hook 'sldb-hook 'visual-line-mode)
(add-hook 'sldb-hook 'beginning-of-buffer)
