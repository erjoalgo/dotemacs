(require 'f)
(add-to-list 'load-path
	      ;;(f-join emacs-top "libs" "slime-2.17"))
	      ;;stumpwm's swank only works with 2.14
	     (f-join emacs-top "libs" "slime-2.14"))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(defun find-buffer-by-starts-with (prefix)
  (loop for buff in (buffer-list)
	thereis (and (s-starts-with-p
		      prefix
		      (buffer-name buff))
		     buff)))

(defun slime-sbcl ()
  (interactive)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer (find-buffer-by-starts-with slime-sbcl-buffer-name)))

    (if slime-sbcl-buffer
	(switch-to-buffer slime-sbcl-buffer)
      ;;(add-hook 'slime-connected-hook 'load-compiler-hook)
      (slime))))

(defvar *stumpwm-swank-port* 4005)
(defun slime-stumpwm ()
  (interactive)
  (let ((slime-stumpwm-buffer
	 (find-buffer-by-starts-with "*slime-repl sbcl")))
    (if slime-stumpwm-buffer
	(switch-to-buffer slime-stumpwm-buffer)
      (slime-connect "localhost" *stumpwm-swank-port*))))

  
(require 'slime)
