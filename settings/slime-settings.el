(require 'f)
(add-to-list 'load-path
	      ;;(f-join emacs-top "libs" "slime-2.17"))
	      ;;stumpwm's swank only works with 2.14
	     (f-join emacs-top "libs" "slime-2.14"))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(defun sbcl-slime ()
  (interactive)
  (let* ((slime-sbcl-buffer-name "*slime-repl sbcl")
	 (slime-sbcl-buffer (loop for buff in (buffer-list)
				  thereis (and (s-starts-with-p
						slime-sbcl-buffer-name
						(buffer-name buff))
					       buff))))

    (if slime-sbcl-buffer
	(switch-to-buffer slime-sbcl-buffer)
      ;;(add-hook 'slime-connected-hook 'load-compiler-hook)
      (slime))))
