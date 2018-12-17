(with-eval-after-load "slime"

  (defvar slime-helper-filename nil "Quicklisp slime helper")

  (setq slime-helper-filename (expand-file-name "~/quicklisp/slime-helper.el"))

  (if (not (file-exists-p slime-helper-filename))
      ;; must (ql:quickload "quicklisp-slime-helper")
      (error "slime helper %s does not exist. hint: %s"
	    slime-helper-filename
	    '(ql:quickload 'quicklisp-slime-helper))

    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl")

    (setq slime-contribs '(slime-fancy))
    (require 'slime-autoloads)
    (add-hook 'lisp-mode-hook 'slime-mode)

    (defun find-buffer-by-prefix (prefix)
      (cl-loop for buff in (buffer-list)
	    thereis (and (s-starts-with-p
			  prefix
			  (buffer-name buff))
			 buff)))



    (add-hook 'sldb-hook 'visual-line-mode)
    (add-hook 'sldb-hook 'beginning-of-buffer)
    (define-key slime-repl-mode-map (kbd "s-h") slime-doc-map)
    (setf slime-load-failed-fasl 'never)

    (defun slime-killall ()
      (interactive)
      (kill-buffers-matching-regexp "[*]\\(slime-repl\\|inferior-lisp\\|slime\\)")
      (slime-disconnect-all))))
