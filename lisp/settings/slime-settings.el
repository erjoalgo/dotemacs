(setq inferior-lisp-program "sbcl")

(with-eval-after-load "slime"
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
  (setf slime-load-failed-fasl 'never)

  (defun slime-killall ()
    (interactive)
    (kill-buffers-matching-regexp "[*]\\(slime-repl\\|inferior-lisp\\|slime\\)")
    (slime-disconnect-all)))

(defmacro slime-eval-async-and-message (form)
  "slime-eval-async-and-message FORM."
  `(slime-eval-async ,form
    (lambda (result) (message "%s" result))))

(defun slime-unexport-all-symbols (package)
  "Unexport all symbols in PACKAGE."
  (interactive (list
                (slime-read-package-name "from package: "
                                         (slime-current-package))))
  (slime-eval-async-and-message `(dbg:unexport-all-symbols ,package)))

(defun slime-unintern-shadowing-symbols (package)
  "Unintern the symbol given with SYMBOL-NAME PACKAGE."
  (interactive
   (list
    (slime-read-package-name "from package: "
                             (slime-current-package))))
  (slime-eval-async-and-message `(dbg:unintern-shadowing-symbols ,package)))

(defun process-ignore-on-exit (regexp)
  (cl-loop for proc in (process-list)
           when (s-matches-p regexp (process-name proc))
           do
           (progn (message "disabling query-on-exit for '%s'" proc)
                  (set-process-query-on-exit-flag proc nil))))

(defun slime-ignore-processes-on-exit (&rest r)
  (process-ignore-on-exit "SLIME"))

(advice-add #'save-some-buffers :before #'slime-ignore-processes-on-exit)

(defun asdf-setup-link-farm ()
  ;; Use instructions in
  ;; https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems.html
  ;; to help ASDF locate the correct swank.asd
  ;; corresponding to this emacs' slime version
  (let ((42-link-farm-filename
         (expand-file-name
          "~/.config/common-lisp/source-registry.conf.d/42-asd-link-farm.conf"))
        (link-farm-directory (expand-file-name "~/.asd-link-farm/")))
    (unless (file-exists-p 42-link-farm-filename)
      (make-directory config t)
      (with-current-buffer (find-file-noselect 42-link-farm-filename)
        (insert (prin1-to-string `(:tree ,link-farm-directory)))))
    (make-directory link-farm-directory t)
    link-farm-directory))

(defun asdf-link-system (asdf-filename)
  ;; Use instructions in
  ;; https://asdf.common-lisp.dev/asdf/Configuring-ASDF-to-find-your-systems.html
  ;; to help ASDF locate the correct swank.asd
  ;; corresponding to this emacs' slime version
  (cl-assert (file-exists-p asdf-filename))
  (let* ((link-farm-directory (asdf-setup-link-farm))
         (link-name (f-join link-farm-directory (f-base asdf-filename))))
    (make-symbolic-link (f-dirname asdf-filename)
                        link-name)))

(defun asdf-system-installed-p (system-name)
  (let ((output-buffer "*asdf-system-installed-p*"))
    (unless
        (zerop
         (call-process inferior-lisp-program nil
                       output-buffer nil
                       "--eval"
                       (prin1-to-string `(ql:quickload ,system-name))
                       "--non-interactive"))
      (error "system %s not installed. see buffer %s for details"
             system-name output-buffer))))

(defun slime-link-swank-to-asdf-link-farm ()
  (let* ((slime-elc (symbol-file 'slime))
         (swank-asd-filename (f-join (f-dirname slime-elc) "swank.asd")))
  (asdf-link-system swank-asd-filename)
  (asdf-system-installed-p "swank")))
