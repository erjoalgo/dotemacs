;;; .emacs --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

(defvar emacs-top
  (file-name-directory (file-truename user-init-file))
  "Find the LISP src directory.")

(defun add-hook-fn-to-modes (hook-fn mode-sym-list)
  "Attach HOOK-FN to the mode hook of each mode in MODE-SYM-LIST."
  (dolist (mode-sym mode-sym-list)
    (let ((hook-sym (intern (concat (symbol-name mode-sym) "-hook"))))
      (add-hook hook-sym hook-fn)
      (when (derived-mode-p mode-sym)
        (funcall hook-fn)))))

(defmacro safe-funcall (fn-args)
  "Demote errors in the FN-ARGS funcall into warnings."
  (destructuring-bind (fn . args) fn-args
  (let ((err-sym (gensym "err-")))
    `(condition-case ,err-sym (,fn ,@args)
       (error
        (warn "WARNING: error calling %s: %s" (list ',fn ,@args)  ,err-sym)
        (when debug-on-error
          (error ,err-sym)))))))

(dolist (dir '("libs" "core" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(mapc (apply-partially 'safe-funcall 'require)
      '(f
        goto-last-change
        quick-yes
        cl-lib
        cl
        zoom-global
        isearch-fast-reverse
        my-emacs-settings
        proxy-mode
        plusx
        dash
        dash-functional
        dedicated
        buttons))

(defun ensure-packages-exist (packages)
  "Ensure each package in PACKAGES is installed."
  (package-initialize)
  (loop with already-refreshed = nil
        for package in packages
        unless (package-installed-p package)
        do
        (progn
          (unless already-refreshed
            (add-to-list ' package-archives
                         '("melpa" . "https://melpa.org/packages/"))

            (safe-funcall (package-refresh-contents))
	    (setf already-refreshed t))
          (safe-funcall (package-install package)))))

  (safe-funcall 'require 'company)
  (require 'dash)
  (require 'dash-functional))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (feature &rest forms)
    `(eval-after-load ,feature '(progn ,@forms))))

(loop with top = (f-join emacs-top "libs")
      for lib-dir in (directory-files top)
      as fn = (f-join top lib-dir)
      if (file-directory-p fn)  do
      (add-to-list 'load-path fn))

(defun current-time-ms ()
  (destructuring-bind (_ secs usecs __) (current-time)
    (+ (* 1000 secs) (/ usecs 1000))))

(defmacro with-elapsed-time (elapsed-time-ms-var form &rest body)
  (let ((start-time-sym (gensym "start-time"))
        (time-now-form `(current-time-ms)))
    `(let ((,start-time-sym ,time-now-form))
       ,form
       (let ((,elapsed-time-ms-var (- ,time-now-form ,start-time-sym)))
         ,@body))))

(defun load-rec (top-dir)
  "Load *.el files under TOP-DIR recursively."
  (loop for file in (directory-files top-dir)
        as filename-abs = (f-join top-dir file)
        if (file-directory-p filename-abs) append
        (unless (member file '("." ".."))
          (load-rec filename-abs))
        else when (and (file-regular-p filename-abs)
                       (equal "el" (f-ext filename-abs)))
        collect
        (with-elapsed-time ms
                           (safe-funcall (load filename-abs))
                           (cons ms filename-abs) )))

(let ((default-directory emacs-top))
  (let ((load-times
         (loop for dir in (list
                           "core" "private" "settings" "extra"
                           "extra-dirs"
                           "experimental"
                           (expand-file-name "~/private-data/emacs-lisp")
                           (expand-file-name "~/private-data-one-way/emacs-lisp"))
               when (file-exists-p dir)
               append (load-rec dir))))
    (sort load-times (lambda (a b) (< (car a) (car b))))
    (loop for (ms . file) in load-times
          do (message "%dms to load %s" ms file))))
