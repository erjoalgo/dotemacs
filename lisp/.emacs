;;; .emacs --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

(add-to-list 'load-path (expand-file-name "~/git/babel/"))

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
  (cl-destructuring-bind (fn . args) fn-args
    (let ((err-sym (gensym "err-")))
      `(condition-case ,err-sym (,fn ,@args)
         (error
          (warn "WARNING: error calling %s: %s" (list ',fn ,@args)  ,err-sym)
          (when debug-on-error
            (error ,err-sym)))))))

(dolist (dir '("libs" "core" "extra"))
  (add-to-list 'load-path
	       (concat emacs-top dir)))

(require 'package)
(require 'cl-lib)

(defun ensure-packages-exist (packages)
  "Ensure each package in PACKAGES is installed."
  (cl-loop with already-refreshed = nil
           for package in packages
           unless (package-installed-p package)
           do
           (progn
             (unless already-refreshed
               (cl-pushnew '("melpa" . "https://melpa.org/packages/")
                           package-archives
                           :test #'equal)
               (safe-funcall (package-refresh-contents))
	       (setf already-refreshed t))
             (safe-funcall (package-install package)))))

(package-initialize)
(ensure-packages-exist
 '(company
   legalese
   magit
   dash
   dash-functional
   go-mode
   calfw
   calfw-gcal
   java-imports
   bbdb
   nginx-mode
   dedicated
   buttons
   flycheck
   babel
   slime
   cider
   websocket
   autobuild))

(require 'f)

(push (expand-file-name "~/git/autobuild/common-rules") load-path)
(require 'autobuild-common-rules)

(dolist (feature
         '(f
           goto-last-change
           quick-yes
           cl-lib
           zoom-global
           isearch-fast-reverse
           my-emacs-settings
           plusx
           dash
           dash-functional
           dedicated
           buttons
           company
           sticky-windows
           header2
           flycheck))
  (safe-funcall
   (require feature)))

(cl-loop with top = (f-join emacs-top "libs")
      for lib-dir in (directory-files top)
      as fn = (f-join top lib-dir)
      if (file-directory-p fn)  do
      (add-to-list 'load-path fn))

(defun current-time-ms ()
  "Return the current time in MS."
  (cl-destructuring-bind (_ secs usecs __) (current-time)
    (+ (* 1000 secs) (/ usecs 1000))))

(defmacro with-elapsed-time (elapsed-time-ms-var form &rest body)
  "Record FORM's time in MS as ELAPSED-TIME-MS-VAR and evaluate BODY."
  (let ((start-time-sym (gensym "start-time"))
        (time-now-form `(current-time-ms)))
    `(let ((,start-time-sym ,time-now-form))
       ,form
       (let ((,elapsed-time-ms-var (- ,time-now-form ,start-time-sym)))
         ,@body))))

(defun load-rec (top-dir)
  "Load *.el files under TOP-DIR recursively."
  (cl-loop for file in (directory-files top-dir)
        as filename-abs = (f-join top-dir file)
        when (and (not (member file '("." "..")))
                  (file-directory-p filename-abs))
        do (cl-pushnew filename-abs load-path))

  (cl-loop for file in (directory-files top-dir)
        as filename-abs = (f-join top-dir file)
        if (file-directory-p filename-abs) append
        (unless (member file '("." ".."))
          (load-rec filename-abs))
        else when (and (file-regular-p filename-abs)
                       (equal "el" (f-ext filename-abs))
                       (not (equal file dir-locals-file)))
        collect
        (with-elapsed-time ms
                           (safe-funcall (load filename-abs))
                           (cons ms filename-abs))))

(let ((default-directory emacs-top))
  (let ((load-times
         (cl-loop for dir in (list
                           "vars"
                           "core" "private" "settings" "extra"
                           "experimental"
                           "extra-dirs"
                           "~/private-data/emacs-lisp"
                           "~/private-data-one-way/emacs-lisp")
               when (file-exists-p dir)
               append (load-rec (expand-file-name dir)))))
    (sort load-times (lambda (a b) (< (car a) (car b))))
    (cl-loop for (ms . file) in load-times
          do (message "%dms to load %s" ms file))))

;; debug byte-compile errors. from https://emacs.stackexchange.com/a/39217/2846
;; (defun dont-delay-compile-warnings (fun type &rest args)
;;   (if (eq type 'bytecomp)
;;       (let ((after-init-time t))
;;         (apply fun type args))
;;     (apply fun type args)))
;; (advice-add 'display-warning :around #'dont-delay-compile-warnings)

;; debug dependencies of deprecated 'cl package, etc
;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

(add-to-list 'load-path (expand-file-name "~/git/babel/"))

;; (define-key key-translation-map [?\C-h] [?\C-?])
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
"🤔"
