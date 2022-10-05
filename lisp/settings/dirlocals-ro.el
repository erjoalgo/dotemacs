(require 'f)

(eval-when-compile (require 'subr-x))

(defalias '->> 'thread-last)

(defvar dirlocals-custom-path-function nil)

(defvar dirlocals-fakeroot
  (f-join (expand-file-name "~/.emacs-dirlocals-ro")))

(defun dirlocals-custom-path-fakeroot (&optional directory)
  "Use alternative fake root to store .dir-locals.el for read-only directories.

   ORIG-FN, DIRECTORY is the original function being advised and its argument."
  (setq directory (or directory default-directory))
  (unless (file-writable-p directory)
    (let ((fake-dir
           (->> directory
             (concat dirlocals-fakeroot)
             (replace-regexp-in-string "//" "/"))))
      (message "DEBUG fake-dir: %s" fake-dir)
      (make-directory fake-dir t)
      (f-join fake-dir dir-locals-file))))

(setq dirlocals-custom-path-function 'dirlocals-custom-path-fakeroot)

(defun dir-locals--all-files--custom-path-function (orig-fn directory)
  (if-let (((bound-and-true-p dirlocals-custom-path-function))
           (custom-path (funcall dirlocals-custom-path-function))
           ((file-exists-p custom-path)))
      (list custom-path)
    (funcall orig-fn directory)))

(advice-add ' dir-locals--all-files :around
              #'dir-locals--all-files--custom-path-function)

(defun dir-locals-collect-variables (class-variables root variables
                                                     &optional predicate)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list."
  (let* ((file-name (or (buffer-file-name)
                        ;; Handle non-file buffers, too.
                        (expand-file-name default-directory)))
         (sub-file-name (if (and file-name
                                 (file-name-absolute-p file-name))
                            ;; FIXME: Why not use file-relative-name?
                            (file-relative-name file-name))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (if (and (file-exists-p sub-file-name)
                                          (file-regular-p sub-file-name))
                                     `((nil ,(cadr entry)))
                                     (cdr entry))
                                  root variables))))
             ((or (not key)
                  (derived-mode-p key))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (delq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root default-directory))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message "%s error: %s" dir-locals-file (error-message-string err))
       nil))))
