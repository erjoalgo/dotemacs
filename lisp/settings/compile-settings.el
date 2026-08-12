(setq compilation-always-kill t)

(defvar compilation-interpret-ansi-color nil)

(setf compilation-interpret-ansi-color t);; todo make buffer-local and mode-local

(setf compilation-save-buffers-predicate (lambda () nil))

;;taken from
;;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)

(setf compilation-ask-about-save nil)

(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

(defun maybe-colorize-compilation-buffer ()
  ;; (require 'ansi-color)
  ;; https://stackoverflow.com/questions/3072648/
  (when compilation-interpret-ansi-color
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(add-hook 'compilation-filter-hook 'maybe-colorize-compilation-buffer)

(defadvice next-error-find-buffer (around prioritize-compilation-buffer activate)
  (setq ad-return-value
        (if (and (bound-and-true-p autobuild-last-compilation-buffer)
                 (buffer-live-p autobuild-last-compilation-buffer))
            (progn
              autobuild-last-compilation-buffer)
          ad-do-it)))

(put 'compile-command 'safe-local-variable 'stringp)

(defun compilation-finished-notify (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (let ((msg (format "compilation %s: %s"
                     (s-trim compilation-state) compile-command))
        (color (if (autobuild-compilation-succeeded-p compilation-state)
                   'green 'red)))
    (stumpwm-message msg color)))

(setq autobuild-notification-function #'compilation-finished-notify
      autobuild-notify-threshold-secs 2)

(defun compilation-autorename-existing-buffer (&rest _args)
  "Start new compilations without deleting the current *compilation* buffer."
  (let* ((name "*compilation*")
         (compilation (get-buffer name)))
    (when compilation
      (with-current-buffer compilation
        (rename-buffer (generate-new-buffer-name name))
        (message "auto-renaming %s buffer to %s"
                 name (current-buffer))))))


(advice-add 'compilation-start :before #'compilation-autorename-existing-buffer)

(defvar-local compilation-original-buffer nil
  "The buffer where compile was originally called")

(defun compile--remember-original-buffer
    (original-fn &rest r)
  "Remember the original buffer where compilation was invoked."
  (let ((original-buffer (current-buffer))
        (result (apply original-fn r)))
    (with-current-buffer result
      (setq-local compilation-original-buffer
                  original-buffer))
    result))

(advice-add 'compile :around
            #'compile--remember-original-buffer)


(defvar my-original-error-source-file nil
  "Stores the absolute disk filename of the source buffer before jumping.")

(defun my-capture-source-buffer-file (&rest _args)
  "Captures the visited filename before next-error or previous-error hops contexts."
  (setq my-original-error-source-file (buffer-file-name (current-buffer))))

;; 1. Automatically snapshot the true buffer file before the window shifts focus
(advice-add 'next-error :before #'my-capture-source-buffer-file)
(advice-add 'previous-error :before #'my-capture-source-buffer-file)

(defun my-compilation-find-file-default-current-buffer (orig-fun marker filename directory &rest args)
  "Forces compilation-find-file to fall back onto the current buffer file if unknown."
  (let ((resolved-filename filename))
    ;; If Emacs has lost track of the file or it's nil/unknown
    (when (or (not filename) (string= filename "") (string= filename "*unknown*"))
      (setq resolved-filename
            (or
             (buffer-file-name (or original-next-error-buffer (current-buffer)))))
      (message "DDEBUG epty original-next-error-buffer: %s" original-next-error-buffer))
    ;; Execute the original compiled function with our dynamically injected fallback path
    (apply orig-fun marker resolved-filename directory args)))

;; Attach the wrapper cleanly to the compilation tracking subsystem
(advice-add 'compilation-find-file :around #'my-compilation-find-file-default-current-buffer)

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(spotbugs
                 ;; Regex matching: Package, Class, Filename, and Line Numbers
                 "^[HML] [A-Z] [A-Z]+: \\([a-z0-9.]+\\)\\.[A-Za-z0-9_$]+\\(?:\\.[a-z0-9_$]+\\)*([^)]*).*?At \\([A-Za-z0-9_$]+\\.java\\):\\[lines? \\([0-9]+\\)\\(?:-[0-9]+\\)?\\]"
                 ;; Function to dynamically locate the file on disk using package directories
                 (lambda ()
                   (let* ((package (match-string 1))
                          (filename (match-string 2))
                          ;; Convert package structures (io.github.wolfraam) to directory paths
                          (package-dir (replace-regexp-in-string "\\." "/" package))
                          ;; Construct standard Maven/Gradle source tree structure locations
                          (inferred-path (concat "src/main/java/" package-dir "/" filename)))
                     (list inferred-path)))
                 3 ;; Line number match group
                 nil
                 nil
                 2)) ;; File name match group highlighting reference

  (add-to-list 'compilation-error-regexp-alist 'spotbugs))
