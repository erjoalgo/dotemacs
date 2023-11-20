(require 'cl-lib)

(defmacro def-open-file-program (opener exts)
  "Specify that file extensions in EXTS should be opened by OPENER.

   OPENER can be a program name or a function."
  `(progn
     (setq *file-programs*
           (cl-remove ,opener *file-programs*
                      :key #'car :test #'equal))
     (push (cons ,opener ',exts) *file-programs*)))

(defvar *file-programs* nil "Alist of (PROGRAM . EXTENSIONS) pairs")

(def-open-file-program "zathura" ("pdf"))
(def-open-file-program "libreoffice" ("doc" "ppt" "odt" "docx"  "pptx" "xls" "ods"))
(def-open-file-program "evince" ("ps" "eps"))
(def-open-file-program "eog" ("png" "pgm" "tif" "jpg" "jpeg" "gif" "pnm" "svg" "ico" "webp"))
(def-open-file-program "aplay" ("wav"))
(def-open-file-program "timidity" ("midi" "mid"))
(defun open-html-file (filename)
  (browser-new-tab (format "file://%s" filename)))
(def-open-file-program #'open-html-file ("html"))
(def-open-file-program #'find-file ("txt"))
(def-open-file-program "gimp" ("xcf"))
(def-open-file-program "mpg321" ("mp3"))
(def-open-file-program "vlc" ("mov" "mkv" "aac"))
(def-open-file-program "celluloid" ("mp4"))
(def-open-file-program "xournal" ("xoj"))
(def-open-file-program :self ("AppImage"))


(defvar open-exe
  (s-trim (shell-command-to-string "which open")))

(defun get-file-program (fn)
  (if (equal system-type 'darwin)
      open-exe
    (let ((ext (file-name-extension fn)))
      (when ext
        (setq ext (downcase ext))
        (cl-loop for (program . exts) in *file-programs* thereis
                 (when (member ext (mapcar #'downcase exts))
                   (if (eq :self program)
                       fn
                     program)))))))

(defun coalesce (&rest strings)
  (cl-loop for s in strings
        thereis (and s (> (length s) 0) s)))

(defun open-file (filename)
  (interactive (list (dired-file-name-at-point)))
  (setf filename (expand-file-name filename))
  (setq filename
        (replace-regexp-in-string "^/sudo:root@[^:]+:" "" filename))
  (let ((program (get-file-program filename)))
    (if (not program)
        (progn
          (message (concat "no program known for file: " filename))
          (find-file filename))
      (if (functionp program) (funcall program filename)
	(progn (message "%s %s" program filename)
               (start-process program nil program filename))))))
