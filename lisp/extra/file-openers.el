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
(setq *file-programs* nil)

(def-open-file-program "zathura" ("pdf"))
(def-open-file-program "libreoffice" ("doc" "ppt" "odt" "docx"  "pptx" "xls" "ods"))
(def-open-file-program "evince" ("ps" "eps"))
(def-open-file-program "x-www-browser-local-file.sh" ("png" "pgm" "tif" "jpg" "jpeg" "gif" "pnm" "svg" "ico" "webp"))
(def-open-file-program "aplay" ("wav"))
(def-open-file-program "timidity" ("midi" "mid"))
(defun open-html-file (filename)
  (browser-new-tab (format "file://%s" filename)))
(def-open-file-program #'open-html-file ("html"))
(def-open-file-program #'find-file ("txt" "html"))
(def-open-file-program "gimp" ("xcf"))
(def-open-file-program "mpg321" ("mp3"))
(def-open-file-program "audacity" ("mp3" "ogg"))
(def-open-file-program "vlc" ("mov" "mkv" "aac"))
(def-open-file-program "celluloid" ("mp4"))
(def-open-file-program "xournal" ("xoj" "pdf"))
(def-open-file-program :self ("AppImage"))
(defvar *creality-print-exe* nil)
(when
    (setq *creality-print-exe*
          (car (file-expand-wildcards "~/Downloads/Creality_Print*AppImage")))
  (def-open-file-program *creality-print-exe* ("stl")))
(def-open-file-program "cura" ("stl" "3mf"))
(def-open-file-program "blender" ("blend" "blend1"))
(def-open-file-program "fstl" ("stl"))
(def-open-file-program "~/Downloads/OpenSCAD-2024.08.05.ai20224-x86_64.AppImage" ("scad"))

(defun open-crdownload (filename)
  (interactive)
  (cl-assert (equal (f-ext filename) "crdownload"))
  (let ((sans-ext (replace-regexp-in-string
                   ".crdownload" "" filename)))
    (cond
     ((file-exists-p sans-ext)
      (message "file finished downloading: %s" sans-ext)
      (open-file sans-ext))
     ((file-exists-p filename)
      (message "waiting for file to finish downloading: %s"
               (shell-command-to-string
                (format "du -h '%s'" filename)))
      (run-at-time 5 nil (apply-partially #'open-crdownload filename)))
     (t
      (warn "crdownload file does not exist: %s" filename)))))

(def-open-file-program #'open-crdownload ("crdownload"))

(defvar open-exe
  (s-trim (shell-command-to-string "which open")))

(defun get-file-programs (fn)
  (if (equal system-type 'darwin)
      open-exe
    (let ((ext (file-name-extension fn)))
      (when ext
        (setq ext (downcase ext))
        (cl-loop for (program . exts) in *file-programs*
                 when (member ext (mapcar #'downcase exts))
                 collect
                 (if (eq :self program)
                     fn
                   program))))))

(defun coalesce (&rest strings)
  (cl-loop for s in strings
           thereis (and s (> (length s) 0) s)))

(defun open-file (filename &optional no-prompt)
  (interactive (list (dired-file-name-at-point)))
  (setf filename (expand-file-name filename))
  (setq filename
        (replace-regexp-in-string "^/sudo:root@[^:]+:" "" filename))
  (let* ((programs (get-file-programs filename))
         (program (if (or (null (cdr programs)) no-prompt)
                      (car programs)
                    (selcand-select programs
                                    :prompt (format "select program to open %s: " filename))))
         (buff (get-buffer-create (format "*%s*" program))))
    (if (not program)
        (progn
          (message (concat "no program known for file: " filename))
          (find-file filename))
      (if (functionp program) (funcall program filename)
	(progn (message "%s %s" program filename)
               (start-process program buff program filename))))))
