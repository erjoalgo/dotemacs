(setf *file-programs*
  '(
   ("zathura" . ("pdf"))
   ("libreoffice" . ("doc" "ppt" "odt" "docx"  "pptx" "xls" "ods"))
   ("evince" . ("ps" "eps"))
   ("eog" . ("png" "pgm" "tif" "jpg" "jpeg" "gif" "pnm" "svg"))
   ("aplay" "wav")
   ("timidity" . ("midi" "mid"))
   ((lambda (fn)
      (browser-new-tab (format "file://%s" fn))) "html")
   ("gimp" . ("xcf"))
   ("mpg321" . ("mp3"))
   ("vlc" . ("mov" "mkv" "aac"))
   ("celluloid" . ("mp4"))
   ("xournal" . ("xoj"))))

(defvar open-exe
  (s-trim (shell-command-to-string "which open")))

(defun get-file-program (fn)
  (if (equal system-type 'darwin)
      open-exe
    (let ((ext (downcase (file-name-extension fn))))
      (cl-loop for (program . exts) in *file-programs* thereis
            (and (member ext exts) program)))))

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
          (warn (concat "no program known for file: " filename))
          (find-file filename))
      (if (functionp program) (funcall program filename)
	(progn (message "%s %s" program filename)
               (start-process program nil program filename))))))
