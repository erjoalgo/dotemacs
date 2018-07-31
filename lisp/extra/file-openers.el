(setf *file-programs*
  '(
   ("zathura" "pdf" )
   ("libreoffice" "doc" "ppt" "odt" "docx"  "pptx" "xls" "ods")
   ("evince" "ps" "eps")
   ("eog" "png" "pgm" "tif" "jpg" "jpeg" "gif")
   ("aplay" "wav")
   ((lambda (fn)
      (browser-new-tab (format "file://%s" fn))) "html")
   ("gimp" "xcf")
   ("mpg321" "mp3")
   ("vlc" "mp4" )))

(defvar open-exe
  (s-trim (shell-command-to-string "which open")))

(defun get-file-program (fn)
  (if (equal system-type 'darwin)
      open-exe
    (let ((ext (downcase (file-name-extension fn))))
      (loop for (program . exts) in *file-programs* thereis
            (and (member ext exts) program)))))

(defun coalesce (&rest strings)
  (loop for s in strings
        thereis (and s (> (length s) 0) s)))

(defun open-file (fn)
  (interactive (list (dired-file-name-at-point)))
  (setf fn (expand-file-name fn))
  (let ((program (get-file-program fn)))
    (if (not program)
	(error (concat "no program known for file: " fn))
      (if (functionp program) (funcall program fn)
	(progn (message "%s %s" program fn)
               (start-process program nil program fn))))))
