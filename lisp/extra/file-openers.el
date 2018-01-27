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

(defun get-file-program (fn)
  (let ((ext (downcase (file-name-extension fn))))
    (loop for (program . exts) in *file-programs* thereis
	  (and (member ext exts) program))))

(defun open-file (fn)
  (interactive (list (dired-file-name-at-point)))
  (setf fn (expand-file-name fn))
  (let ((program (get-file-program fn)))
    (if (not program)
	(error (concat "no program known for file: " fn))
      (if (functionp program) (funcall program fn)
	(start-process program nil program fn)))))
