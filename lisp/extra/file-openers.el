(setf *file-programs*
  '(
   ("zathura" "pdf" )
   ("libreoffice" "doc" "ppt" "odt" "docx"  "pptx")
   ("evince" "ps" "eps")
   ("eog" "png" "pgm" "tif" "jpg" "jpeg" "gif")
   ("aplay" "wav")
   ("firefox" "html")
   ("vlc" "mp4" )))

(defun get-file-program (fn)
  (let ((ext (downcase (file-name-extension fn))))
    (loop for (program . exts) in *file-programs* thereis
	  (and (member ext exts) program))))
