(defvar *file-programs*
  '(
   ("zathura" "pdf" )
   ("libreoffice" "doc" "ppt" "odt" "docx"  "pptx")
   ("evince" "ps" "eps")
   ("eog" "png" "pgm" "tif" "jpg" "jpeg")
   ("aplay" "wav")
   ("firefox" "html")
   ("vlc" "mp4" )))

(defun get-file-program (fn)
  (let ((ext (downcase (file-name-extension fn))))
    (loop for (program . exts) in *file-programs* thereis
	  (and (member ext exts) program))))

(defun open-file (fn)
  (interactive (list (expand-file-name (dired-file-name-at-point))))
  (let* ((program (get-file-program fn)))
    (if (not program)
	(error (concat "no program known for file: " fn))
      (start-process program nil program fn))))
  
(with-eval-after-load "dired"
  (define-key dired-mode-map "q" 'dired-up-directory)
  (define-key dired-mode-map (kbd "s-f") 'open-file))


;;don't prompt me
;;todo this isn't working
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))

