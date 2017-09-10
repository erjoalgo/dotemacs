(with-eval-after-load "image-mode"
  (define-key image-mode-map "n" 'image-next-file)
  (define-key image-mode-map "b" 'image-previous-file)
  (define-key image-mode-map "s" 'share-current-image)
  (define-key image-mode-map "c" 'exif-set-usercomment)
  )

(defmacro ensure-zero-exit (cmd-forms)
  `(if (= 0 ,cmd-forms) 0
     (error "non-zero exit from %s" ',cmd-forms)))

(defun exif-set-tag (jpg-file tag value)
  (ensure-zero-exit
   (shell-command
    (format "exiv2 -M'set Exif.Photo.%s %s' %s"
	    tag value jpg-file))))

(defun exif-get-tag (jpeg-file tag)
  (shell-command-to-string
   (format (concat "exiv2 pr -g 'Exif.Photo.%s' %s"
		   "| tr -s ' ' | cut -d' ' -f4-  | tr -d '\\n'")
	   tag jpeg-file)))

(defun exif-append-tag (jpeg-file tag value &optional delim)
  (setf delim (or delim " "))
  (let* ((old-value (exif-get-tag jpeg-file tag))
	 (new-value (concat old-value delim value)))
    (exif-set-tag jpeg-file tag new-value)))


(defun exif-append-usercomment-tag (jpeg-file value)
  (exif-append-usercomment-tag jpeg-file "UserComment" value))

(defun exif-set-usercomment (jpeg-file value)
  (interactive (let ((jpeg-file (buffer-file-name nil)))
		 (list jpeg-file
		       (read-string "edit UserComment: "
				    (exif-get-tag jpeg-file "UserComment")))))
  (exif-set-tag jpeg-file "UserComment" value))

(defun exif-get-usercomment (jpeg-file)
  (interactive (let ((jpeg-file (buffer-file-name nil)))))
  (exif-get-tag jpeg-file "UserComment"))

(defun compress-video (filename video-name &optional resolution)
  (interactive (list (dired-file-name-at-point)
		     ;; (read-file-name "enter new filename: ")
		     ;; (concat "share/" (read-string "enter video name: ") ".mp4")
		     (read-string "enter video name: ")
		     ;; (read-string "enter resolution: " "640x480")
		     nil
		     ))
  (setf resolution (or resolution "640x480"))
  (let* ((new-filename (format "share/%s-%s-%s.mp4"
			       (-> filename f-filename f-no-ext)
			       resolution video-name))
	 (cmd (format "avconv -i %s -s %s -strict experimental -vf transpose=1 %s"
	 ;; (cmd (format "avconv -i %s -s %s -strict experimental %s"
		      filename resolution new-filename))
	 (async-shell-command-buffer 'new-buffer))
    (message "cmd %s" cmd)
    (async-shell-command cmd)))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "s-v") 'compress-video))

(defun turn-off-erjoalgo-cmd-mode ()
  (message "turning off erjoalgo...")
  (erjoalgo-command-mode 0))

(add-hook 'image-mode-hook 'turn-off-erjoalgo-cmd-mode)

(defvar image-share-image-directory "./share")

(defun image-set-share-current-image-directory (directory)
  (interactive "Denter directory ")
  "specify the destination directory for symlinking images to share via
share-current-image"
  (setf image-share-image-directory
	(expand-file-name directory)))

(defun share-current-image ()
  "symlink current image to the specified directory"
  (interactive)
  (let ((cmd (format "ln -s %s %s" (buffer-file-name nil)
		     image-share-image-directory)))
    (message "%s" cmd)
    (shell-command cmd)))
