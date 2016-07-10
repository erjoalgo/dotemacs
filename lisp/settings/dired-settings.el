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
  (interactive (list (dired-file-name-at-point)))
  (setf fn (expand-file-name fn))
  (let ((program (get-file-program fn)))
    (if (not program)
	(error (concat "no program known for file: " fn))
      (start-process program nil program fn))))
  
(with-eval-after-load "dired"
  (define-key dired-mode-map "q" 'dired-up-directory)
  (define-key dired-mode-map (kbd "s-f") 'open-file)
  (define-key dired-mode-map (kbd "s-d") 'dired-recursive-du))


;;don't prompt me
;;todo this isn't working
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))

(with-eval-after-load "dired"
  ;;TODO
  (define-key dired-mode-map (kbd "s-c")
    (lambda () (interactive) (set-clipboard (dired-file-name-at-point))))
  
  (define-key dired-mode-map (kbd "s-f")
    (lambda () (interactive) (open-file (dired-file-name-at-point)))))

(defun dired-recursive-du ()(interactive)
	      (let ((dired-dir dired-directory)
		    (du-buffer "dired-rec-du"))
		;;(shell-command (format "du -ah -d 1 %s | sort -h -r" dired_dir) dubuffer dubuffer )
		(shell-command (format "du -ah --max-depth 1 %s | sort -h" dired-dir) du-buffer du-buffer )
		(set-buffer du-buffer )
		(replace-string dired-dir "" nil (point-min) (point-max))
		(temp-buffer-window-show du-buffer)
		(goto-char (point-max))))

(setq image-dired-show-all-from-dir-max-files 2000)
