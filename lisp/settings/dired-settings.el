;;don't prompt me
;;todo this isn't working
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))



(defun dired-recursive-du ()(interactive)
	      (let ((dired-dir dired-directory)
		    (du-buffer "dired-rec-du"))
		(shell-command (format "du -ah --max-depth 1 %s | sort -h" dired-dir) du-buffer du-buffer )
		(set-buffer du-buffer )
		(replace-string dired-dir "" nil (point-min) (point-max))
		(temp-buffer-window-show du-buffer)
		(goto-char (point-max))))

(setf dired-tmp-char 31)
(setf dired-mark-char 42)

(defun dired-tagger-read-char ()
  (let ((char (read-char "enter lowercase char, q to quit: ")))
    (unless (member char '(Quit 113)) char)))

(defun dired-mark-file-as (dest-char)
  (interactive (list (dired-tagger-read-char)))
  (when dest-char
    (dired-change-marks dired-mark-char dired-tmp-char)
    (dired-mark nil)
    (dired-change-marks dired-mark-char dest-char)
    (dired-change-marks dired-tmp-char dired-mark-char)
    t))

(defun dired-tagger-tag-loop (&optional open)
  (interactive "P")
  (loop
   as buf = (when open (dired-find-file))
   as char = (dired-tagger-read-char)
   when buf do (kill-buffer buf)
   while (and char (dired-mark-file-as char))))

(defun dired-kill-marked-filenames ()
  (interactive)
  (->> (dired-remember-marks (point-min) (point-max))
    (mapcar (lambda (item)
              (f-filename (car item))))
    (s-join ", ")
    (kill-new)))

(defun dired-tagger-move ()
  (interactive)

  (let* ((arr (make-vector 256 nil)))

    (loop for (file . mark) in (dired-remember-marks (point-min) (point-max))
          do (assert mark)
          if (eq mark dired-mark-char) do
          (error "remove the standard mark from all files first: %s"
                 file)
          else do
          (push file (aref arr mark)))

    (loop for files across arr
          for mark from 0
          if files do
          (progn (message "mark: %s: %s" mark files)
                 '(let ((dest
                         (find-file ))))
                 (message "files '%s' are: %s"
                          (char-to-string mark)
                          (s-join "\n" files))
                 (dired-change-marks mark dired-mark-char)
                 (dired-do-rename)))))

(setq image-dired-show-all-from-dir-max-files 2000)
