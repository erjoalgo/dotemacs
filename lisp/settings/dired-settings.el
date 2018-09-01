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

(defun dired-tagger-tag-loop ()
  (interactive)
  (loop while (call-interactively 'dired-mark-file-as)))

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

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "b") 'dired-mark-file-as)
     (define-key dired-mode-map (kbd "B") 'dired-tagger-tag-loop)))

(setq image-dired-show-all-from-dir-max-files 2000)
