(defun translation-new (name text-original text-english)
  (interactive (list (read-string "enter name of translation (no spaces): ")
		     nil nil))
  (let* ((dir (f-join "~/git/translations/" name))
	(original (f-join dir (concat name "-original.txt")))
	(correction (f-join dir (concat name "-corrección.txt")))
	(english (f-join dir (concat name "-english.txt"))))
    (make-directory dir)

    (find-file original)
    (if text-original
	(insert text-original)
      (progn (message "paste original translation")
	     (clipboard-yank)
	     (recursive-edit)))
    (save-buffer)

    (find-file english)
    (if text-english
	(insert text-english)
      (progn (message "paste english source")
	     (recursive-edit)))
    (save-buffer)

    (find-file correction)
    (insert-file original)
    (message "start working!")))

(defun translation-publish (subject body address)
  (interactive
   (save-excursion
     (let ((subject
	    (progn
	      (goto-char (point-min))
	      (end-of-line)
	      (buffer-substring-no-properties (point-min) (point))))
	   (body (buffer-substring-no-properties (point) (point-max))))
       (list subject body
	     "libnews.wp.1423124091423874123@gmail.com"))))
  (compose-mail
   address subject nil)
  (goto-char (point-max))
  (insert body))
