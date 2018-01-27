(defvar
  translation-suffixes
  '((original . "-original.txt")
    (correction . "-corrección.txt")
    (english . "-english.txt")
    (spanish . "-spanish.txt")
    (wdiff . "-wdiff.txt")))

(defvar translations-home
  (expand-file-name "~/git/translations/"))

(defun translation-suffix (name suffix-sym)
  (let ((suffix (cdr (assoc suffix-sym translation-suffixes)))
	filename )
    (f-join translations-home
	    name ;;directory
	    (concat name suffix));;file bassename
    ))

(defun translation-interactive-create-file (filename &optional text)
  (find-file filename)
  (if text
      (insert text)
    (progn (message (format "enter %s contents" (f-base filename)))
	   (clipboard-yank)
	   (recursive-edit)))
  (save-buffer)
  (buffer-string))

(defun translation-mkdir (name)
  (let ((dir (f-join translations-home name)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun translation-new (name &optional text-english)
  (interactive "senter name of translation: ")
  (translation-mkdir name)
  (translation-interactive-create-file
   (translation-suffix name 'english) text-english)

  (translation-interactive-create-file
   (translation-suffix name 'spanish) "")
  (visual-line-mode 1))

(defun translation-new-correction
    (name &optional text-original text-english)
  (interactive "senter name of translation: ")
  (translation-mkdir name)

  (setf text-original
	(translation-interactive-create-file
	 (translation-suffix name 'original) text-original))

  (translation-interactive-create-file
   (translation-suffix name 'english) text-english)

  (find-file (translation-suffix name 'correction))
  (insert text-original)

  (translation-correction-fix-paragraphs))

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

(defun debian-file->string (filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (buffer-string)))

(defun translation-wdiff (name)
  (interactive (list (f-base default-directory)))
  (let* (
	 (wdiff-file-name (translation-suffix name 'wdiff))
	 (cmd (format "wdiff %s %s > %s"
		      (translation-suffix name 'original)
		      (translation-suffix name 'correction)
		      wdiff-file-name)))
    (message "cmd %s" cmd)
    (shell-command cmd)
    (kill-new (debian-file->string wdiff-file-name))
    (message "yanked wdif output")))

(defun translation-english-trim-non-article-text ()
  (interactive)
  (goto-char (point-min))
  (let* ((s (buffer-string))
	 (pre-line "\nDownload PDF flyer\n")
	 (post-line "\n[a-z ]+, [a-z ,]+\n")
	 (wildcard "\\(.\\|\n\\)")
	 (regexp (concat
		  (format "^%s*" wildcard)
		  pre-line
		  (format "\\(%s+?\\)" wildcard)
		  post-line
		  ))
	 )
    (let ((m (string-match regexp s)))
      (assert m)
      (erase-buffer)
      (insert (match-string 2 s))
      (save-buffer))
    ))
