(defvar
  translation-suffixes
  '((original . "-original.txt")
    (correction . "-corrección.txt")
    (english . "-english.txt")
    (spanish . "-spanish.txt")
    (wdiff . "-wdiff.txt")
    (wdiff-html . "-wdiff.html")))

(defvar translations-home
  (expand-file-name "~/git/translations/"))

(defun translation-suffix (name suffix-sym &optional path)
  (let ((suffix (cdr (assoc suffix-sym translation-suffixes)))
	filename)
    (f-join translations-home
            (or path name)
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

(defun translation-mkdir (name &optional path)
  (unless path
    (setf path (read-file-name "enter path: " translations-home "")))
  (assert (s-starts-with? (f-full translations-home) (f-full path)))
  (let ((dir (f-join path name)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun translation-new (name &optional text-english dir)
  (interactive "senter name of translation: ")
  (setf dir (translation-mkdir name dir))
  (translation-interactive-create-file
   (translation-suffix name 'english dir) text-english)

  (translation-interactive-create-file
   (translation-suffix name 'spanish dir) ""))

(defun translation-new-correction
    (name &optional text-original text-english dir)
  (interactive "senter name of translation: ")
  (setf dir (translation-mkdir name dir))

  (setf text-original
	(translation-interactive-create-file
	 (translation-suffix name 'original dir) text-original))

  (translation-interactive-create-file
   (translation-suffix name 'english dir) text-english)

  (find-file (translation-suffix name 'correction dir))
  (insert text-original)

  (translation-correction-fix-paragraphs)
  (visual-line-mode t))

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

(defun message-text ()
  (save-excursion (goto-char (point-min))
		  (re-search-forward "^Date")
		  (next-logical-line 1)
		  (buffer-substring-no-properties (point) (point-max))))

(defun translation-santize-subject (subject &optional manual-sanitize)
  (let ((sanitized (-> subject
		       (gnus-replace-in-string "[^[:alnum:]]" "-")
		       (gnus-replace-in-string "traducci.n-" "")
                       (gnus-replace-in-string "-+" "-")
		       downcase)))
    (when manual-sanitize
      (setf sanitized (read-string "enter translation name: "
				   sanitized)))
    sanitized))

(defun translation-new-correction-from-article ()
  (interactive)
  (let ((text-original (message-text))
	(translation-name (translation-santize-subject
			   (message-fetch-field "Subject") t)))
    (translation-new-correction translation-name
				text-original nil)))

(defun docx2txt (docx &optional filename-mapper)
  (interactive (list (dired-file-name-at-point)))
  (let* ((docx (expand-file-name docx))
         (dest-dir (f-dirname docx))
         (translation-name (funcall (or filename-mapper 'identity) (f-base docx)))
         (dest-fn (f-join dest-dir (concat translation-name ".txt"))))
    (call-process "docx2txt" nil "buffer" nil docx dest-fn)
    (assert (file-exists-p dest-fn))
    dest-fn))

(defun translation-new-correction-from-docx-attachment ()
  (interactive)
  (let ((attachments-dir (gnus-dir-name-for-message)))
    (gnus-mime-save-all-attachments attachments-dir)
    (let ((cands
	   (remove-if-not (lambda (fn) (string-match ".*[.]docx?" fn))
			  (directory-files attachments-dir)))
	  (default-directory attachments-dir))
      (when (or (null cands) (cdr cands))
	(error "not exactly one docx? file found"))
      (let* ((docx (car cands))
             (translation-name (translation-santize-subject (f-base docx) t))
             (txt (docx2txt docx (lambda (fname) translation-name)))
            (text (with-temp-buffer
                    (insert-file-contents txt)
                    (buffer-string))))
        (kill-new text)
	(translation-new-correction translation-name  nil nil)))))

(defun debian-file->string (filename &optional not-literal-p)
    (with-temp-buffer
      (funcall (if not-literal-p 'insert-file-contents 'insert-file-contents-literally) filename)
      (buffer-string)))

(defun translation-wdiff (directory)
  (interactive (list default-directory))
  (let* ((name (f-base directory))
         (cmd-fmt (format "%%s %s %s > %%s"
                          (translation-suffix name 'original directory)
                          (translation-suffix name 'correction directory))))
    (loop for (program out-filename) in
          `(("html-wdiff" ,(translation-suffix name 'wdiff-html directory))
            ("wdiff" ,(translation-suffix name 'wdiff directory)))
          as cmd = (format cmd-fmt program out-filename)
          do (progn
               (let ((default-directory directory))
                 (shell-command cmd))
               (kill-new (debian-file->string out-filename))
               (message "yanked wdif output")))))

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
      (save-buffer))))

(defun translation-correction-fix-paragraphs ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\n" "XXX")
    (goto-char (point-min))
    (replace-regexp "XXXXXX" "\n\n")
    (goto-char (point-min))
    (replace-regexp "XXX" " ")

    (goto-char (point-min))
    (replace-regexp " +" " ")

    (goto-char (point-min))
    (replace-regexp "^ +" "")
    (visual-line-mode 1)))

(defun translation-fix-quotes ()
  (interactive)
  (query-replace-regexp "\"\\(.*?\\)\"" "“\\1”"))

(defun translation-new-inline-correction (filename)
  (interactive (list (dired-file-name-at-point)))
  (when (equal "docx" (f-ext filename))
    (setf filename
          (docx2txt filename (lambda (fname)
                               (translation-santize-subject fname t)))))
  (let* ((correction-filename
          (concat (f-base filename)
                  (cdr (assoc 'correction translation-suffixes)))))
    (copy-file (expand-file-name filename) correction-filename)
    (find-file correction-filename)
    (visual-line-mode)))
