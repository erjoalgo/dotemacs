(setf
  translation-suffixes
  '((original . "-original.txt")
    (correction . "-corrección.txt")
    (english . "-english.txt")
    (spanish . "-spanish.txt")
    (wdiff . "-wdiff.txt")
    (wdiff-html . "-wdiff.html")
    (final . "-final.txt")
    (wdiff-final . "-final-wdiff.txt")
    (wdiff-final-html . "-final-wdiff.html")))

(defvar translations-home
  (expand-file-name "~/git/translations/"))

(defun translation-suffix (name suffix-sym &optional path)
  (let ((suffix (cdr (assoc suffix-sym translation-suffixes)))
	filename)
    (when (and (null name) path)
      (setf name (f-base path)))
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
  (interactive `(nil))
  (setf name (or name (read-string "senter name of translation: ")))
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

(defun translation-publish-commit (subject body address)
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
  (translation-commit default-directory)
  (compose-mail
   address subject nil)
  (goto-char (point-max))
  (insert body))

(defun translation-correction-p (directory)
  (file-exists-p
   (translation-suffix nil 'correction directory)))

(defun translation-reply (directory gnus-message-mode-buffer)
  (interactive (list (read-directory-name "enter translation directory: "
                                          default-directory)
                     (progn (message "navigate to gnus reply buffer...")
                            (recursive-edit)
                            (current-buffer))))

  (let ((correction-p (translation-correction-p directory)))

    (when correction-p
      (translation-wdiff directory))

    (translation-commit directory)

    (with-current-buffer gnus-message-mode-buffer
      (let (wdiff-html text)
        (if correction-p
            (setf wdiff-html (translation-suffix nil 'wdiff-html directory)
                  text (translation-suffix nil 'correction directory))
          (setf text (translation-suffix nil 'spanish directory)))
        (goto-char (point-max))
        (when wdiff-html
          (gnus-insert-html-from-file wdiff-html))
        (gnus-attach-file-simple text)))))


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

(defun wdiff (a b dest-txt dest-html)
  (loop
   with cmd-fmt =  (format "%%s %s %s > %%s" a b)
   for (program out-filename) in
   `(("html-wdiff" ,dest-html)
     ("wdiff" ,dest-txt))
   as cmd = (format cmd-fmt program out-filename)
   if out-filename
   do (progn
        (let ((default-directory directory))
          (shell-command cmd))
        (when (s-ends-with-p ".html" out-filename)
          (let ((url (concat "file://" out-filename)))
            (message "url %s" url)
            (browse-url url))))))

(defun translation-wdiff (directory)
  (interactive (list default-directory))
  (let ((name (f-base directory)))
    (apply 'wdiff
           (mapcar (lambda (suffix) (translation-suffix name suffix directory))
                   '(original correction wdiff wdiff-html)))))

(defun translation-wdiff-final (directory)
  (interactive (list default-directory))
  (let* ((name (f-base directory))
        (final (translation-suffix name 'final directory)))
    (unless (file-exists-p final)
      (translation-interactive-create-file final))
    (wdiff
     (translation-submission directory)
     final
     nil
     (translation-suffix name 'wdiff-final-html directory))))

(defun translation-submission (directory)
  "return the result of my work, whether a correction or
a translation from scratch"
  (let ((name (f-base directory))
        (correction (translation-suffix name 'correction directory))
        (spanish (translation-suffix name 'spanish directory)))
    (if (file-exists-p correction) correction spanish)))

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
  (when (re-search-forward "\n\n" nil t)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "^ \\{25,\\}" "\n")
      (replace-regexp "\n" "XXX")
      (goto-char (point-min))
      (replace-regexp "XXXXXX" "\n\n")
      (goto-char (point-min))
      (replace-regexp "XXX" " ")

      (goto-char (point-min))
      (replace-regexp " +" " ")

      (goto-char (point-min))
      (replace-regexp "^ +" "")
      ))
  (visual-line-mode 1))

(defun translation-fix-quotes ()
  (interactive)
  (save-excursion
    (loop for (from . to) in '(
                               ("\"\\(\\(.\\|\n\\)*?\\)\"" "“\\1”")
                               ("\"" (lambda (&rest args)
                                       (error "unbalanced quotes in line %s"
                                              (line-number-at-pos (match-beginning 0)))))
                               ("'\\(.*?\\)'" "‘\\1’")
                               ("'" "’")
                               (" *-- *" "—")
                               ("\\([\"”]\\)\\([.,:]\\)" "\\2\\1")
                               ("[.]\\{3\\}" "…")
                               )
          do
          (progn
            (goto-char (point-min))
            (query-replace-regexp from to)))))

(defvar translation-manual-checklist
  '("ensure nationalities not capitalized"
    "ensure months not capitalized"
    "ensure dates are not in 'Month Day, Year' format")
  "items pending automation"
  )


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

(defun translation-new-from-file (filename &optional correction-p)
  (interactive (list (dired-file-name-at-point) prefix-arg))
  (when (equal "docx" (f-ext filename))
    (setf filename
          (docx2txt filename (lambda (fname)
                               (translation-santize-subject fname t)))))
  (let* ((name (f-base filename))
         (text-original (debian-file->string filename t))
         (fun (if correction-p 'translation-new-correction 'translation-new)))
    (apply fun name text-original nil)))

(defun translation-new-correction-from-file (filename)
  (interactive (list (dired-file-name-at-point)))
  (translation-new-from-file filename t))

(defun translation-new-from-image (filename)
  (interactive (list (dired-file-name-at-point)))
  (let* ((name (translation-santize-subject (f-base filename) t))
         (dir (translation-mkdir name)))
    (copy-file filename dir)
    (translation-new name nil (f-dirname dir))))

(defun translation-new-from-url (url)
  (interactive (list (browse-url-url-at-point)))
  (translation-new
   nil
   (with-current-buffer (url-retrieve-synchronously url)
     (prog1
         (buffer-string)
       (kill-buffer)))))

(defun translation-commit (directory)
  (interactive (list default-directory))
  (let ((default-directory directory))
    (shell-command (format "git add .; git commit -m '%s'" directory))))

(defun translation-prepare ()
  (interactive)
  (when (eq 0 (ispell-spanish))
    (translation-fix-quotes)))

;; * TODO ensure month, nationalities aren't capitalized
;; * TODO spell-check english words, names accepted by ispell-spanish
;; * TODO verify country names translate
