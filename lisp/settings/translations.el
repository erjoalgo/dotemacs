(require 'babel)

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
            ;;file bassename
	    (concat name suffix))))

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

(defun babel-english-to-spanish (text)
  (interactive "senter english text: ")
  (let ((english (babel-string text "en" "es" 'google)))
    (message "translated: %s" english)
    english))


;;;###autoload
(defun translation-new (name &optional text-english dir)
  (interactive `(nil))
  (setf name (or name (read-string "senter name of translation: ")))
  (setf dir (translation-mkdir name dir))

  (let ((text-english
         (translation-interactive-create-file
          (translation-suffix name 'english dir) text-english)))
        (assert text-english)
        (translation-interactive-create-file
         (translation-suffix name 'spanish dir)
         (babel-english-to-spanish text-english))))


;;;###autoload
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

(defvar translation-submissions-address-alist)

;;;###autoload
(defun translation-publish-commit (subject body address attachment-type original-buffer)
  (interactive
   (save-excursion
     (let* ((subject
             (progn
               (goto-char (point-min))
               (end-of-line)
               (buffer-substring-no-properties (point-min) (point))))
            (body (buffer-substring-no-properties (point) (point-max)))
            (address (completing-read "select submission address: "
                                      (mapcar 'car translation-submissions-address-alist)))
            (attachment-type (or
                              (cdr (assoc address translation-submissions-address-alist))
                              'plaintext)))
       (list subject body address attachment-type (buffer-file-name)))))
  (assert address)
  (translation-commit default-directory)
  (compose-mail
   address subject nil)
  (goto-char (point-max))
  (case attachment-type
    ('plaintext (insert body))
    ('attachment (gnus-attach-file-simple original-buffer))
    (t (error "unknown attachment-type: %s" attachment-type))))

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

;;;###autoload
(defun translation-new-correction-from-docx-attachment ()
  (interactive)
  (let ((attachments-dir (gnus-dir-name-for-message)))
    (gnus-mime-save-all-attachments attachments-dir)
    (let ((cands
	   (remove-if-not (lambda (fn) (string-match ".*[.]docx?" fn))
			  (directory-files attachments-dir)))
	  (default-directory attachments-dir))

      (let* ((docx (cond
                    ((null cands) (error "no docx attachments found"))
                    ((null (cdr cands)) (car cands))
                    (t (selcand-select cands "select an attachment to translate: "))))
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
  (check-cmd "which" '("wdiff") "missing wdiff")
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

;;;###autoload
(defun translation-wdiff (directory)
  (interactive (list default-directory))
  (let ((name (f-base directory)))
    (apply 'wdiff
           (mapcar (lambda (suffix) (translation-suffix name suffix directory))
                   '(original correction wdiff wdiff-html)))))

;;;###autoload
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
     (translation-suffix name 'wdiff-final-html directory))
    (translation-commit directory)))

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
		  post-line)))
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
      (replace-regexp "^ +" "")))
  (visual-line-mode 1))

(defvar translation-regexp-rules-alist)

(defun translation-phrases-to-rules (fun phrases)
  (mapcar (lambda (phrase)
            (list (regexp-quote phrase)
                  `(lambda (&rest args)
                     (,fun (match-string 0)))))
          phrases))

(setq month-regexp
      (s-join "\\|"
              (loop for mon in
                    (append
                     '("enero" "febrero" "marzo" "abril"
                       "mayo" "junio" "julio" "agosto"
                       "septiembre" "octubre" "noviembre" "diciembre")
                     '("jan" "apr" "aug" "dec"))
                    collect mon
                    collect (cl-subseq mon 0 3))))

(setf translation-regexp-rules-alist
      (append
       `(
         ;; (,month-regexp #'downcase)
         (,(format "\\([0-9]\\{1,2\\}\\) \\(%s\\),? \\([0-9]\\{2,4\\}\\)"
                  month-regexp)
          "\\1 de \\? del \\3")
         (,(format "\\(%s\\) \\([0-9]\\{1,2\\}\\),? \\([0-9]\\{2,4\\}\\)"
                  month-regexp)
          "\\2 de \\? del \\3")
         ("\"\\(\\(.\\|\n\\)*?\\)\"" "“\\1”")
         ("\"" (lambda (&rest args)
                 (error "unbalanced quotes in line %s"
                        (line-number-at-pos (match-beginning 0)))))
         ("'\\(.*?\\)'" "‘\\1’")
         ("`" "‘")
         ("'" "’")
         (" *--+ *" "—")
         (" +-+ +" "—")
         ("\\([\"”]\\)\\([.,:]\\)" "\\2\\1")
         ("[.]\\{3\\}" "…")
         ("[.]\\{2\\}" ".")
         ("”“" "“")
         ("“”" "”")
         (" \\([.”]\\)" "\\1")
         ("^\\([A-Z][a-z]* .\\{,100\\}\\)[.]$" "\\1")
         (", y " " y ")
         ("ee.uu." "EE. UU.")
         (" -\\([a-zA-Z]\\)" "—\\1")
         ("\\([a-zA-Z]\\)- " "\\1—")
         ("[?][?]" (lambda (&rest args)
                     (error "?? exists"))))

       (translation-phrases-to-rules
        'capitalize '("estados unidos"))

       (translation-phrases-to-rules
        'upcase '("ee. uu."))))

(defun translation-fix-quotes ()
  (interactive)
  (save-excursion
    (loop for (from . to) in translation-regexp-rules-alist
          do
          (progn
            (goto-char (point-min))
            (let ((case-fold-search nil))
              (query-replace-regexp from to))))
    t))

(defvar translation-manual-checklist
  '("ensure nationalities are not capitalized")
  "items pending automation")


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
    (visual-line-mode)
    (flyspell-mode)))

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

(cl-defun translation-google-translate
    (api-key source-text
             &key
             (lang-code-source "en")
             (lang-code-target "es")
             (format "text"))
  (let ((url-request-method        "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data          "field1=Hello&field2=from&field3=Emacs"))
    (with-current-buffer (url-retrieve-synchronously url)
      (buffer-string))))

(defun translation-new-google-translate-correction (url)
  (interactive "senter document url: ")
  (let ((html-content
         ;; text-content
         ;; (shell-command-to-string
         ;;  (format "html-url-extract-text.py %s" url))
         (url-retrieve url))
        (google-translate-post-endpoint
         "https://translation.googleapis.com/language/translate/v2")
        (params `(("target" "es")
                  ("format" "html")
                  ("source" "en")
                  ("key" ,google-translate-api-key)
                  ("q" ,html-content))))))

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

(defun check-cmd (cmd args err)
  (unless (zerop (apply #'call-process cmd nil nil nil args))
    (error "error: %s" err)))

(defun translation-commit (directory)
  (interactive (list default-directory))
  (check-cmd "git" '("status") "git not initialized")
  (let ((default-directory directory))
    (shell-command (format "git add .; git commit -m '%s'" directory))))

;;;###autoload
(defun translation-prepare ()
  (interactive)
  (when (eq 0 (ispell-spanish))
    (translation-fix-quotes)))

(defun ispell-spanish ()
  (interactive)
  (ispell-change-dictionary "castellano8")
  (ispell))

;; * TODO ensure month, nationalities aren't capitalized
;; * TODO spell-check english words, names accepted by ispell-spanish
;; * TODO verify country names translate
(autobuild-define-rule
 autobuild-translations
 nil
 (when (s-starts-with?
        (expand-file-name "~/git/translations")
        (buffer-file-name))
   (lambda ()
     (when (translation-prepare)
       (call-interactively #'translation-publish-commit)))))

(define-minor-mode translation-mode
  "Translation minor mode"
  nil
  "translation-mode"
  (buttons-make
   ((kbd "s-t")
    (buttons-make
     ((kbd "s-l") #'lingee-translate)))))

(defun translation-mode-maybe-enable ()
  (when (and (buffer-file-name)
             (s-starts-with?
              (expand-file-name "~/git/translations")
              (buffer-file-name)))
    (translation-mode t)))

(add-hook 'find-file-hook #'translation-mode-maybe-enable)

(provide 'translation)
