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
  (cl-assert (s-starts-with? (f-full translations-home) (f-full path)))
  (let ((dir (f-join path name)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defvar babel-default-to-language nil
  "Default language code into which to translate.")

(defun babel-read-default-to-language ()
  (interactive)
  (setq babel-default-to-language
        (cdr
         (selcand-select babel-languages
                  "select language code: "
                  #'car))))

(defun babel-translate-region (a b &optional to engine from)
  (interactive "r")
  (let* ((text (buffer-substring a b))
         (to (or babel-default-to-language
                 (babel-read-default-to-language)))
         (engine (or engine 'google)))
    (with-current-buffer (get-buffer-create "*babel-translation-result*")
      (erase-buffer)
      (insert
       (babel-string text from to engine))
      (switch-to-buffer (current-buffer)))))

(defun babel-english-to-spanish (text)
  (interactive "senter english text: ")
  (let ((english (babel-string text "en" "es" 'google)))
    (message "translated: %s" english)
    english))

(defun babel-spanish-to-english (text)
  (interactive "senter spanish text: ")
  (let ((english (babel-string text "es" "en" 'google)))
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
        (cl-assert text-english)
        (translation-interactive-create-file
         (translation-suffix name 'spanish dir)
         (babel-english-to-spanish text-english))
        (translations-flobicación)))


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

  (translation-correction-fix-paragraphs))

(defun translations-flobicación ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "FLOB[^ ]+ ?" nil t)
      (replace-match "\n\n"))))

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
  (cl-assert address)
  (translation-commit default-directory)
  (compose-mail
   address subject nil)
  (goto-char (point-max))
  (cl-case attachment-type
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

(defun convert-to-txt (filename output-mapper)
  (let* ((input (expand-file-name filename))
         (output-basename
          (if (functionp output-mapper) (funcall output-mapper filename)
            output-mapper))
         (dest-dir (f-dirname filename))
         (output (f-join dest-dir (concat output-basename ".txt")))
         (ext (f-ext input)))
    (message "DEBUG edpr input: %s" input)
    (message "DEBUG n9md output: %s" output)
    (cond
     ((equal ext "docx")
      (cl-assert
       (zerop (call-process "docx2txt" nil "*docx2txt*" nil input output))))
     ((equal ext "pdf")
      (cl-assert
       (zerop (call-process "pdftotext" nil
                            "*pdftotext*" nil input output))))
     ((equal ext "odt")
      (cl-assert (zerop
                  (call-process "odt2txt" nil "*odt2txt*" nil
                                input
                                (format "--output=%s" output))))))
    (cl-assert (file-exists-p output))
    output))

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
             (txt (convert-to-txt docx (lambda (fname) translation-name)))
             (text (with-temp-buffer
                     (insert-file-contents txt)
                     (buffer-string))))
        (kill-new text)
	(translation-new-correction translation-name  nil nil)))))

(defun wdiff (a b dest-txt dest-html)
  (check-cmd "which" '("wdiff") "missing wdiff")
  (cl-loop
   for (program out-filename) in `(("wdiff-html" ,dest-html)
                                   ("wdiff" ,dest-txt))
   if out-filename
   do
   (with-current-buffer (find-file-noselect out-filename)
     (let* ((default-directory directory)
            (err-buff "*wdiff-err*")
            (ret (save-window-excursion
                   (shell-command (s-join " " (list program a b))
                                (current-buffer)
                                err-buff))))
       (save-buffer)
       ;; (if (not (zerop ret))
       ;;     (error "%s failed. see %s for details" program err-buff)
       (when (s-ends-with-p ".html" out-filename)
         (let ((url (concat "file://" out-filename)))
           (message "url %s" url)
           (browse-url url)))))))

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
      (cl-assert m)
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
      (replace-regexp "^ +" ""))))

(defun translation-correction-fix-paragraphs-2 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp (char-to-string 8204) "")
    (goto-char (point-min))
    (replace-regexp "\n[ \t ]*\n" "XXX")
    (goto-char (point-min))
    (replace-regexp "\n" "")
    (goto-char (point-min))
    (replace-regexp "XXX" "\n\n")
    (goto-char (point-min))
    (replace-regexp "  " " ")
    (goto-char (point-min))
    (replace-regexp " +$" "")))

(defvar translation-regexp-rules-alist)

(defun translation-phrases-to-rules (fun phrases)
  (mapcar (lambda (phrase)
            (list (regexp-quote phrase)
                  `(lambda (&rest args)
                     (,fun (match-string 0)))))
          phrases))

(setq month-regexp
      (s-join "\\|"
              (cl-loop for mon in
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
    (cl-loop for (from . to) in translation-regexp-rules-alist
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

(defun translation--read-file-name (&optional prompt)
  (unless prompt (setq prompt "enter filename: "))
  (if (eq major-mode 'dired-mode)
      (dired-file-name-at-point)
    (read-file-name prompt)))

(defun translation-new-from-file (input-filename &optional correction-p)
  (interactive (list (translation--read-file-name) prefix-arg))
  (let* ((txt-filename
          (convert-to-txt
           input-filename
           (lambda (filename)
             (translation-santize-subject (f-base filename) t))))
         (text-original (debian-file->string txt-filename t))
         (fun (if correction-p 'translation-new-correction 'translation-new)))
    (apply fun txt-filename text-original nil)))

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
  (interactive (list (translation--read-file-name)))
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
  (let ((default-directory (expand-file-name directory)))
    (check-cmd "git" '("status") "git not initialized")
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

;;;###autoload
(defun translation-setup ()
  (interactive)
  (compile "sudo apt-get install -y wdiff docx2txt poppler-utils")
  (if-let* ((api-key-path (-> user-init-file
                            (file-truename)
                            f-dirname
                            (f-join "vars" "google-translate.el")))
            ((not (file-exists-p api-key-path)))
            (api-key (read-string "enter google translate api key: ")))
      (with-current-buffer (find-file-noselect api-key-path)
        (-> `(setq babel-google-translate-api-key
                   ,api-key)
          prin1-to-string
          insert)
        (save-buffer)))
  (if-let* ((git-repo-path (expand-file-name "~/git/translations"))
            ((not (file-exists-p git-repo-path)))
            (repo-url (read-string "enter translations repository url: "))
            (default-directory (f-dirname git-repo-path)))
      (async-shell-command (format "git clone %s" repo-url))))

(define-minor-mode translation-mode
  "Translation minor mode"
  :lighter "translation-mode"
  (buttons-make
   ((kbd "s-t")
    (buttons-make
     ((kbd "s-l") #'lingee-translate))))
  (visual-line-mode t))

(defun translation-mode-maybe-enable ()
  (when (and (buffer-file-name)
             (s-starts-with?
              (expand-file-name "~/git/translations")
              (buffer-file-name)))
    (translation-mode t)))

(add-hook 'find-file-hook #'translation-mode-maybe-enable)

(autobuild-define-rule autobuild-translation-prepare (translation-mode)
  (autobuild-nice 7)
 #'translation-prepare)

(autobuild-define-rule autobuild-translation-publish-commit (translation-mode)
  #'translation-publish-commit)

(autobuild-define-rule autobuild-translation-reply (translation-mode)
  #'translation-reply)

(provide 'translation)
