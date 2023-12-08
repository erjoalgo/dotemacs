(require 'f)
(condition-case ex
    (require 'wm-windows)
  (error (warn "wm-windows not available: %s" ex)))


(defun latex-compile ()
  (interactive)
  (let* ((tex (buffer-file-name (current-buffer)))
	 (base-sans-ext (f-base tex))
	 (pdf (concat base-sans-ext ".pdf"))
	 (compile-errors-buffer "*TEX-COMPILE-ERRORS*"))

    (ispell)
    (when (get-buffer compile-errors-buffer)
      (with-current-buffer compile-errors-buffer
	(erase-buffer)))

    (call-process "pdflatex" nil compile-errors-buffer nil
			      "-halt-on-error" tex)

    (async-start
     ;; What to do in the child process
     `(lambda ()
        (let* ((async-shell-command-buffer 'new-buffer)
               (ret-code
                (call-process "pdflatex" nil ,compile-errors-buffer nil
			      "-halt-on-error" ,tex)))
          ret-code))

     ;; What to do when it finishes
     `(lambda (ret-code)
       (if (= ret-code 0)
	   (progn
	     (message "successful compilation")
             (let ((bcf (format "%s.bcf" ,base-sans-ext)))
               (when (file-exists-p bcf)
                 (message "running biber on %s" bcf)
                 (start-process "biber" "*biber*" "biber" bcf)))
	     (let ((win
                    (cl-loop for win in (wm-windows-list)
	                     thereis
                             (and
                              (string-match (regexp-quote ,base-sans-ext)
                                            (wm-window-title win))
                              (let ((case-fold-search t))
                                (string-match-p ".*zathura.*" (wm-window-class win)))
                              win))))
	       (if win
                   (wm-window-raise win)
	         (start-process "view-pdf" "view-pdf" "zathura" ,pdf))))
         (save-excursion
	   (switch-to-buffer-other-window ,compile-errors-buffer)
	   (goto-char (point-max))
	   (other-window -1)))))))



(add-hook 'tex-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'latex-compile nil t)))

(defun f-remove-extension (filename)
  (let ((ext (f-ext filename)))
    (if (string-blank-p ext) filename
    (substring filename 0 (- (length filename) (1+ (length ext)))))))

(defun file-discover-extension (filename)
  (s-trim
   (shell-command-to-string
   (format "file --extension '%s' | cut -d: -f2 | cut -d/ -f1"
           filename))))

(defun filename-add-missing-extension (filename)
  (cl-assert (not (f-ext filename)))
  (let ((ext (file-discover-extension filename)))
    (rename-file filename (format "%s.%s" filename ext))))

(defun tex-include-graphics (filename &optional no-figure)
  (interactive)
  (unless (f-ext filename)
    (setq filename (filename-add-missing-extension filename)))
  (unless (member (downcase (f-ext filename)) '("jpeg" "png" "gif"))
    (let ((jpeg-filename (format "%s.jpeg" (f-remove-extension filename))))
      (unless (and
               ;; (y-or-n-p (format "convert %s into %s? " filename jpeg-filename))
               (zerop (call-process "convert" nil t nil filename jpeg-filename)))
        (error "failed to convert to known includegraphics extension"))
      (setq filename jpeg-filename)))
  (unless no-figure
    (insert "\\begin{figure}[H]")
    (newline-and-indent))
  (insert "\\includegraphics[width=\\linewidth]{" filename "}")
  (newline-and-indent)
  (unless no-figure
    (insert "\\caption{") (recursive-edit) (insert "}")
    (newline-and-indent)
    (insert "\\end{figure}")))

(defun tex-make-graphics-local ()
  (interactive)
  (let ((dest "./graphics"))
    (unless (file-exists-p dest)
      (make-directory dest))
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (re-search-forward "\\includegraphics[[][^]]*]{\\([^}]+\\)}" nil t)
               as filename = (match-string 1)
               as local = (f-join dest (f-filename filename))
               do (cond
                   ((not (file-exists-p filename))
                    (message "WARN: skipping non-existing file: %s" filename))
                   ((equal filename local)
                    (message "skipping already-coped file: %s" filename))
                   (t
                    (message "copying %s to %s" filename local)
                    (copy-file filename local t t t t)
                    (replace-match local t t nil 1)))))))

;;require this later in case it's not available
(defun tex-maybe-add-command (content)
  (save-excursion
    (goto-char (point-min))
    (unless
        (re-search-forward (regexp-quote content) nil t)
      (goto-char (point-min))
      (re-search-forward "^[\\]begin[{]document[}]")
      (newline-and-indent)
      (insert content))))

(defun tex-add-youtube-url (url)
  (interactive "senter youtube url: ")
  (tex-maybe-add-command
   "\\newcommand{\\hrefyt}[3]{
  \\href{#1}{
  \\includegraphics[width=\\linewidth]{#2}
  \\qrcode{#1}#3
  }
}")
  (let* ((id (or (yt-extract-video-id url)
                 (error "unable to extract youtube id from url: %s" url)))
         (image-url (youtube-image-url id))
         (filename (expand-file-name (format "~/Downloads/%s.jpeg" id)))
         (title (s-trim-right (shell-command-to-string (format "webpage-title.sh '%s'" url)))))
    (message "downloading youtube thumbnail from url: %s" image-url)
    (url-copy-file image-url filename t)
    (insert (format "\\hrefyt{%s}{%s}{%s}" url filename title))))
