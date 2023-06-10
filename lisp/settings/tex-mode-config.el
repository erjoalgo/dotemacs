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
	     (let ((win
                    (cl-loop for win in (wm-windows-list)
	                     thereis
                             (and
                              (string-match (regexp-quote ,base-sans-ext) (wm-window-title win))
                              (let ((case-fold-search t))
                                (string-match-p ".*zathura.*" "zathura.Zathura"))
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

(defun tex-include-graphics (filename)
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
  (insert "\\begin{figure}[H]")
  (newline-and-indent)
  (insert "\\includegraphics[width=\\linewidth]{" filename "}")
  (newline-and-indent)
  (insert "\\caption{") (recursive-edit) (insert "}") (newline-and-indent)
  (insert "\\end{figure}"))

;;require this later in case it's not available
