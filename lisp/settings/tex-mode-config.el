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

(defun tex-include-graphics (filename)
  (buttons-template-insert
   "\\begin{" "figure}[H]"
   (newline-and-indent)
   "\\includegraphics[width=\\linewidth]{"
   filename
   "}"
   (newline-and-indent)
   "\\caption{" (recursive-edit) "}" (newline)
   "\\end{" "figure}"
   nil))

;;require this later in case it's not available
