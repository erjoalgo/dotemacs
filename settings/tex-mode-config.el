(require 'f)


(defun latex-compile ()
  (interactive)
  (let* ((tex (buffer-file-name (current-buffer)))
	 (base-sans-ext (f-base tex))
	 (pdf (concat base-sans-ext ".pdf"))
	 (compile-errors-buffer "*TEX-COMPILE-ERRORS*")
	 (async-shell-command-buffer 'new-buffer)
	 (ret-code (call-process "pdflatex" nil compile-errors-buffer nil "-halt-on-error" tex)))

    (with-current-buffer compile-errors-buffer (erase-buffer))
    
    (if (= ret-code 0)
	(progn
	  (message "successful compilation")
	  (let ((win (wm-windows-find-window-by-title-regexp (regexp-quote base-sans-ext))))
	    (if win (wm-window-raise win)
	      (start-process "view-pdf" "view-pdf" "zathura" pdf))))
      (save-excursion
	(switch-to-buffer-other-window compile-errors-buffer)
	(goto-char (point-max))
	(other-window -1)))))



(add-hook 'tex-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'latex-compile nil t)))

;;require this later in case it's not available
(require 'wm-windows)
