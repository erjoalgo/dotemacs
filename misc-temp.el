;;minibuffer-local-map
(load-file
 (f-join emacs-top "in-progress/python-buttons.el"))

(add-hook 'ielm-mode-hook 'elisp_install_buttons)
(add-hook 'slime-repl-mode-hook 'elisp_install_buttons)
(add-hook 'emacs-lisp-mode-hook 'elisp_install_buttons)
(add-hook 'lisp-mode-hook 'cl_install_buttons)
(add-hook 'go-mode-hook 'go_install_buttons)
(add-hook 'c++-mode-hook 'cpp_install_buttons)
(add-hook 'ess-mode-hook 'r_install_buttons)

(add-hook 'lisp-mode-hook 'slime-mode)
(elisp_install_buttons read-expression-map)

;(add-hook 'shell-script-mode-hook 'bash_install_buttons)
(add-hook 'sh-mode-hook 'bash_install_buttons)



(defun upcase-last ()
  (interactive)
  (save-excursion
    (upcase-region (point)
		 (progn
		   (backward-sexp)
		   (point)))))

(defun bash-identifier-current-line  ()
  (let ((line
	(buffer-substring-no-properties
	 (point)
	 (line-beginning-position))))
    (and
     (string-match
      "^[[:space:]]*\\([^=]+\\)="
      line)
     (match-string 1 line))))

(defun firefox-new-tab (url &optional unknown-arg)
  (let ((new-tab "netcat-firefox-mozrepl"))
    (start-process new-tab new-tab
		   "nc" "localhost" "4242" "-q" "1")
    (process-send-string new-tab
			 (format
			  ;;newline is important
			  "gBrowser.selectedTab = gBrowser.addTab(\"%s\");\n"
			  url))))

(defun process-filter-line-buffer (real-filter)
	 (let ((cum-string-sym (gensym "proc-filter-buff"))
	       (newline (string-to-char "\n"))
	       (string-indexof (lambda (string char start)
				 (loop for i from start below (length string)
				       thereis (and (eq char (aref string i))
						    i)))))
	   (set cum-string-sym "")
	   `(lambda (proc string)
	      (setf string (concat ,cum-string-sym string))
	      (let ((start 0) new-start)
		(while (setf new-start
				(funcall ,string-indexof string ,newline start))

		  ;;does not include newline
		  (funcall ,real-filter proc (substring string start new-start))

		  (setf start (1+ new-start)));;past newline

		(setf ,cum-string-sym (substring string start))))))

(setq browse-url-browser-function 'firefox-new-tab)

(defun peek (str start max)
  "peek into str at most `max' characters"
  (substring str start (min (length str) (+ start max))))
