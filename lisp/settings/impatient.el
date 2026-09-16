;; Core packages
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 8017)   ; pick any free local port
  (setq httpd-host "127.0.0.1"))  ; explicitly bind to localhost only

(use-package impatient-mode
  :ensure t)

;; Tie markdown rendering into impatient-mode
(defun my/markdown-html (buffer)
  "Render markdown BUFFER to HTML using a local converter."
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><head><meta charset=\"utf-8\">
                     <style>
                       body { max-width: 800px; margin: 40px auto; font-family: sans-serif; line-height: 1.6; padding: 0 20px; }
                       pre { background: #f4f4f4; padding: 10px; overflow-x: auto; }
                       code { background: #f4f4f4; padding: 2px 4px; }
                       blockquote { border-left: 4px solid #ddd; margin-left: 0; padding-left: 15px; color: #555; }
                     </style></head><body>%s</body></html>"
                   (shell-command-to-string
                    (format "pandoc -f markdown -t html %s"
                            (shell-quote-argument (or buffer-file-name ""))))))
         (current-buffer)))

(defun my/markdown-preview ()
  "Start local impatient-mode preview for the current markdown buffer."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode 1)
  (setq imp-user-filter #'my/markdown-html)
  (let ((url (format "http://127.0.0.1:%d/imp/live/%s/" httpd-port (buffer-name))))
    (message "Preview at %s" url)
    (browse-url url)))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-v") #'my/markdown-preview))
