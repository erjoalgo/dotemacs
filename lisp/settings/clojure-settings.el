(ensure-packages-exist
 '(clojure-mode cider))

(setf cider-repl-display-help-banner nil )
(setf cider-show-error-buffer nil)
;;https://github.com/bbatsov/projectile

(defun cider-buffer-or-jack-in ()
  (interactive)
  (let ((ciders (remove-if-not (lambda (buf)
			  (s-starts-with? "*cider-repl" (buffer-name buf)))
			(buffer-list))))
    (if ciders (switch-to-buffer (car ciders))
      (cider-jack-in))))

(with-eval-after-load "cider-repl"
  (define-key cider-repl-mode-map (kbd "M-p") (lambda () (interactive)
					       (cider-repl--history-replace 'backward nil)))
  (define-key cider-repl-mode-map (kbd "M-n") (lambda () (interactive)
						(cider-repl--history-replace 'forward nil)))

  (define-key cider-repl-mode-map (kbd "M-?") (lambda () (interactive)
						(switch-to-buffer "*cider-error*")))
  '(define-key clojure-mode-map (kbd "TAB") 'company-complete)
  )


(setf nrepl-prompt-to-kill-server-buffer-on-quit nil)


(defun clojure-opts-summary-to-org-table (path-to-jar)
  (interactive "fenter path to jar, should accept -h option: ")
 ;TODO align descriptions in the correct column
  (let* (
	(out "  -e, --email EMAIL                          erjoalgo@gmail.com                email address
  -d, --db DB                                /home/ealfonso/.imap-contacts.db  path to sqlite db
  -m, --max-results MAX                      600                               max results to fetch, default 600, 0 for infinite
  -p, --passwd-file PASSWD_FN                                                  path to file containing app specific pass. user is prompted if not provided
  -n, --newline                                                                flag to insert newlines instead of \r
  -q, --quiet                                                                  quiet
  -s, --imap-protocol-host-port IMAP_SERVER  [\"https\" \"imap.gmail.com\" 993]    url for for imap server including protocol, host, port, example 'https://imap.gmail.com:993'")
	(out (shell-command-to-string (format "java -jar %s -h" path-to-jar)))

	)
    (insert (concat " | Option | Default | Description | \n"
	    (replace-regexp-in-string " \\{2,\\}" " | " out)))))

(defface cider-repl-err-output-face
  '((t (:inherit font-lock-warning-face)))
  "Face for STDERR output in the REPL buffer."
  :group 'cider-repl
  :package-version '(cider . "0.6.0"))

(define-key clojure-mode-map (kbd "TAB") 'completion-at-point)
