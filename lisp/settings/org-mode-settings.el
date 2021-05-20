(require  'org)

(setq org-blank-before-new-entry
      ;;don't add extra newlines
      '((heading . nil)
	(plain-list-item . nil))
      ;;don't cut the current line
      org-M-RET-may-split-line '((default . nil )))

(setq org-startup-folded nil)

(setf org-top-dir
      (f-expand "~/private-data/org"))

(setf *org-todo-first-todo-line-number* 3)

(defun org-todo-promote-top ()
  (interactive)
  (save-excursion
    (while (org-move-subtree-up))))


(setf search-invisible nil)
(setf org-hide-leading-stars t)

(defun org-listify-region (a b)
  (interactive "r")
  (save-excursion
    (goto-char a)
    (goto-char (line-beginning-position))
    (setf b (save-excursion (goto-char b)
			    (line-end-position)))
    (while (and (re-search-forward "^\\([-*]+\\)" nil t)
		(< (match-end 0) b))
      (replace-match
       (concat (make-string (1- (length (match-string 1))) 32) "-")))))

(setq org-html-validation-link nil)

(def-file-local-toggle-command org-treat-as-readme-p)

(defun maybe-export-to-markdown ()
  (when (and (eq major-mode 'org-mode)
	     (or (equal (f-filename (buffer-file-name))
			"README.org")
		 (and (boundp 'org-treat-as-readme-p)
		      org-treat-as-readme-p)))
    (let ((dont-ask-user-about-supersession-threat t))
      (org-md-export-to-markdown)
      (org-html-export-to-html))))

(add-hook 'after-save-hook 'maybe-export-to-markdown)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance
      '("crypt"))
(require 'epa)
(setq org-crypt-key "AFF54E1E");; erjoalgo@gmail.com



(when (file-exists-p org-top-dir)
  (cl-pushnew org-top-dir org-agenda-files :test 'equal)
  '(org-todo-list org-match)

  (setq initial-buffer-choice
	(lambda ()
	  (call-interactively 'org-agenda-list)
	  (get-buffer "*Org Agenda*")))

  '(switch-to-buffer "*Org Agenda*")
  '(delete-other-windows))

(defun org-archive-done-tasks (arg)
  ;;taken from
  ;;http://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  (interactive "P")
  (let ((scope (if arg 'file 'agenda)));('tree 'file 'agenda)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setf org-map-continue-from (outline-previous-heading)))
     "/DONE" scope)
    (when (fboundp 'check-unsaved-buffers)
      (check-unsaved-buffers))))

(defun org-insert-inline-image (caption filename width)
  (interactive
   (let* ((filename (let ((cand (last-scrot-filename)
                                ;; (gui-get-selection 'CLIPBOARD)
                                )
                          default-dir initial)
                      (when (and cand (file-exists-p cand))
                        ;;TODO
                        (setf default-dir (concat (f-dirname cand) "/")
                              initial (f-filename cand)))
                      (read-file-name  "enter image filename: " default-dir cand t  initial)))
          (caption (read-string "enter caption for image: " (f-base filename)))
          (width (if current-prefix-arg
                     (read-number "width (in px): " 0)
                   0)))
     (list caption (expand-file-name filename) (when (not (zerop width)) width))))

  "     #+CAPTION: This is the caption for the next figure link (or table)
     #+NAME:
     [[./img/a.jpg]]"
  (when (bound-and-true-p org-inline-image-directory)
    (cl-assert (file-directory-p org-inline-image-directory))
    (let ((original filename))
      (setq filename (f-join org-inline-image-directory
                             (org-sanitize-filename (f-filename filename))))
      (call-process "mv" nil nil nil
                    original filename)))
  (cl-assert (file-exists-p filename))
  (insert "#+CAPTION: " (or caption "")) (newline-and-indent)
  (when width
    (insert (format "#+ATTR_HTML: :width %d" width)) (newline-and-indent))
  (insert (format "[[file:%s]]" filename))
  (newline-and-indent))

(def-file-local-set-command org-inline-image-directory)

(defun file-modification-timestamp (filename)
  (string-to-number
   (shell-command-to-string
    (format "stat '%s' -c '%%Y'" filename))))

(defalias 'sort-by #'sort-key)

(defun most-recent-file-name (files &optional nth)
  ;; TODO optimize
  (nth (or nth 0)
       (sort-by files #'file-modification-timestamp :descending t)))

(defvar auto-scrots-dirs
  (list
   (f-expand "~/pictures/auto-scrots")
   (f-expand "~/Downloads")))


(defun directory-files-exclude-dots (top)
  (cl-remove-if (lambda (filename) (member filename '("." "..")))
	     (directory-files top)))


(defun list-directories-flatten (top-dirs)
  (cl-loop for top in top-dirs nconc
	(cl-loop for basename in (directory-files-exclude-dots top)
	      collect (f-join top basename))))

(defun most-recent-file-name-in-directories (top-dirs &optional nth)
  (-> (list-directories-flatten top-dirs)
    (most-recent-file-name nth)))

(defun last-scrot-filename ()
  (most-recent-file-name-in-directories auto-scrots-dirs))

(defun org-insert-last-scrot (&optional caption)
  "also move last scrot to current directory"
  (interactive (list (read-string "enter caption for image: "
				  (x-get-clipboard))))
  (let* ((filename (last-scrot-filename))
	 (basename (f-filename filename))
	 (basename-noext (f-no-ext basename))
	 (caption (or caption
		      (replace-regexp-in-string "-" " " basename-noext)))
	 (destination "images")
	 (org-image-filename (concat "./"
				     (f-join "./" destination basename))))
    (unless (member (downcase (f-ext filename)) '("jpeg" "jpg" "png" "gif"
						  "svg"))
      (error "last file is not an image: %s" filename))
    (unless (file-exists-p destination)
      (make-directory destination))
    (unless (zerop (call-process "mv" nil t nil "-t" destination filename))
      (error "failed to  move %s to %s" filename default-directory))
    (org-insert-inline-image caption org-image-filename)))


(defun my-org-shift-right (arg a b)
  (interactive "p\nr")
  (save-excursion
    (goto-char a)
    (cl-loop with offset = 0
          with len = (or arg 1)
          while (re-search-forward "^\\(.\\)?" (+ b offset) t)
          as new-char = (if (equal (match-string 1) "*") "*" " ")
          as rep = (concat (match-string 0)
                           (make-string len (string-to-char new-char)))
          do (replace-match rep nil t)
          do (incf offset len))))


(defun my-org-shift-left (arg a b)
  (interactive "p\nr")
  (save-excursion
    (replace-regexp (format "^ \\{%d\\}" (or arg 1))
		    ""
		    nil a b)))

(defun org-texinfo-export-to-texinfo-and-html ()
  (interactive)
  (org-texinfo-export-to-texinfo)
  (let ((texi-file (concat
		    (f-base (buffer-file-name)) ".texi")))
    (shell-command (format (concat "texi2html " texi-file
				   " --no-number-sections"
				   " --split node"
				   " --css-include img.css")
			   ))
    ))

(defun my-org-redisplay-inline-images (limit)
  (when (and org-inline-image-overlays
	     (eq 'org-self-insert-command this-command)
	     (eql 91 (char-after)))
    (org-redisplay-inline-images)))

(add-hook 'org-font-lock-hook
	  'my-org-redisplay-inline-images)

(defun org-toggle-list-heading ()
  (interactive)
  (let* ((elm-at-point (org-element-at-point))
	 (at-list-p (cl-case (car elm-at-point)
		      ((item paragraph) t)
		      (headline nil)
		      (t (error "unknown element %s: "
				(car elm-at-point))))))
    (save-excursion
      (save-match-data
	(re-search-backward "^\\([*]+\\|\\([ ]+[-]\\)\\)"
			    (line-beginning-position))
	(let* ((matched-len (length (match-string 0)))
	       (new-string (if at-list-p
			       (make-string matched-len
					    (string-to-char "*"))
			     (concat
			      (make-string (1- matched-len)
					   (string-to-char " ")) "-"))))
	  (replace-match new-string t t))))))

(defun org-insert-file-link (filename)
  (interactive "fenter filename: ")
  (org-insert-link filename filename (f-base filename)))

(with-eval-after-load (symbol-file 'org-mode)
  (require 'ox-texinfo))

(setq org-refile-targets
      '(("master.org" :maxlevel . 2)
        ("work.org" :maxlevel . 2)))

(defun region-or-clipboard ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (car kill-ring)))

(defun org-paste-code-block (mode code indent-level)
  (interactive (let ((mode (read-symbol-completing "select mode: "))
                     (code (region-or-clipboard)))
                 (list
                  (replace-regexp-in-string "-mode" "" (symbol-name mode))
                  code
                  (current-column))))
  (let* ((SPACE 32)
         (leading-ws (make-string indent-level SPACE))
         (text
          (concat "#+BEGIN_SRC " mode "\n"
                  (->>
                   (concat code
                           "\n" "#+END_SRC")
                   (replace-regexp-in-string "^" leading-ws)))))
    (insert text)))

(defun org-sanitize-filename (filename)
  (replace-regexp-in-string "[^a-zA-Z0-9._/-]" "-" filename))

(defun org-sanitize-filenames ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while  (re-search-forward "file:\\(.*\\)]]" nil t)
      (replace-match (concat "file:"
                             (org-sanitize-filename (match-string 1))
                             "]]")))))


(defvar yt-iframe-format
  ;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 ;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))
