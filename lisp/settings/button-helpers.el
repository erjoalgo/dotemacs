(defun describe-function-at-point ()
  (interactive)
  (describe-function (function-called-at-point)))

(defun xml-toggle-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
         "^[[:space:]]*\\(<!--\\(.*\\)-->\\)[[:space:]]*"
         (line-end-position) t)
        (replace-match (match-string 2) nil t nil 1)
      (progn
        (re-search-forward "^[[:space:]]*\\(.*\\)[[:space:]]*"
                           (line-end-position) t)
        (replace-match (format "<!--%s-->" (match-string 1)) nil t nil 1)))))

'(setq go-types '("struct" "int" "bool" "string" "float"))

(defun my-comment-out (arg &optional duplicate respect-whitespace) (interactive "P")
       (let* ((mode-map-keymap-sym
               (intern (concat (symbol-name major-mode) "-map")))
              (comment-cmd (when (boundp mode-map-keymap-sym)
                             (lookup-key (symbol-value mode-map-keymap-sym)
                                         (kbd "s-/")))))
         (when (and comment-cmd (not (eq comment-cmd (function my-comment-out))))
           (call-interactively comment-cmd)
           (return)))
       (let ((comment-start (or comment-start "# "))
             (start-end (if mark-active
                            (cons (region-beginning)
                                  (region-end))
                          (cons
                           (line-beginning-position)
                           (save-excursion
                             (when arg (next-logical-line (1- arg)))
                             (point))))))
         (let* ((comment-start (or comment-start "# "))
                (start (save-excursion
                         (goto-char (car start-end))
                         (line-beginning-position)))
                (end (save-excursion
                       (goto-char (cdr start-end))
                       (line-end-position)))
                (comment-regexp (concat
                                 "\\`[[:space:]]*"
                                 (if (not respect-whitespace)
                                     (replace-regexp-in-string
                                      "[ 	]+" "[[:space:]]*"
                                      (regexp-quote comment-start))
                                   (regexp-quote comment-start))))
                (text (buffer-substring-no-properties start end))
                (is-commented (string-match comment-regexp text))
                (comment-end ""))
           (if (zerop (length text))
               (insert (concat comment-start
                               (when (and comment-add (> comment-add 0))
                                 comment-start)
                               (unless (s-ends-with-p " " comment-start) " ")))
             (funcall (if is-commented 'uncomment-region 'comment-region)
                      start end nil))
           (when duplicate
             (goto-char end)
             (end-of-line)
             (open-line 1)
             (next-line 1)
             (insert text)))))

(defun my-comment-out-and-duplicate (arg)
  (interactive "P")
  (my-comment-out arg t))

(defun migrate-buttons (from-file to-file)

  (defun map-buttons (source-file)
    (cl-labels
        ((rec (form)
              (cond
               ((atom form) form)
               (t (cl-case (car form)
                    (mk-cmd
                     (cl-loop with curr = ""
                           with ret = '(cmd)
                           for (act . rest) in (append (cdr form)  '(nil))
                           when (and (eq act 'ins) (string-match "{" (car rest)))
                           do (setf act 'insert)
                           do (cl-case act
                                (ins
                                 (cl-assert (null (cdr rest)))
                                 (setf curr (concat curr (car rest))))
                                (rec (setf curr (concat curr "{}")))
                                (var-rec (setf curr (concat curr "{0}")))
                                (var-ins (setf curr (concat curr "{0}")))
                                (cbd (setf curr (concat curr "{cbd}")))
                                (idt (setf curr (concat curr "{idt}")))
                                (scn (setf curr (concat curr ";{nli}")))
                                (py-scn (setf curr (concat curr ":{nli}")))
                                (nli (setf curr (concat curr "{nli}")))
                                (inm (setf curr (concat curr "{inm}")))
                                (py-bck (setf curr (concat curr "{idt}")))
                                (var-pop nil)
                                (t
                                 (when (not (zerop (length curr)))
                                   (push (if (equal curr "{}") `(rec)
                                           `(ins ,curr))
                                         ret)
                                   (setf curr ""))
                                 (when act
                                   (if (eq 'evl act)
                                       (progn
                                         (cl-assert (null (cdr rest)))
                                         (push (car rest) ret))
                                     (push `(,act ,@rest) ret)))))
                           finally (cl-return (reverse ret))))
                    (defbuttons (cl-destructuring-bind (sym modes parent body) (cdr form)
                                  `(defbuttons ,sym
                                     ,parent
                                     ,(if (and modes (atom modes)) (list modes) modes)
                                     ,(rec body))))
                    (t
                     ;; (mapcar 'rec form)
                     (cl-loop for sub in form collect (rec sub))
                     ))))))
      (let* ((contents
              (format "(progn %s)" (debian-file->string from-file)))
             (read (read contents)))
        (rec read))))


  (setf print-level nil)
  (let ((print-level nil))
    (find-file to-file)
    (let ((contents (pp-to-string
                     `(buttons-macrolet ()
                           ,@(cdr (map-buttons "buttons-data.el"))))))
      (with-current-buffer to-file
        (erase-buffer)
        (insert contents)
        (goto-char (point-min))
        (while (re-search-forward "(\\(\".*?\"\\|(kbd.*?)\\)
[ 	]+(cmd
[ 	]+\\(.*\\)" nil t)
          (replace-match "(\\1 (cmd \\2"))

        (goto-char (point-min))
        (while (re-search-forward "(\\(kbd.*?)\\)
[ 	]+\\('.*\\)" nil t)
          (replace-match "(\\1 \\2"))
        (save-buffer)))))

' (migrate-buttons "buttons-data.el.bak" "buttons-data-new.el")
