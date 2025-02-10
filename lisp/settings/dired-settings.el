;;don't prompt me
;;todo this isn't working
(defadvice dired-do-async-shell-command (around no-prompt activate)
  (let* ((async-shell-command-buffer 'new-buffer))
    ad-do-it))



(defun dired-recursive-du ()(interactive)
       (let ((dired-dir dired-directory)
	     (du-buffer "dired-rec-du"))
	 (shell-command (format "du -ah --max-depth 1 %s | sort -h" dired-dir) du-buffer du-buffer )
	 (set-buffer du-buffer )
	 (replace-string dired-dir "" nil (point-min) (point-max))
	 (temp-buffer-window-show du-buffer)
	 (goto-char (point-max))))

(setf dired-tmp-char 31)
(setf dired-mark-char 42)

(defun dired-tagger-read-char ()
  (let ((char (read-char "enter lowercase char, q to quit: ")))
    (unless (member char '(Quit 113)) char)))

(defun dired-mark-file-as (dest-char)
  (interactive (list (dired-tagger-read-char)))
  (when dest-char
    (dired-change-marks dired-mark-char dired-tmp-char)
    (dired-mark nil)
    (dired-change-marks dired-mark-char dest-char)
    (dired-change-marks dired-tmp-char dired-mark-char)
    t))

(defun dired-tagger-tag-loop (&optional open)
  (interactive "P")
  (cl-loop
   as buf = (when open (dired-find-file))
   as char = (dired-tagger-read-char)
   when buf do (kill-buffer buf)
   while (and char (dired-mark-file-as char))))

(defun dired-kill-marked-filenames ()
  (interactive)
  (->> (dired-remember-marks (point-min) (point-max))
       (mapcar (lambda (item)
                 (f-filename (car item))))
       (s-join ", ")
       (kill-new)))

(defun dired-tagger-move ()
  (interactive)

  (let* ((arr (make-vector 256 nil)))

    (cl-loop for (file . mark) in (dired-remember-marks (point-min) (point-max))
             do (cl-assert mark)
             if (eq mark dired-mark-char) do
             (error "remove the standard mark from all files first: %s"
                    file)
             else do
             (push file (aref arr mark)))

    (cl-loop for files across arr
             for mark from 0
             if files do
             (progn (message "mark: %s: %s" mark files)
                    '(let ((dest
                            (find-file ))))
                    (message "files '%s' are: %s"
                             (char-to-string mark)
                             (s-join "\n" files))
                    (dired-change-marks mark dired-mark-char)
                    (dired-do-rename)))))

(setq image-dired-show-all-from-dir-max-files 2000)

(setq dired-create-destination-dirs 'always)

(defun dired-sort-toggle--keep-cursor-position (oldfun &rest r)
  (let ((old-line-number (line-number-at-pos))
        (old-column-number (- (point) (line-beginning-position))))
    (prog1
        (apply oldfun r)
      (goto-char (point-min))
      (next-line old-line-number)
      (forward-char old-column-number))))


(advice-add #'dired-sort-toggle-or-edit
            :around
            #'dired-sort-toggle--keep-cursor-position)


(defun my-rename-file (file newname ok-if-already-exists)
  (call-process "mv" nil "*mv*" t file newname))

(defun dired-rename-file (file newname ok-if-already-exists)
  "Rename FILE to NEWNAME.
Signal a `file-already-exists' error if a file NEWNAME already exists
unless OK-IF-ALREADY-EXISTS is non-nil."
  (let ((file-is-dir-p (file-directory-p file)))
    (dired-handle-overwrite newname)
    (dired-maybe-create-dirs (file-name-directory newname))
    (if (and dired-vc-rename-file
             (vc-backend file)
             (ignore-errors (vc-responsible-backend newname)))
        (vc-rename-file file newname)
      ;; error is caught in -create-files
      (my-rename-file file newname ok-if-already-exists))
    ;; Silently rename the visited file of any buffer visiting this file.
    (and (get-file-buffer file)
         (with-current-buffer (get-file-buffer file)
	   (set-visited-file-name newname nil t)))
    (dired-remove-file file)
    ;; See if it's an inserted subdir, and rename that, too.
    (when file-is-dir-p
      (dired-rename-subdir file newname))))


(defun dired-do-create-files (op-symbol file-creator operation arg
					&optional marker-char op1
					how-to)
  "Create a new file for each marked file.
Prompt user for a target directory in which to create the new
  files.  The target may also be a non-directory file, if only
  one file is marked.  The initial suggestion for target is the
  Dired buffer's current directory (or, if `dired-dwim-target' is
  non-nil, the current directory of a neighboring Dired window).
OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  will determine whether pop-ups are appropriate for this OP-SYMBOL.
FILE-CREATOR and OPERATION as in `dired-create-files'.
ARG as in `dired-get-marked-files'.
Optional arg MARKER-CHAR as in `dired-create-files'.
Optional arg OP1 is an alternate form for OPERATION if there is
  only one file.
Optional arg HOW-TO determines how to treat the target.
  If HOW-TO is nil, use `file-directory-p' to determine if the
   target is a directory.  If so, the marked file(s) are created
   inside that directory.  Otherwise, the target is a plain file;
   an error is raised unless there is exactly one marked file.
  If HOW-TO is t, target is always treated as a plain file.
  Otherwise, HOW-TO should be a function of one argument, TARGET.
   If its return value is nil, TARGET is regarded as a plain file.
   If it return value is a list, TARGET is a generalized
    directory (e.g. some sort of archive).  The first element of
    this list must be a function with at least four arguments:
      operation - as OPERATION above.
      rfn-list  - list of the relative names for the marked files.
      fn-list   - list of the absolute names for the marked files.
      target    - the name of the target itself.
    The rest of elements of the list returned by HOW-TO are optional
    arguments for the function that is the first element of the list.
   For any other return value, TARGET is treated as a directory."
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-get-marked-files nil arg nil nil t))
	 (rfn-list (mapcar #'dired-make-relative fn-list))
	 (dired-one-file	; fluid variable inside dired-create-files
	  (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	 (target-dir (dired-dwim-target-directory))
	 (default (and dired-one-file
		       (not dired-dwim-target) ; Bug#25609
		       (expand-file-name (file-name-nondirectory (car fn-list))
					 target-dir)))
	 (defaults (dired-dwim-target-defaults fn-list target-dir))
	 (target (expand-file-name ; fluid variable inside dired-create-files
		  (minibuffer-with-setup-hook
		      (lambda ()
                        (setq-local minibuffer-default-add-function nil)
			(setq minibuffer-default defaults))
		    (dired-mark-read-file-name
                     (format "%s %%s %s: "
                             (if dired-one-file op1 operation)
                             (if (memq op-symbol '(symlink hardlink))
                                 ;; Linking operations create links
                                 ;; from the prompted file name; the
                                 ;; other operations copy (etc) to the
                                 ;; prompted file name.
                                 "from" "to"))
		     target-dir op-symbol arg rfn-list default))))
	 (into-dir
          (progn
            (unless
                ;; dired-one-file
                nil
              (dired-maybe-create-dirs target))
            (cond ((null how-to)
		   ;; Allow users to change the letter case of
		   ;; a directory on a case-insensitive
		   ;; filesystem.  If we don't test these
		   ;; conditions up front, file-directory-p
		   ;; below will return t on a case-insensitive
		   ;; filesystem, and Emacs will try to move
		   ;; foo -> foo/foo, which fails.
		   (if (and (file-name-case-insensitive-p (car fn-list))
			    (eq op-symbol 'move)
			    dired-one-file
			    (string= (downcase
				      (expand-file-name (car fn-list)))
				     (downcase
				      (expand-file-name target)))
			    (not (string=
				  (file-name-nondirectory (car fn-list))
				  (file-name-nondirectory target))))
		       nil
		     (file-directory-p target)))
		  ((eq how-to t) nil)
		  (t (funcall how-to target))))))
    (if (and (consp into-dir) (functionp (car into-dir)))
	(apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
      (if (not (or dired-one-file into-dir))
	  (error "Marked %s: target must be a directory: %s" operation target))
      (if (and (not (file-directory-p (car fn-list)))
               (not (file-directory-p target))
               (directory-name-p target))
          (error "%s: Target directory does not exist: %s" operation target))
      ;; rename-file bombs when moving directories unless we do this:
      (or into-dir (setq target (directory-file-name target)))
      (prog1
          (dired-create-files
           file-creator operation fn-list
           (if into-dir			; target is a directory
	       ;; This function uses fluid variable target when called
	       ;; inside dired-create-files:
	       (lambda (from)
	         (expand-file-name (file-name-nondirectory from) target))
	     (lambda (_from) target))
           marker-char)
        (when (or (eq dired-do-revert-buffer t)
                  (and (functionp dired-do-revert-buffer)
                       (funcall dired-do-revert-buffer target)))
          (dired-fun-in-all-buffers (file-name-directory target) nil
                                    #'revert-buffer))))))
