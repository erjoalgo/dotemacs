;;; misc-utils.el --- misc utils with no home
;;
;; Filename: misc-utils.el
;; Description:
;; Author: Ernesto Alfonso
;; Maintainer:
;; Created: Sun Dec 16 18:01:13 2018 (-0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'cl-lib)
(require 's)
(require 'f)

(defun whereis (program)
  "Search PATH for PROGRAM."
  (interactive
   (list (read-shell-command "enter program: ")))
  (let ((dirs
	 (cl-loop for dir in (split-string (getenv "PATH") ":" t)
	       if (and (file-exists-p dir)
		       (member program (directory-files dir)))
	       collect (f-join dir program))))

    (when (called-interactively-p 'interactively)
      (message "%s" dirs))
    dirs))

(defun recover-this-file-and-diff ()
  "(progn (recover-this-file) (diff-buffer-with-file)) ."
  (interactive)
  (recover-this-file);;TODO ignore prompt
  (diff-buffer-with-file))

(defun shell-command-of-region (a b)
  "Run the command specified in region A B, which defaults to the current line."
  (interactive "r")
  (let ((cmd (apply 'buffer-substring-no-properties
		    (if (region-active-p)
			(list a b)
		      (list (line-beginning-position)
			    (line-end-position)))))
	(buf "*Async Shell Command*")
	(kill-buffer-query-functions
	 (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (when (get-buffer buf)
      (kill-buffer buf))
    (message "cmd is: %s" cmd)
    (async-shell-command cmd)))

(defun shell-command-of-buffer ()
  "Like shell-command-of-region, with region as the entire buffer."
  (interactive)
  (shell-command-of-region (point-min) (point-max)))

(defun lnabs (dest &optional source)
  "Make DEST point to the absolute path of SOURCE."
  (interactive
   (let* ((initial
          (if (and (eq major-mode 'dired-mode)
		   (funcall 'dired-file-name-at-point))
	      (f-filename
	       (funcall 'dired-file-name-at-point))
            (buffer-file-name)))
         (source (read-file-name "Enter soft link source: " nil initial t initial))
         (dest (read-file-name "Enter soft link dest: ")))
     (list dest source)))

  (->
      (call-process "ln" nil "*lnabs*" nil
                    "-sf"
                    (expand-file-name source)
                    (expand-file-name dest))
    zerop
    assert))

(defvar *shred-rec-default-times* 10)

(defun shred-rec (fn &optional shred-times)
  "Recursively shred the file or directory FN, making SHRED-TIMES passes."
  (interactive (list
		(if (and
		     (eq major-mode 'dired-mode)
		     (funcall 'dired-file-name-at-point))
		    (f-filename
		     (funcall 'dired-file-name-at-point))
		  (read-file-name "enter fn to shred: "))))
  (unless shred-times
    (setf shred-times *shred-rec-default-times*))
  (y-or-n-p (format "Shred %s? " fn))
  (let* ((shred-times-string (int-to-string shred-times))
	 (shred-cmd-arg-list
	  (if (executable-find "shred")
	      `("shred" "-zufn" ,shred-times-string)
	    `("srm" "-z")))
	 (fn (expand-file-name fn))
	 (BUFNAME "shred"))

    (if (file-directory-p fn)
	(and (y-or-n-p (format "Recursively shred %s? " fn))
	     (progn
	       (apply 'start-process
		      `(BUFNAME BUFNAME "find" fn "-type" "f"
				"-exec" ,@shred-cmd-arg-list "{}" ";"))
	       (start-process BUFNAME BUFNAME "find" fn "-depth" "-type" "d"
			      "-exec" "rmdir" "{}" ";")))
      (unless (zerop (apply 'call-process (car shred-cmd-arg-list) nil BUFNAME t
			    (append (cdr shred-cmd-arg-list) (list fn))))
	(switch-to-buffer BUFNAME)
	(error "Error in shred")))
    (when (eq major-mode 'dired-mode)
      (call-interactively 'revert-buffer))))

(cl-defun sort-key (list key &key descending (pred '<))
  (let* ((sorted-tuples
	  (sort (mapcar (lambda (el)
			  (cons el (funcall key el))) list)
		(lambda (a b)
		  (funcall pred (cdr a) (cdr b)))))
	 (sorted (mapcar 'car sorted-tuples)))
    (if descending (reverse sorted)
      sorted)))

(defun directory-files-sort-by-ctime-descending (dir)
  "List files and directories in DIR sorted by ctime."
  (let ((files (directory-files dir)))
    (reverse (sort-key files (lambda (fn)
			       (let ((attrs (file-attributes (f-join dir fn))))
				 (nth 6 attrs)))))))

(defun read-symbol-completing (prompt &optional default)
  "Read a LISP symbol with completion, prompting with PROMPT and defaulting to DEFAULT."
  (intern
   (completing-read prompt obarray nil nil (and default (symbol-name default)))))

(defun file-local-variable-set-mode (mode)
  "Add ‘mode' = MODE as a file-local variable."
  (interactive (list (read-symbol-completing "enter mode: " major-mode)))
  (unless (or (eq mode major-mode)
              (member mode minor-mode-list))
    (funcall mode))
  (if (not (eq mode major-mode))
      (add-file-local-variable mode t)
    ;;first load mode to set the right comment-start
    (funcall mode)
    (let ((mode-sans-mode
	   (let ((mode-name (symbol-name mode)))
	     (string-match
	      "\\(.*\\)-mode"
	      mode-name)
	     (intern (match-string 1 mode-name)))))
      (add-file-local-variable 'mode mode-sans-mode))))

(defvar check-unsaved-buffers-current-buffer nil
  "The buffer being examined by a ‘check-unsaved-buffers' recursive edit.")

(setq check-unsaved-buffers-skip-buffers-regexp-list
  '("^[[:space:]]*[*].*[*]$"
    "^[ ]*[*]mm[*]-[0-9]+"
    "^[ ][*]nnimap"
    "^[ ][*]nnimap"
    "^ ?[*].*"
    "^irc.freenode.net:.*"
    "^#sip.*"))

(defun check-unsaved-buffers ()
  "Check buffers with changes not persisted in the filesystem."
  (interactive)
  (cl-loop as next-buff =
	(cl-loop for buff in (buffer-list)
	      thereis (and
		       (not (get-buffer-process buff))
                       (or (not (buffer-file-name buff))
			   (buffer-modified-p buff))
                       (not (member (buffer-local-value 'major-mode buff) '(dired-mode)))
                       (not
                        (cl-loop for re in
                              check-unsaved-buffers-skip-buffers-regexp-list
                              thereis (string-match  re (buffer-name buff))))
		       buff))
	while next-buff do
	(progn (switch-to-buffer next-buff)
	       (message "unsaved changes in: %s... close or save, then exit rec-edit"
			(buffer-name next-buff))
	       (let ((erjoalgo-command-mode-keep-state t))
                 (setf check-unsaved-buffers-current-buffer next-buff)
                 (recursive-edit)))
	finally (message "done checking buffers"))
  t)
(add-hook 'kill-emacs-query-functions 'check-unsaved-buffers)

(defadvice isearch-forward-regexp (around force-case-fold activate)
  "Force isearch case fold always."
  (let* ((case-fold-search t))
    (when current-prefix-arg
      (set-mark-command nil))
    ad-do-it))

(defadvice kill-buffer (around check-unsaved-buffers-auto-move-to-next-buffer activate)
  "When in a CHECK-UNSAVED-BUFFERS recurisve-edit, exit it to move on to the next buffer."
  '(message "on check-unsaved-buffers-auto-move-to-next-buffer: (equal %s %s) => %s. %s"
	   (current-buffer)
	   check-unsaved-buffers-current-buffer
	   (equal (current-buffer) check-unsaved-buffers-current-buffer)
	   this-command)
  (let ((is-unsaved-buffer (and check-unsaved-buffers-current-buffer
				(equal (current-buffer) check-unsaved-buffers-current-buffer))))
    ad-do-it
    (when is-unsaved-buffer
      (message "moving onto next check-unsaved-buffers buffer...")
      (exit-recursive-edit))))

(defun diff-sexps (sexp-a sexp-b)
  "Signal an error where SEXP-A, SEXP-B differ."
  (cl-loop for a in sexp-a
	for b in sexp-b
	do
	(if (not (eq (atom a) (atom b)))
	    (error "Mismatch: %s %s" a b)
	  (if (not (atom a))
	      (diff-sexps a b)
	    (unless (equal a b)
	      (error "Mismatch: %s %s" a b)))))
  (or (not (and (consp sexp-a) (consp sexp-b)))
      (= (length sexp-a) (length sexp-b))))

(defun lookup-key-in-current-maps (key)
  "Return a list of currently active keymaps that have a binding for KEY ."
  (interactive (list (read-key-sequence "enter key to lookup in current maps: ")))
  (let* ((kmaps-filtered
          (cl-loop for kmap in (current-active-maps)
                   with kvec = key
                   as binding =
                   (cl-loop with cum = nil
                            with curr = kmap
                            for k across kvec
                            do (push curr cum)
                            do (setq curr (lookup-key curr (vector k)))
                            while curr
                            when (not (keymapp curr))
                            do (let ((sym (keymap-symbol (list kmap))))
                                 (return (cons
                                          (mapcar #'keymap-symbol (mapcar #'list cum))
                                          curr))))
                   when binding
                   collect binding))
	 (kmap-to-key-alist kmaps-filtered))
    (message "%s" kmap-to-key-alist)
    kmap-to-key-alist))

(defun undefine-key (key kmap)
  "Interactively un-define KEY in KMAP."
  (interactive
   (let* ((key (read-key-sequence "enter key to lookup in current maps: "))
          (kmaps (lookup-key-in-current-maps key))
          ;; (kmap (selcand-select kmaps))
          (kmap-name (completing-read "select map: " (mapcar 'car kmaps) nil t))
          (kmap (symbol-value (intern kmap-name))))
     (list key kmap)))
  (cl-assert key)
  (cl-assert kmap)
  (cl-assert (lookup-key kmap key))
  (define-key kmap key nil))

(defun keymap-current-active-keymap-symbols ()
  "Return a list of currently active keymap symbols."
  (interactive)
  (let ((symbols (keymap-symbol (current-active-maps))))
    (when (called-interactively-p 'any)
      (message "%s" symbols))
    symbols))

(defun walk-dir-tree (top fun)
  "Map FUN over the files rooted at directory TOP."
  (interactive
   "Denter top directory:\nCenter command to run on each file: ")
  (when (commandp fun)
    (setf fun `(lambda (fn)
		 (unless (auto-save-file-name-p (f-filename fn))
		   (let ((was-open (find-buffer-visiting fn))
			 (buffer (find-file-noselect fn))
			 was-modified)
		     (set-buffer buffer)
		     (setf was-modified (buffer-modified-p buffer))
		     (call-interactively ',fun)
		     (unless was-modified (save-buffer))
		     (unless was-open (kill-buffer buffer)))))))
  (lexical-let ((fun fun))
    (cl-loop with front = (list top)
	  with new-front = nil
	  while front do
	  (cl-loop while front
		as dir = (pop front)
		as files = (progn (cl-assert (f-dir? dir))
				  (directory-files dir))
		do (cl-loop for base in files
			 as fn = (f-join dir base)
			 do (if (f-dir? fn)
				(unless (member base '(".." "."))
				  (push fn new-front))
			      (unless (auto-save-file-name-p base)
				(funcall fun fn)))))
	  do (setf front new-front
		   new-front nil))))

(defmacro with-temporary-current-file (filename &rest body)
  "Temporarily open existing file FILENAME and evaluate BODY there."
  (let ((was-open-sym (gensym "was-open"))
        (buffer-sym (gensym "buffer"))
        (ret-val-sym (gensym "ret-val")))
    ;; (save-find-file-excursion
    (save-excursion
      `(let* ((,was-open-sym (get-file-buffer ,filename))
              (,buffer-sym (or ,was-open-sym
                               (find-file-noselect ,filename)))
              ret-val-sym)
         (with-current-buffer ,buffer-sym
           (setf ret-val-sym (progn ,@body))
           (unless ,was-open-sym (kill-buffer ,buffer-sym))
           ret-val-sym)))))

(defun regexp-replace-current-buffer (from to &optional pause)
  "Replace regexp FROM with replacement TO in the current buffer.

  If PAUSE is non-nil, stop after every replacement.
  Return the number of matches replaced."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward from nil t)
        (when pause (y-or-n-p (format
                               "Continue (%s --> %s)? "
                               from to)))
        (replace-match
         (if (functionp to)
             (save-excursion (funcall to))
           to)
         t)
        (cl-incf count)))
    count))

(defun replace-regexp-recursively (dir exts from to &optional pause)
  "REGEXP replace FROM => TO on all files under DIR with extension in EXTS.

   When PAUSE is non-nil, prompt every match.
   If EXTS is nil, all file extensions are considered."

  ;;TODO colored output
  (interactive
   (let* ((exts
           (split-string
            (read-string
             "enter space-separated extensions (eg 'cc h'): "
             (f-ext (or (buffer-file-name (current-buffer)) "")) nil  "")
            " " t))
	  (dir (if current-prefix-arg
		   (read-directory-name "enter directory: ")
		 default-directory))
	  (from (read-string "Enter from regexp: " (thing-at-point 'symbol)))
	  (to (read-string "Enter to regexp: "))
	  (pause (y-or-n-p "Pause at every match? ")))
     (list dir exts from to pause)))
  (let ((count 0))
    (walk-dir-tree
     dir
     `(lambda (filename)
        (when (or (null exts) (member (f-ext filename) exts))
          (message "replacing '%s' => '%s' in %s" from to filename)
          (let ((local-count 0)
                ;; support relative symlinks
                (default-directory (f-dirname filename)))
          (with-temporary-current-file
           filename
           (cl-incf count
                    (setq local-count
                          (regexp-replace-current-buffer from to pause)))
           (when (buffer-modified-p)
             (save-buffer)))
          (message "%s occurrences replaced in %s" count filename)))))
    (message "%d occurrences replaced in directory"
             count)))

(defun keymap-symbol (keymaps)
  "Return the symbol to which any keymap in KEYMAPS is bound, or nil if no such symbol exists."
  (let (syms)
    (mapatoms (lambda (sym)
                (and (not (eq sym 'keymap))
                     (boundp sym)
                     (find (symbol-value sym) keymaps)
                     (push sym syms))))
    syms))

(defun remove-trailing-whitespace (a b)
  "Remove trailing whitespace in region A, B, defaulting to the entire buffer."
  (interactive
   (if (region-active-p)
       (let ((a (min (mark) (point)))
	     (b (max (mark) (point))))
	 (list a b))
     (list (point-min) (point-max))))
  (save-excursion
    (goto-char a)
    (while (search-forward-regexp "[ \t]+$" b t)
      (replace-match ""))))

(defun kill-buffers-matching-regexp (regex)
  "Kill buffers whose names match REGEX."
  (interactive "sEnter regex to kill buffers: ")
  (let* ((regex (format ".*%s.*" regex))
	 (buffers (remove-if-not
		   (lambda (buff) (string-match regex (buffer-name buff)))
		   (buffer-list)))
	 (kill-func
	  (lambda (buff)
	    (let* ((proc (get-buffer-process buff)))
	      (when proc
		(message "killing %s" (process-name proc))
		(interrupt-process proc)
		(kill-process proc)))
	    (message "killing buffer: %s " (buffer-name buff))
	    (kill-buffer buff))))
    (mapc kill-func buffers)))

(defun lpr-buffer-no-confirm ()
  ;; /usr/local/share/emacs/25.2/lisp/lpr.el.gz
  "Print buffer contents without pagination or page headers.
See the variables `lpr-switches' and `lpr-command'
for customization of the printer command."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches nil))

(defun multi-regexp-replace (text-replacement-alist &optional a b)
  "Replace each (FROM TO) pair in TEXT-REPLACEMENT-ALIST in region A, B."
  (unless (and a b) (setf a (point-min) b (point-max)))
  (save-excursion
    (goto-char a)
    (let ((regexp (s-join "\\|" (mapcar 'car text-replacement-alist))))
      (while (re-search-forward regexp b t)
        (let* ((text (match-string 0))
	       (_ (message "text is %s" text))
	       (replacement (save-match-data
			      (cl-loop for (regexp replacement) in text-replacement-alist
				    thereis (when (string-match regexp text)
					      replacement)))))
          (replace-match replacement))))))

(defun multi-regexp-replace-sequential (text-replacement-alist
					&optional a b)
  "Replace each (FROM TO) pair in TEXT-REPLACEMENT-ALIST sequentially on region A, B."
  (unless (and a b) (setf a (point-min) b (point-max)))
  (cl-loop for (regexp replacement) in text-replacement-alist
	do (progn (goto-char a)
		  (while (re-search-forward regexp b t)
		    (replace-match replacement)))))

(defun url-decode (a b)
  "URL-decode the region A, B."
  (interactive "r")
  (multi-regexp-replace
   '(("%20" " " )
     ("%3B" ";")
     ("%26" "&" )
     ("%3D" "=")
     ("%3C" "<" )
     ("%3F" "?")
     ("%3E" ">" )
     ("%40" "@")
     ("%22" "\"" )
     ("%5B" "[")
     ("%23" "#" )
     ("%5C" "\\")
     ("%24" "$" )
     ("%5D" "]")
     ("%25" "%" )
     ("%5E" "^")
     ("%27" "'" )
     ("%60" "`")
     ("%2B" "+" )
     ("%7B" "{")
     ("%2C" "," )
     ("%7C" "|")
     ("%2F" "/" )
     ("%7D" "}")
     ("%3A" ":" )
     ("%7E" "~"))
   a b))

(defun flush-empty-lines ()
  "Flush empty lines in the current buffer."
  (interactive)
  (flush-lines "^$" (point-min) (point-max)))

(defmacro def-file-local-set-command (file-local-var-sym
                                      &optional prompt-form old-value-sym)
  "Define a command to set the file-local value of FILE-LOCAL-VAR-SYM.

  If PROMPT is a string, read the variable's value via ‘read-string'.
  Otherwise, PROMPT-FUN must be a function that accepts the
  current value of FILE-LOCAL-VAR-SYM and returns the new value."

  (let* ((fun-sym (intern (format "file-local-set-%s" file-local-var-sym)))
         (old-value-sym (or old-value-sym (gensym "old-value-")))
	 (prompt-form (or prompt-form
                          `(read-string
                            (format "Enter file-local value for %s (currently %s): "
                                    ',file-local-var-sym
                                    ,old-value-sym)))))
    `(progn
       (make-variable-buffer-local ',file-local-var-sym)
       (defun ,fun-sym (arg)
         (interactive "P")
         (let* ((curr-value (when (boundp ',file-local-var-sym)
			      ,file-local-var-sym))
	        (new-value ,prompt-form))
	   (add-file-local-variable ',file-local-var-sym new-value)
	   (setf ,file-local-var-sym new-value))))))

(def-file-local-set-command js-indent-mode
  (read-number "enter value for js-indent-mode: "))

(defun file-local-set (sym value)
  "Set SYM to VALUE persistently using a file-local variable."
  (interactive "SEnter variable to set: \nXEnter value: ")
  (add-file-local-variable sym value t))

(defmacro def-file-local-toggle-command (file-local-var-sym)
  "Define a command to toggle the file-local value of FILE-LOCAL-VAR-SYM on/off."
  `(def-file-local-set-command ,file-local-var-sym (lambda (_prompt old) (not old))))

(defun uuid ()
  "Generate a UUID."
  (interactive (list t))
  (if (zerop (call-process "which" nil nil nil "uuidgen"))
      (let ((uuid (shell-command-to-string "uuidgen| tr -d '\n'")))
        uuid)
    (genpass-genpass 32 genpass-alnum t)))

(defun new-buffer-focus ()
  "Switch to the next 'new buffer'."
  (interactive)
  (switch-to-buffer (cl-loop for buff in (buffer-list)
			  thereis
			  (and (s-starts-with-p "new-buffer" (buffer-name buff)) buff))))

(defun perf-test (source-buff)
  "Try to use primitive statistical profiling to identify a hotspot.

  Compile SOURCE-BUFF and repeatedly interrupt the subprocess."
  (interactive (list (current-buffer)))
  (while t
    (switch-to-buffer source-buff)
    (recompile)
    (switch-to-buffer "*compilation*")
    (sit-for 3)
    (comint-interrupt-subjob)
    (read-key "cont: ")))

(defun path-append-directory (dir)
  "Append DIR to the PATH."
  (interactive "Denter directory: ")
  (setenv "PATH" (concat (getenv "PATH") ":" dir))
  (push dir exec-path))

(defun path-ls ()
  "Display the current PATH."
  (interactive)
  (message "PATH:\n%s"
           (replace-regexp-in-string ":" "\n" (getenv "PATH"))))

(defun source-shell-vars (sh-vars-filename &optional quiet)
  "Source shell vars defined in the file SH-VARS-FILENAME.  No echo on QUIET."
  (interactive (list
                (read-file-name "enter shell file to source: ")))
  (cl-assert (file-exists-p sh-vars-filename))
  (cl-loop with cmd = (format "bash -c 'set -a; source %s &> /dev/null; env'"
                           sh-vars-filename)
        with out = (shell-command-to-string cmd)
        with env = (s-split "\n" out t)
        for var-val in env
        when (string-match "^\\([^= ]+?\\)=\\(.*\\)$" var-val)
        do (let ((var (match-string 1 var-val))
                 (val (match-string 2 var-val)))
             (unless (string-match "^\\(BASH_FUNC_\\|_\\| \\)" var)
               (unless quiet (message "setting %s to %s" var val))
               (setenv var val)
               (when (equal "PATH" var)
                 (cl-loop for dir in (s-split ":" val t)
                       do (cl-pushnew dir exec-path :test #'equal)))))))

(defun diff-buffer-with-another-file (buffer file)
  "View the differences between BUFFER and another file FILE.
This requires the external program `diff' to be in your `exec-path'."
  (interactive "bBuffer: \nfFile: ")
  (with-current-buffer (get-buffer (or buffer (current-buffer)))
    (diff file (current-buffer) nil 'noasync)))

(defmacro comp (comps-sym &optional default)
  "A macro to complete for a set of choices COMPS-SYM, with default DEFAULT."
  `(completing-read ,(format "complete from %s: " comps-sym)
                    ,comps-sym
                    nil t nil nil (list ,default)))

(defun lisp-fmt-fix-hanging-rparen ()
  "Auto-fix my old LISP code with the wrong rparen style."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp
     "\n[ 	]*\\()+\\)" "\\1" nil
     (point-min) (point-max) nil)))

(defun print-level-disable-ellipsis (enable)
  "Disable elipsis when printing LISP expressions, unless ENABLE is non-nil."
  (interactive "P")
  (let ((val (if enable 12 nil)))
    (dolist (sym
             '(eval-expression-print-level
               eval-expression-print-length
               print-level
               print-length))
      (set sym val))))

(defun regexp-replace-select-from-list (regexp-replacement-alist &optional noquery)
  "Interactively select a regexp-replacement pair from REGEXP-REPLACEMENT-ALIST.

   If NOQUERY is non-nil, use ‘replace-regexp' to replace every match.
   Otherwise, use ‘query-replace-regexp'"
  (cl-destructuring-bind (regexp . replacement)
      (selcand-select regexp-replacement-alist)
    (funcall
     (if noquery 'replace-regexp 'query-replace-regexp)
     regexp replacement)))

;;minibuffer-local-map

(add-hook 'lisp-mode-hook 'slime-mode)

(defun upcase-last (&optional capitalize)
  "Upcase the last sexp.  If CAPITALIZE is non-nil, capitalize instead."
  (interactive)
  (let ((fun (if capitalize 'capitalize-region 'upcase-region)))
    (save-excursion
      (funcall fun (point)
               (progn
                 (backward-sexp)
                 (point))))))

(defun capitalize-last ()
  "Capitalize the last sexp."
  (interactive "P")
  (upcase-last t))

(defun bash-identifier-current-line  ()
  "Return the bash variable at the current line."
  (let ((line
	 (buffer-substring-no-properties
	  (point)
	  (line-beginning-position))))
    (and
     (string-match
      "^[[:space:]]*\\([^=]+\\)="
      line)
     (match-string 1 line))))



(defun process-filter-line-buffer (real-filter &optional separator-char)
  "Warp REAL-FILTER with a new process filter which emits complete lines.

  Optionally uses SEPARATOR-CHAR instead of newline.
  Returns a new ‘process-filter' function."
  (let ((cum-string-sym (gensym "proc-filter-buff"))
	(newline (or separator-char (string-to-char "\n")))
	(string-indexof (lambda (string char start)
			  (cl-loop for i from start below (length string)
				thereis (when (eq char (aref string i))
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



(defun peek (str start max)
  "Peek into str at most MAX characters in STR, starting at START."
  (substring str start (min (length str) (+ start max))))

(defun diff-lines-set (a b)
  (interactive
   (cl-labels ((read-region-lines-interactively (region-name)
                                                (message "select region %s, then exit recedit: "
                                                         region-name)
                                             (recursive-edit)
                                             (->>
                                              (buffer-substring
                                               (region-beginning)
                                               (region-end))
                                              (s-split "\n"))))
     (list (read-region-lines-interactively "A")
           (read-region-lines-interactively "B"))))
  (let ((buffname "*A B region diff*"))
    (with-help-window buffname
      (with-current-buffer buffname
        (cl-loop for (msg lines) on
              (list
               "common lines" (intersection a b :test #'equal)
               "unique to A" (set-difference a b :test #'equal)
               "unique to B" (set-difference b a :test #'equal))
              by #'cddr
              do
              (progn (princ msg)
                     (add-text-properties (line-beginning-position)
                                          (line-end-position)
                                          '(face bold))
                     (princ "\n")
                     (princ (s-join "\n" (or lines "")))
                     (princ "\n")
                     (princ "\n")))))))

(defun package-install-after-refresh ()
  (interactive)
  (package-refresh-contents)
  (call-interactively #'package-install))

(defun detect-indent-level ()
  ;; (save-excursion
  (interactive)
  (goto-char (point-min))
  (cl-loop with min-spaces = 1.0e+INF
           with min-loc = nil
           while (re-search-forward "^[ ]+" nil t)
           as spaces = (length (match-string 0))
           when (< spaces min-spaces)
           do (setq min-spaces spaces
                    min-loc (match-beginning 0))
           finally
           (progn
             (when min-loc
               (goto-char min-loc)
               (message "%d" min-spaces)
               (return min-spaces)))))


(defmacro register-groups-bind (vars &rest body)
  "Bind capture groups from the last string match as VARS."
  (declare (indent 1))
  `(let ,(cl-loop for var in vars
                  for num from 0
                  unless (eq '_ var)
                 collect `(,var (match-string ,num)))
     ,@body))


(defun git-merge ()
  "Simple git merge."
  (interactive)
  (revert-buffer-no-confirm)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<<<<<<<" nil t)
      (goto-char (match-beginning 0))
      (re-search-forward
       (concat
        "^<<<<<<< ?\\(.*\\)\n\\(\\(.*\n\\)*?\\)"
        "=======.*\n\\(\\(.*\n\\)*?\\)>>>>>>> \\(.*\\)\n"))
      (register-groups-bind (_ opt1-name opt1-data _ opt2-data _ opt2-name)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (choices-alist
                `((,(substring-no-properties opt1-name) . ,opt1-data)
                  (,(substring-no-properties opt2-name) . ,opt2-data)))
               (choice-key
                (save-match-data
                  (selcand-select (mapcar #'car choices-alist)
                                  "select merge option: "
                                  nil nil nil t)))
               (choice (alist-get choice-key choices-alist nil nil #'equal)))
          (cl-assert choice)
          (goto-char start)
          (replace-match choice t t nil)
          '(cl-assert (y-or-n-p "Continue? ")))))))

(defun switch-to-buffer-with-process-pid (pid)
  "Switch to the buffer associated with a process with PID."
  (interactive "nenter pid: ")
  (cl-loop for proc in (process-list)
           thereis (when (eq (process-id proc) pid)
                     (if-let* ((buff (process-buffer proc))
                              (_is-live (buffer-live-p buff)))
                         (switch-to-buffer buff)
                       (when (y-or-n-p
                              (format (concat
                                       "process %s is a child of emacs, "
                                      "but has no buffer. kill?")
                                      pid))
                         (kill-process proc)))
                     t)))

(defun debian-file->string (filename &optional not-literal-p)
  (with-temp-buffer
    (funcall (if not-literal-p 'insert-file-contents 'insert-file-contents-literally) filename)
    (buffer-string)))

(defun diff-a-b (a b)
  (with-temp-buffer
    (insert a)
    (let ((a-buff (current-buffer)))
      (with-temp-buffer
        (insert b)
        (diff a-buff (current-buffer) nil 'noasync)))))


(defun diff-region-with-kill-ring (a b)
  (interactive "r")
  (unless (region-active-p) (error "no region"))
  (unless kill-ring (error "no kill-ring"))
  (diff-a-b (region) (car kill-ring)))

(defun diff-two-regions ()
  (interactive)
  (cl-labels
      ((prompt-region (&optional prompt)
                      (unless prompt
                        (setq prompt "select region, then exit rec. edit."))
                      (message prompt)
                      (recursive-edit)
                      (region)))
    (diff-a-b (prompt-region) (prompt-region))))

(defun eval-buffer-debug ()
  (interactive)
  (goto-char (point-min))
  (while (let ((old-point (point)))
           (forward-sexp)
           (not (= (point) old-point)))
    (eval-last-sexp nil)))

(defun buffer-process-id ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ((not proc) (message "buffer has no process"))
     ((not (process-live-p proc))
           (message "buffer has no live process"))
     (t (message "%s" (process-id proc))))))

(defvar emacs-all-sources-dir
  (expand-file-name "~/src/emacs-all-sources/"))

(defun emacs-c-source-directory ()
  (cl-loop
   with glob = (f-join
                  emacs-all-sources-dir
                  (format "emacs-%s*"
                          (replace-regexp-in-string "[.].*" "" emacs-version))
                  "src")
   for filename
   in (f-glob glob)
   thereis (when (file-directory-p filename)
             filename)))

(defun emacs-fetch-c-sources ()
  (or (emacs-c-source-directory)
      (progn
        (unless (file-exists-p emacs-all-sources-dir)
          (mkdir emacs-all-sources-dir))
        (let ((default-directory emacs-all-sources-dir))
          (format "downloading emacs sources...")
          (shell-command "apt-get source emacs"))
        (cl-assert (emacs-c-source-directory))
        (emacs-c-source-directory))))

(setq find-function-C-source-directory
      (emacs-fetch-c-sources))

(defun espeak-read-text ()
  (cond
   ((region-active-p)
    (buffer-substring (region-beginning)
                      (region-end)))
   (t (read-string "enter text to speak: "))))

(cl-defun espeak (text &key (lang "en")
                       (speed 160)
                       (gap 10))
  (interactive (list (espeak-read-text)))
  (message "DEBUG tjvd text: %s" text)
  (start-process "espeak" "*espeak*" "espeak"
                 text
                 "-v" lang
                 "-s" (format "%d" speed)
                 "-g" (format "%d" gap)))

(defun espeak-spell (text)
  (interactive (list (espeak-read-text)))
  (sit-for 2)
  (espeak
   (replace-regexp-in-string "" ". " text)
   :speed 160
   :gap 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc-utils.el ends here
