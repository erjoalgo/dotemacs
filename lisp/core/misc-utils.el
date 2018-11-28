(require 'cl-lib)

(defun whereis (program)
  (interactive "senter program: ")
  (let ((dirs
	 (loop for dir in (split-string (getenv "PATH") ":" t)
	       if (and (file-exists-p dir)
		       (member program (directory-files dir)))
	       collect (f-join dir program))))

    (when (called-interactively-p 'interactively)
      (message "%s" dirs))
    dirs))

(defun recover-this-file-and-diff ()
  (interactive)
  (recover-this-file);;TODO ignore prompt
  (diff-buffer-with-file))

(defun shell-command-of-region ()
  "compare to shell-command-on-region"
  (interactive)
  (let ((cmd (apply 'buffer-substring-no-properties
		    (if (region-active-p)
			(list (region-beginning) (region-end))
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
  "like shell-command-of-region"
  (interactive)
  (shell-command-of-region (point-min) (point-max)))

(defun lnabs (source &optional prompt)
  ;;(interactive "fEnter soft link source: ")
  (interactive (list
		(let ((initial (and (eq major-mode 'dired-mode)
				    (dired-file-name-at-point)
				    (f-filename
				     (dired-file-name-at-point)))))

		  (read-file-name
		   "Enter soft link source: "
		   nil initial t initial))))
  (let* ((source (expand-file-name source))
	 (base (f-filename source))
	 (dest (expand-file-name
		(read-file-name "enter destination: " nil base nil nil nil )))
	 (command (format "ln -sf %s %s" source dest)))
    (when (or (not prompt) (y-or-n-p command))
      (apply 'call-process "ln" nil "*lnabs*" nil
	     `("-sf" ,source ,dest)))))

(defvar *shred-rec-default-times* 10)

(defun shred-rec (fn &optional shred-times)
  ;;(interactive "fEnter soft link source: ")
  (interactive (list
		(if (and
		     (eq major-mode 'dired-mode)
		     (dired-file-name-at-point))
		    (f-filename
		     (dired-file-name-at-point))
		  (read-file-name "enter fn to shred: "))))
  (unless shred-times
    (setf shred-times *shred-rec-default-times*))
  (y-or-n-p (format "confirm shred %s: " fn))
  (let* ((shred-times-string (int-to-string shred-times))
	 (shred-cmd-arg-list
	  (if (executable-find "shred")
	      `("shred" "-zufn" ,shred-times-string)
	    `("srm" "-z")))
	 (fn (expand-file-name fn))
	 (BUFNAME "shred"))

    (if (file-directory-p fn)
	(and (y-or-n-p (format "confirm recursive shred of %s: " fn))
	     (progn
	       (apply 'start-process
		      `(BUFNAME BUFNAME "find" fn "-type" "f"
				"-exec" ,@shred-cmd-arg-list "{}" ";"))
	       (start-process BUFNAME BUFNAME "find" fn "-depth" "-type" "d"
			      "-exec" "rmdir" "{}" ";")))
      (unless (zerop (apply 'call-process (car shred-cmd-arg-list) nil BUFNAME t
			    (append (cdr shred-cmd-arg-list) (list fn))))
	(switch-to-buffer BUFNAME)
	(error "error in shred")))
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
  (let ((files (directory-files dir)))
    (reverse (sort-key files (lambda (fn)
			       (let ((attrs (file-attributes (f-join dir fn))))
				 (nth 6 attrs)))))))

(defun read-symbol-completing (prompt &optional default)
  (intern
   (completing-read prompt obarray nil nil (and default (symbol-name default)))))

(defun add-file-local-variable-mode (mode)
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
  "the buffer being examined by a ‘check-unsaved-buffers' recursive edit")

(defun check-unsaved-buffers ()
  (interactive)
  (loop as next-buff =
	(loop for buff in (buffer-list)
	      thereis (and
		       (not (get-buffer-process buff))
                       (or (not (buffer-file-name buff))
			   (buffer-modified-p buff))
                       (not (member (buffer-local-value 'major-mode buff) '(dired-mode)))
                       (not
                        (loop for re in
                              '("^[[:space:]]*[*].*[*]$"
                                "^[ ]*[*]mm[*]-[0-9]+"
                                "^[ ][*]nnimap"
                                "^[ ][*]nnimap"
                                "^irc.freenode.net:.*")
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

(defun check-unsaved-buffers-maybe-exit-recedit ()
  (when (equal (current-buffer) check-unsaved-buffers-current-buffer)
      (message "moving onto next check-unsaved-buffers buffer...")
      ;; (exit-recursive-edit)
      nil))
(add-hook 'kill-buffer-hook 'check-unsaved-buffers-maybe-exit-recedit )

(defun diff-sexps (sexp-a sexp-b)
  ;;TODO loop fill in missing length
  (loop for a in sexp-a
	for b in sexp-b
	do
	(if (not (eq (atom a) (atom b)))
	    (error "mismatch: %s %s" a b)
	  (if (not (atom a))
	      (diff-sexps a b)
	    (unless (equal a b)
	      (error "mismatch: %s %s" a b)))))
  (or (not (and (consp sexp-a) (consp sexp-b)))
      (= (length sexp-a) (length sexp-b))))

(defun lookup-key-in-current-maps (key)
  "list of active keymaps that bind the given key"
  (interactive (list (read-key-sequence "enter key to lookup in current maps: ")))
  (let* ((kmaps-filtered (remove-if-not (lambda (kmap)
					  (lookup-key kmap key))
					(current-active-maps)))
	 (kmap-syms (keymap-symbol kmaps-filtered))
	 (kmap-to-key-alist (mapcar (lambda (kmap-sym)
				      (cons kmap-sym (lookup-key
						      (symbol-value kmap-sym) key)))
				    kmap-syms)))
    (message "%s" kmap-to-key-alist)
    kmap-to-key-alist))

(defun keymap-current-active-keymap-symbols ()
  (interactive)
  (let ((symbols (keymap-symbol (current-active-maps))))
    (when (called-interactively-p 'any)
      (message "%s" symbols))
    symbols))

(defun clean-up-async-shell-command-buffers ()
  (interactive)
  (loop with async-buffs = (remove-if-not
			    (lambda (buff)
			      (string-match "Async.Shell"
					    (buffer-name buff)))
			    (buffer-list))
	for buff in async-buffs do
	(progn (switch-to-buffer buff)
	       (when (get-buffer-process buff)
		 (message "command was: %s"
			  (process-command (get-buffer-process buff))))
	       (recursive-edit))))

(defun walk-dir-tree (top fun)
  (interactive
   "Denter top directory:
Center command to run on each file: ")
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
    (loop with front = (list top)
	  with new-front = nil
	  while front do
	  (loop while front
		as dir = (pop front)
		as files = (progn (assert (f-dir? dir))
				  (directory-files dir))
		do (loop for base in files
			 as fn = (f-join dir base)
			 do (if (f-dir? fn)
				(unless (member base '(".." "."))
				  (push fn new-front))
			      (unless (auto-save-file-name-p base)
				(funcall fun fn)))))
	  do (setf front new-front
		   new-front nil))))

(defun completing-read-single-char (prompt candidates)
  (let* ((alist (mapcar (lambda (cand)  (cons (substring cand 0 1) cand))
			candidates))
	 (prompt (format "%s\nenter char: " (s-join "\n" (mapcar 'prin1-to-string alist))))
	 (chars (mapcar 'car alist))
	 char)
    (loop as char = (read-char prompt)
	  as pair =  (assoc (and char (char-to-string char)) alist)
	  until pair
	  finally (return (cdr pair)))))

(defmacro with-temporary-current-file (filename &rest body)
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

(defun replace-regexp-dir (dir extension from to &optional pause)
  "basically a recursive sed"
  ;;TODO colored output
  (interactive
   (let* ((ext (read-string
		"enter extension (eg 'js'): "
		(f-ext (or (buffer-file-name (current-buffer)) "")) nil  '(nil)))
	  (dir (if current-prefix-arg
		   (read-directory-name "enter directory: ")
		 default-directory))
	  (from (read-string "enter from regexp: "))
	  (to (read-string "enter to regexp: "))
	  (pause (y-or-n-p "pause at every match? "))
	  )
     (list dir ext from to pause)))
  (let ((count-sym (gensym)))
    (set count-sym 0)
    (walk-dir-tree dir
		   `(lambda (fn)
		      (when (or (null extension) (string= (f-ext fn) extension))
			(with-temporary-open-file
			 fn
			 (incf ,count-sym (regexp-replace-current-buffer from to pause))
			 (when (buffer-modified-p)
			   (save-buffer))))))
    (message "%d occurrences replaced" (symbol-value count-sym))))

(defun regexp-replace-current-buffer (from to &optional pause)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward from nil t)
	(incf count)
	(replace-match (if (functionp to)
			   (save-excursion (funcall to))
			 to) t)
	(when pause (y-or-n-p (format
			       "confirm: (%s --> %s)"
			       from to)))))
    count))

(defun regexp-exists-current-buffer (regexp)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp nil t)))

(defun keymap-symbol (keymaps)
  "Return the symbol to which KEYMAP is ound, or nil if no such symbol exists."
  (let (syms)
    (mapatoms (lambda (sym)
                (and (not (eq sym 'keymap))
                     (boundp sym)
                     (find (symbol-value sym) keymaps)
                     (push sym syms))))
    syms))

(defun remove-trailing-whitespace (a b)
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

(defun erc-autologin ()
  (interactive)
  (erc))

(defun kill-buffers-matching-regexp (regex)
  (interactive "senter regex to kill buffers: ")
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

(defun ispell-spanish ()
  (interactive)
  (ispell-change-dictionary "castellano8")
  (ispell))

(defun lpr-buffer-no-confirm ()
  ;; /usr/local/share/emacs/25.2/lisp/lpr.el.gz
  "Print buffer contents without pagination or page headers.
See the variables `lpr-switches' and `lpr-command'
for customization of the printer command."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches nil))

(defun multi-regexp-replace (text-replacement-alist)
  (cl-labels
      ((ors-regexp (regexps)
		   (s-join "\\|" regexps)))
    (let ((regexp (ors-regexp (mapcar 'car text-replacement-alist))))
      (while (re-search-forward regexp nil t)
	(let* ((text (match-string 0))
	       (_ (message "text is %s" text))
	       (replacement (save-match-data
			      (loop for (regexp replacement) in text-replacement-alist
				    thereis (when (string-match regexp text)
					      replacement)))))
	  (replace-match replacement))))))

(defun multi-regexp-replace-sequential (text-replacement-alist
					&optional a b)
  (unless a (setf a (point-min) b (point-max)))
  (loop for (regexp replacement) in text-replacement-alist
	do (progn (goto-char a)
		  (while (re-search-forward regexp b t)
		    (replace-match replacement)))))

(defun url-decode ()
  (interactive)
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
     ("%7E" "~")
     )))

'(multi-regexp-replace '(("defun" "tayfun")
			 ("interactive" "petaluma")))

(defun flush-empty-lines ()
  (interactive)
  (flush-lines "^$" (point-min) (point-max)))

(defmacro make-file-local-variable-set-command
    (file-local-var-sym &optional prompt prompt-fun set-or-toggle)
  "defun a command which sets a given file-local-variable
`prompt-fun' is the function to call to prompt for a new value.
it is called with 2 arguments, the prompt, and the current value
of the variable, or nil if unbound.
`prompt-fun' defaults to `read-string'"
  (let* ((var-sym-name (symbol-name file-local-var-sym))
	 (set-or-toggle (or set-or-toggle 'set))
	 (fun-sym (intern (format "file-local-%s-%s"
				  (symbol-name set-or-toggle)
				  var-sym-name)))
	 (prompt (or prompt (format "enter new value for %s: "
				    var-sym-name)))
	 (prompt-fun (or prompt-fun 'read-string)))
    (make-variable-buffer-local file-local-var-sym)
    `(defun ,fun-sym (arg)
       (interactive "P")
       (let* ((curr-value (when (boundp ',file-local-var-sym)
			    ,file-local-var-sym))
	      (new-value (funcall ,prompt-fun ,prompt curr-value)))
	 (add-file-local-variable ',file-local-var-sym new-value)
	 (setf ,file-local-var-sym new-value)))))

(defmacro make-file-local-variable-flag-toggle-command
    (file-local-var-sym)
  `(make-file-local-variable-set-command
    ,file-local-var-sym nil (lambda (_ curr) (not curr)) toggle))

(defun uuid (&optional insert)
  (interactive (list t))
  (let ((uuid (shell-command-to-string "uuidgen| tr -d '\n'")))
    (when insert (insert uuid))
    uuid))

(defun new-buffer-focus ()
  "switch to the next new buffer"
  (interactive)
  (switch-to-buffer (loop for buff in (buffer-list)
			  thereis
			  (and (s-starts-with-p "new-buffer" (buffer-name buff)) buff))))

(defun perf-test (source-buff)
  (interactive (list (current-buffer)))
  (while t
    (switch-to-buffer source-buff)
    (erjoalgo-compile-compile nil)
    (switch-to-buffer "*compilation*")
    (sit-for 3)
    (comint-interrupt-subjob)
    (read-key "cont: ")))

(defun path-append-directory (dir)
  (interactive "Denter directory: ")
  (setenv "PATH" (concat (getenv "PATH") ":" dir))
  (push dir exec-path))

(defun source-bashrc (bashrc)
  (interactive (list
                (read-file-name "enter bash file to source: "
                                 "~/.bashrc" "~/.bashrc")))
  (loop with cmd = (format "bash -i 'source %s &> /dev/null; env'" bashrc)
        with env = (s-split "\n" (shell-command-to-string cmd) t)

        for var-val in env
        do (string-match "\\([^=]+\\)=\\(.*\\)" var-val)
        as var = (match-string 1 var-val)
        as val = (match-string 2 var-val)
        do (message "setting %s to %s" var val)
        do (setenv var val))
  (loop for dir in (s-split ":" (getenv "PATH") t)
        do (add-to-list 'exec-path dir)))

(defun diff-buffer-with-another-file (buffer file)
  "View the differences between BUFFER and another file.
This requires the external program `diff' to be in your `exec-path'."
  (interactive "bBuffer: \nfFile: ")
  (with-current-buffer (get-buffer (or buffer (current-buffer)))
    (diff file (current-buffer) nil 'noasync)))

(defmacro comp (comps-sym &optional default)
  `(completing-read ,(format "complete from %s: " comps-sym)
                    ,comps-sym
                    nil t nil nil (list ,default)))

(defun lisp-fmt-fix-hanging-rparen ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; (while (re-search-forward "\n^[ 	]*\\()+\\)" nil t)
    (query-replace-regexp
     "\n[ 	]*\\()+\\)" "\\1" nil
     (point-min) (point-max) nil)))

(defun print-level-disable-ellipsis (arg)
  (interactive "P")
  (dolist (sym
           '(eval-expression-print-level
             eval-expression-print-length
             print-level
             print-length))
    (set sym nil)))

(defun common-prefix (strings)
  (loop with longest = (or (car strings) "")
        for s in (cdr strings)
        minimize
        (loop for c1 across longest
              for c2 across s
              for n below (or len (length longest))
              while (eq c1 c2)
              finally (return n))
        into len
        finally (return (subseq longest 0 (or len 0)))))


(defun s-match-buffer-all-nonverlapping (regexp)
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward regexp nil t)
          collect (match-string 0))))

(defun buffer-remove-common-prefix (&optional a b delimiter regexp)
  (interactive (nconc
                (if (region-active-p)
                  (list (region-beginning) (region-end))
                  '(nil nil))
                '(?/)))
  ;; TODO account for deletions when using b
  (let* ((prefix (common-prefix (if (not regexp)
                                    (remove-if 's-whitespace-p
                                               (s-split "\n" (buffer-string) t))
                                  ;; (mapcar 'car (s-match-strings-all regexp (buffer-string)))
                                  (s-match-buffer-all-nonverlapping regexp))))
         (len (length prefix)))

    (when delimiter
      (while (and (not (zerop len))
                  (not (eq (aref prefix (1- len)) delimiter)))
        (decf len)))

    (unless (zerop len)
      (save-excursion
        (goto-char (or a (point-min)))
        '(loop with deleted-count = 0
               as line-len = (- (line-end-position) (line-beginning-position))
               as new-end = (+ (line-beginning-position) len)
               while (and (< new-end (line-end-position))
                          (or (null b)
                              (<= new-end (+ b deleted-count))))
               do (progn
                    (delete-region (line-beginning-position) new-end)
                    (incf deleted-count line-len))
               while (zerop (forward-line)))
        (loop with deleted-count = 0
              as match = (re-search-forward (or regexp "^.*[^ \n\t].*$")
                                            (when b (+ b deleted-count))
                                            t)
              while match
              when match do (replace-match (subseq (match-string 0) len)))))))

(defun s-whitespace-p (s)
  (string-blank-p (s-trim s)))

(defun s-non-whitespace-p (s)
  (string-blank-p (s-trim s)))

(defun buffer-remove-common-filename-prefix ()
  (interactive)
  (buffer-remove-common-prefix nil nil ?/ "/.*"))

(defmacro save-excursion-file (filename &rest body)
  (let ((was-open-p-sym (gensym "was-open-p"))
        (buff-sym (gensym "buff"))
        (ret-val-sym (gensym "ret-val")))
    `(save-excursion
       (let ((,was-open-p-sym (find-buffer-visiting ,filename))
             ,ret-val-sym)
         (setf ,buff-sym
               (or ,was-open-p-sym
                   (find-file-noselect filename t)))
         (setf ,ret-val-sym ,@body)
         (unless ,was-open-p-sym
           (kill-buffer ,buff-sym))
         ,ret-val-sym))))

(defun buffer-make-prefix-unique (buffer &optional min-prefix-len)
  "Kill other buffers to make autocompletion easier."
  (interactive (list (current-buffer) 4))
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (others (remove buffer (buffer-list)))
         (buffer-name (buffer-name buffer))
         points)
    (loop for len from 1 upto (length buffer-name)
          ;; as new-others = (remove-if-not (apply-partially 's-starts-with? (subseq buffer 0 len))
          ;;                               others)
          as prefix = (subseq buffer-name 0 len)
          as new-others = (loop for other in others
                                when (s-starts-with-p prefix (buffer-name other))
                                collect other)
          when (not (eq (length others)
                        (length new-others)))
          do (progn
               (unless (zerop (1- len))
                 (push (cons (1- len) others) points))
               (setf others new-others))
          finally (when others
                    (push (cons len others) points)))
    (message "value of points: %s" points)

    (let* ((choices (loop for (len . others) in points
                          when (or (null min-prefix-len)
                                   (>= len min-prefix-len))
                          collect (subseq buffer-name 0 len))))
      (if (null choices)
          (message "buffer %s is already unique %s"
                   buffer (if min-prefix-len
                              (format "up to %s chars" min-prefix-len) ""))
        (let* ((choice (completing-read "select prefix length: " choices nil t
                                        (car (reverse choices))))
               (selected-length (length choice))
               (others (cdr (assoc selected-length points))))
          ;; others
          (message "killing: %s" others)
          (loop for (len . others) in points
                when (>= len selected-length)
                do (mapc 'kill-buffer others)))))))

(defun selcand-hints (cands &optional chars)
  (setf chars (or chars "qwertasdfzxcv1234"))
  (let* ((w (ceiling (log (length cands) (length chars))))
         (hints (loop with curr = '("")
                      for wi below w do
                      (setf curr
                            (loop for c across chars
                                  append (mapcar (apply-partially
                                                  'concat (char-to-string c))
                                                 curr)))
                      finally (return curr))))
    (loop for hint in hints
          for cand in cands
          collect (cons hint cand))))

(defun selcand-select (cands &optional prompt)
  (let* ((hints-cands (selcand-hints cands))
         (sep ") ")
         (choices (loop for (hint . cand) in hints-cands
                        collect (concat hint sep (prin1-to-string cand))))
         (prompt (or prompt "select candidate: "))
         (choice (completing-read prompt choices
                                  nil
                                  t))
         (cand (let* ((hint (car (s-split sep choice))))
                 (cdr (assoc hint hints-cands #'equal)))))
    cand))

(defun regexp-replace-select-from-list (regexp-replacement-alist &optional noquery)
  (destructuring-bind (regexp . replacement)
      (selcand-select regexp-replacement-alist)
    (funcall
     (if noquery 'replace-regexp 'query-replace-regexp)
     regexp replacement)))
