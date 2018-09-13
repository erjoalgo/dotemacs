;;; command-mode-commands.el ---

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; several custom commands for command-mode

;;; Code:

(defvar *scroll-amount* nil )
(setq *scroll-amount* 8)
(defun scroll-down-keep-cursor (arg)
   ;; Scroll the text one line down while keeping the cursor
  (interactive "P")
  (scroll-down-command (- (* *scroll-amount* (or arg 1)))))

(defun scroll-up-keep-cursor (arg)
   ;; Scroll the text one line up while keeping the cursor
  (interactive "P")
  (scroll-up-command (- (* *scroll-amount* (or arg 1)))))
(setq scroll-error-top-bottom t)

(defun find-file-at-point-cmd ()
  (interactive)
  (find-file-at-point))

(defun my-forward-delete (&optional n)
  (interactive "P")
  (delete-region (point) (progn (forward-sexp n) (point))))

(defmacro with-split-preference (vertical-p &rest body)
  (let ((w-sym (gensym "w"))
        (h-sym (gensym "h")))
    `(let (,w-sym ,h-sym)
       (if ,vertical-p
           (setf ,w-sym 0)
         (setf ,h-sym 0))
       (let ((split-width-threshold ,w-sym)
             (split-height-threshold ,h-sym))
         ,@body))))


(defun duplicate-current-buffer (&optional vertical-p)
  (interactive "P")
  (with-split-preference vertical-p
   (switch-to-buffer-other-window (current-buffer))))

(defun yank-or-pop ()
  (interactive)
  (cond
   ((eq major-mode 'term-mode) (term-paste))
   ((eq last-command 'yank) (yank-pop))
   (t (yank))))

(defun my-kill-whole-line (arg)
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defmacro toggle-bool (sym)
  `(setf ,sym (not ,sym)))

(defun my-move-beginning-of-line ()
  ;;TODO use mode-hook-initialized variables
  (interactive)
  (case major-mode
    ('eshell-mode
     (eshell-bol))

    ('slime-repl-mode
     (beginning-of-line))

    ;;((string-match "[*]R[*]" (buffer-name (current-buffer)))
    ('inferior-ess-mode
     (move-past-prompt "^> " t))

    (t
     (if (equal last-command 'my-move-beginning-of-line)

	 (progn (if my-move-beginning-of-line-toggle
		    (move-beginning-of-line nil )
		  (back-to-indentation))
		(toggle-bool my-move-beginning-of-line-toggle))
       (progn (setq my-move-beginning-of-line-toggle t)
	      (back-to-indentation))))))

(defun copy-line-up (arg) (interactive "P")
  (move-line-up-mine nil t arg))

(defun copy-line-down (arg) (interactive "P")
  (move-line-up-mine 1 t arg))

(defun move-line-down (arg) (interactive "P")
  (move-line-up-mine 1 nil arg))

(defun move-line-up (arg) (interactive "P")
  (move-line-up-mine nil nil arg))

(defun move-line-up-mine (&optional down copy n)
  (interactive (list nil nil current-prefix-arg))
  (dotimes (_ (or n 1))
    (let*
	((a (line-beginning-position))
	 ;;(b (min (1+ (line-end-position) ) (point-max)))
	 (b (line-end-position))
	 (middle (- (point) a))
	 (line (buffer-substring a b)))
      (unless copy
	(progn (delete-region a b)
	       (when (not (equal (point) (point-max))) (delete-char 1))))
      (if down
	  (if (progn (end-of-line)
		     (equal (point-max) (point)))
		(progn (open-line 1)
		       (forward-char))
	      (forward-line))
	(unless copy (forward-line -1)))
      (goto-char (line-beginning-position))
					;(if copy (insert line))
      (progn (insert line "\n") (backward-char))
      (goto-char (+ (line-beginning-position) middle)))))

(defvar kill-surrounding-cum-count)
(defun kill-surrounding-sexp (arg)
  (interactive "P")
  (setq kill-surrounding-cum-count
	(1+ (if (eq last-command 'kill-surrounding-sexp)
	     kill-surrounding-cum-count 0)))
  (save-excursion
    (let* ((n (+ (or arg 1) -1 kill-surrounding-cum-count))
	   (at-beginning-of-sexp (at-beginning-of-sexp))
	   (killed (buffer-substring
		    (progn (backward-sexp
			    (- n (if at-beginning-of-sexp 1 0)))
			    (point))
		    (progn (forward-sexp n) (point)))))
      (message "killed: %s" killed)
      (kill-new killed)
      (when window-system
	(set-clipboard killed)))))

(defun set-clipboard (x)
  (x-set-selection 'CLIPBOARD x)
  (x-set-selection nil x))

(defun at-beginning-of-sexp ()
  (condition-case ex
      (save-excursion (= (point)
		     (progn (forward-sexp 1)
			    (backward-sexp 1)
			    (point))))
      ('error nil )))


(defun then-cycle-window (fun) (interactive)
	 `(lambda () (interactive)
	    (funcall ',fun)
	    (other-window 1)
	    (cycle-buffer nil)))

(fset 'my-split-window-below (then-cycle-window 'split-window-below))
(fset 'my-split-window-right (then-cycle-window 'split-window-right))

(defmacro -> (&rest forms)
  (if (cadr forms)
      ;;(destructuring-bind (first (a a-rest) . rest) forms
      ;;`(-> a first a-rest ,@rest))
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
					   second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
    (car forms)))

(defmacro ->> (&rest forms)
  (if (second forms)
      (destructuring-bind (a b . cde) forms
	(let ((b (if (atom b) (list b) b)))
	  `(->> ,(nconc b (list a)) ,@cde)))
    (first forms)))

(setf cycle-buffer-exclude (list
			    "*scratch*" "*GNU Emacs*" " *Minibuf-1*"
			    " *Minibuf-0*" "*Messages*" " *code-conversion-work*"
			    " *Echo Area 1*" " *Echo Area 0*" "*Completions*"
			    "*Apropos*" "*Help*"))


(defun nth-mod (n list)
  (nth (mod n (length list)) list))

(defmacro defcommand-cycle-buffer (cmd-name
                                   buff-sym
				   buffer-pred-form
				   &optional
				   no-matches-form)
  "Define a command ‘cmd-name' to switch to the buffer matching
‘buffer-pred-form', or calls ‘no-matches-form' if none matches
or if the command was called with a prefix argument.
If the current buffer matches, ‘cmd-name'
will prefer to switch to a different buffer"

  (let ((matching-buff-sym (gensym "matching-buff")))

    `(defun ,cmd-name (arg)
       (interactive "P")
       (or
        (unless arg
          (when-let
              ((,matching-buff-sym
                (loop
	         for ,buff-sym in (buffer-list)
                 thereis (and (not (eq (current-buffer) ,buff-sym))
                              ,buffer-pred-form
                              (switch-to-buffer ,buff-sym))
                 finally (return
                          (let ((,buff-sym (current-buffer)))
                            (when ,buffer-pred-form
                              (switch-to-buffer ,buff-sym)))))))
            (switch-to-buffer ,matching-buff-sym)))
        ,no-matches-form))))


(defcommand-cycle-buffer cycle-buffer
  buff
  (not (member buff cycle-buffer-exclude))
  (error "no more non-excluded buffers to cycle"))


(defun async-shell-command-no-prompt (&rest args)
  (let* ((async-shell-command-buffer 'new-buffer))
    (apply 'async-shell-command args)))

(defun my-eval-defun (&optional arg)
  (interactive "P")
  "proxy to either slime, cider, or emacs lisp's eval-defun"
  (cond
   ((and (boundp 'slime-mode) slime-mode)
    (let ((slime-load-failed-fasl 'always))
      (call-interactively 'slime-eval-defun)))

   ((and (boundp 'cider-mode) cider-mode)
    (call-interactively 'cider-eval-defun-at-point))

   ;;emacs lisp
   (t (eval-defun arg))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun sanitize-filename (fn)
  (-> (if (string-match "^/sudo:root@[^:]+:\\(.*\\)" fn)
	  (match-string 1 fn)
	fn)
      expand-file-name))

(defun find-iregex (regex directory)
  (interactive (list
		(read-string "enter regex: ")
		(when current-prefix-arg
		  (expand-file-name
		   (read-directory-name "enter directory: ")))))
  (unless directory
    (setf directory (expand-file-name default-directory)))
  (when (string-match "^/sudo:root@[^:]+:\\(.*\\)" directory)
    (setf directory (match-string 1 directory)))

  (message directory)
  (let ((buff-name "find-iregex"))
    (when (and (get-buffer buff-name)
	       (or t clear-buffer))
      (with-current-buffer buff-name
	(erase-buffer)))

    (let ((proc
	   (start-process buff-name buff-name "find"
			  directory "-iregex" (format ".*%s.*" regex))))
      (set-process-sentinel
       proc
       `(lambda (proc change)
	  (switch-to-buffer ,buff-name)
	  (setf default-directory ,directory)
	  (buffer-relativize-path-names ,directory)
	  (progn (goto-char (point-max))
		   (insert "DONE"))
	  (beginning-of-buffer))))))


(defun gen-new-buffer (&optional name)
  (generate-new-buffer (generate-new-buffer-name (or name "new-buffer"))))

(defun find-new-buffer  ()
  (interactive)
  (let* ((default-directory "/tmp/")
	 (buff-name (gen-new-buffer)))
    (switch-to-buffer-other-window buff-name)
    (auto-save-mode 1)))

(defun sudo-buffer () (interactive)
       (let ((curr-fn (expand-file-name
		       (if (eq major-mode 'dired-mode)
			   dired-directory
			 (buffer-file-name (current-buffer))))))

	 (unless (s-starts-with-p "/sudo" curr-fn)
	   (let ((pos (point)))
	     (find-file (concat "/sudo::" curr-fn))
	     (goto-char pos)))))


(require 'f)
(defun buffer-relativize-path-names (dir &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^" (regexp-quote dir) "/?")
	      nil t)
	(replace-match "")))))

(defun buffer-add-space-before-line-number (&optional buffer)
  ;; TODO fix in find-file-at-point
  "workaround to make `find-file-at-point' provide a filename that exists"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[^:]+\\(:[0-9]+:\\)"
	      nil t)
        (message "match found")
	(replace-match " \\1" nil nil nil 1)))))

(defun grep-recursive (extension pattern dir &optional clear-buffer)
  ;;TODO colored output
  (interactive
   (let* ((pattern
	   (let ((default (car kill-ring))
		 (symbol-at-point
		  (let ((search (sexp-at-point)))
		    (and search (symbolp search)
			 (symbol-name search)))))

	     (read-string
	      (format "enter grep pattern: (default %s): " default)
	      symbol-at-point
	      nil default)))

	  (ext (and nil (read-string
			 "enter extension (eg 'js'): "
			 (f-ext (or (buffer-file-name (current-buffer)) "")))))
	  (dir (-> (if current-prefix-arg
		       (read-directory-name "enter directory: ")
		     default-directory)
		   sanitize-filename)))
     (list  (and (not (string= "" ext)) ext) pattern (expand-file-name dir) t)))

  (let ((buff-name "grep-recursive")
        (sudo-p (and (file-remote-p default-directory)
                     (list (file-remote-p default-directory 'method))))
	proc)
    (when clear-buffer
      (switch-to-buffer buff-name)
      (erase-buffer))
    (setf default-directory dir)

    (setf proc (apply 'start-process buff-name buff-name
		   (append
		    `(,@sudo-p "find" ,dir "-name" ".git" "-prune" "-o")
		    (when extension
		      (list "-name" (concat "*" extension)))
		    `("-exec" ,@sudo-p "grep" "-HinsI" ,pattern "{}" ";"))))
    (set-process-sentinel proc
			  `(lambda (proc change)
			     (switch-to-buffer ,buff-name)
			     ;; (setf default-directory ,dir)
			     (buffer-relativize-path-names ,dir)
                             (buffer-add-space-before-line-number)
			     (progn (goto-char (point-max))
				      (insert "DONE"))
			     (beginning-of-buffer)))
    '(start-process buff-name buff-name
		    "grep" "-RHins" pattern dir)
    ;(set (make-local-variable 'window-point-insertion-type) t)
    ))

(defun kill-current-buffer-filename ()(interactive)
       (let ((fn
	      (replace-regexp-in-string
	       "^/sudo:root@.*?:" ""
	       (if (eq major-mode 'dired-mode)
		   (expand-file-name
		    (or (dired-file-name-at-point)
		       default-directory))
		   (buffer-file-name (current-buffer))))))

	 (kill-new fn)
	 (message "killed: %s" fn)
	 fn))

(defun message-current-buffer-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
	(message "%s" (process-command proc))
      (message "buffer has no process"))))

(defun flush-repeatedly ()
  (interactive)
  (goto-char (point-min))
  (let (regexp)
    (while (progn (setf regexp (read-string "flush lines: "))
		  (-> regexp length zerop not))
      (flush-lines regexp))))

(defun cycle-buffer (arg) (interactive "P")
       (let ((buf-list (remove-if (lambda (buf)
                                   (-> (buffer-name buf)
                                       (member cycle-buffer-exclude)))
                                 (buffer-list)))
            (nth-mod (lambda (n list) (nth (mod n (length list)) list)))
            (direction (if arg -1 1)))
	 (unless (> (length buf-list) 1)
		  (error "no more buffers"))
        (if (member last-command '(cycle-buffer 'cycle-prev-buffer))
            (progn
              (incf cycle-buffer-index direction)
              (switch-to-buffer (funcall nth-mod cycle-buffer-index buf-list)))
          (progn
            (setq cycle-buffer-index
                  (loop for i from 0
                         for buff in (buffer-list)
                        thereis (and (eq (current-buffer) buff) i)))
            (switch-to-buffer nil)))))

(defun uri-encode (search-terms)
  (reduce
   (lambda (string from-to)
     (replace-regexp-in-string (car from-to) (cadr from-to) string nil t))
   '(("%" "%25")
     (" " "%20")
     ("[+]" "%2B"))
   :initial-value search-terms))

(setf engine-alist
  ;; search-engine-query-url-format
  '(
    ("ddg" . "https://duckduckgo.com/lite/?q=%s")
    ("linguee" . "https://www.linguee.com/english-spanish/search?source=auto&query=%s")
    )
 )

(defun completing-read-alist (prompt alist)
  (cdr (assoc (completing-read prompt (mapcar 'car alist)
                   nil t (caar alist)))))

(defun search-engine-search (term &optional engine)
  (interactive (list (if (region-active-p)
			 (buffer-substring-no-properties
			  (region-beginning) (region-end))
		       (read-string "enter search terms: " (car kill-ring)))
                     (if (boundp 'engine) engine
                       (completing-read-alist
                        "select search engine: " engine-alist))))
  (let ((search-engine-query-url-format (cdr (assoc-string engine engine-alist))))
  (message "searching for %s..." term)
  (browser-new-tab (format search-engine-query-url-format
			   (uri-encode
			    (replace-regexp-in-string "[\n]" "" term))))))

(defmacro search-engine-search-cmd (engine)
  `(defun ,(intern (format "search-engine-search-%s" engine)) ()
     ,(format "search using the %s search engine" engine)
     (interactive)
     (let ((engine ,engine))
       (call-interactively 'search-engine-search))))



(provide 'command-mode-commands)
;;; command-mode-commands.el ends here
