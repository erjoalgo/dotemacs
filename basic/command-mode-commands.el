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

(defun duplicate-current-buffer ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))
  
(defun yank-or-pop ()
  (interactive)
   (if (eq last-command 'yank)
       (yank-pop)
     (yank)))

(defun my-kill-whole-line (arg)
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defmacro toggle-bool (sym)
  `(setf ,sym (not ,sym)))

(defun my-move-beginning-of-line ()
  ;;TODO use mode-hook-initialized variables
  (interactive)
  (cond
      ((eq major-mode 'eshell-mode)
       (eshell-bol))
      
      ((eq major-mode 'slime-repl-mode)
       (beginning-of-line))
      
      ;;((string-match "[*]R[*]" (buffer-name (current-buffer)))
      ((eq major-mode 'inferior-ess-mode)
       (move-past-prompt "^> " t))
      
      ((equal last-command 'my-move-beginning-of-line)
       (if my-move-beginning-of-line-toggle
	   (move-beginning-of-line nil )
	 (back-to-indentation))
       (toggle-bool my-move-beginning-of-line-toggle))
      
      (t (setq my-move-beginning-of-line-toggle t) (back-to-indentation))))

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

(setq kill-surrounding-cum-count)
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
      (set-clipboard killed))))

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

(setq exclude-buffer-cycle (list "*scratch*" "*GNU Emacs*" " *Minibuf-1*" " *Minibuf-0*" "*Messages*" " *code-conversion-work*" " *Echo Area 1*" " *Echo Area 0*" "*Completions*" "*Apropos*" "*Help*"))
(defun cycle-buffer (arg) (interactive "P") 
       (let* ((buflist (buffer-list))
	      (first t))
	 (if (member last-command '(cycle-buffer 'cycle-prev-buffer))
	     (progn
	       (while (or first (member (buffer-name (nth buffer-index buflist)) exclude-buffer-cycle))
		 (setq buffer-index (mod (+ buffer-index (if arg -1 1)) (length (buffer-list))))
		 (setq first nil))
	       (switch-to-buffer (nth buffer-index buflist))
	       )
	   (progn
	     (setq buffer-index
		   (loop for i from 0
			 for buff in (buffer-list)
			 thereis (and (eq (current-buffer) buff) i)))
	     (switch-to-buffer nil)))))

(defun async-shell-command-no-prompt (&rest args)
  (let* ((async-shell-command-buffer 'new-buffer))
    (apply 'async-shell-command args)))

(defun grep-search-default ()
  (interactive)
  ;;(edebug);
  (let* ((sym (sexp-at-point))
	 ;(default (if (and sym (symbolp sym)) (symbol-name sym) nil)))
	 (default (if (and sym) (prin1-to-string sym) nil)))
    (async-shell-command-no-prompt
     (read-shell-command "Enter grep search: " (concat "grep -Risn " default) ))))

(defun my-eval-defun (arg)
  (interactive "P")
  (if (and (boundp 'slime-mode) slime-mode)
      (let ((slime-load-failed-fasl 'always))
	(slime-compile-and-load-file '(4)))
    (eval-defun arg)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun find-iregex (regex directory)
  (interactive (list
		(read-string "enter regex: ")
		(when current-prefix-arg
		  (expand-file-name
		   (read-directory-name "enter directory: ")))))
  (unless directory
    (setf directory (expand-file-name default-directory)))
  (message directory)
  (start-process "findiregex" "findiregex" "find"
		 directory "-iregex" (format ".*%s.*" regex))
  (switch-to-buffer "findiregex")
  (beginning-of-buffer))

(defun gen-new-buffer (&optional name)
  (generate-new-buffer (generate-new-buffer-name (or name "new-buffer"))))

(defun find-new-buffer  ()
  (interactive)
  (let* ((default-directory "/tmp/")
	 (buff-name (gen-new-buffer)))
    (switch-to-buffer-other-window buff-name)
    (auto-save-mode 1)))

(defun sudo-buffer () (interactive)
       (let ((curr-fn (if (eq major-mode 'dired-mode)
			  dired-directory
			(buffer-file-name (current-buffer)))))
	 
	 (unless (s-starts-with-p "/sudo" curr-fn)
	   (let ((pos (point)))
	     (find-file (concat "/sudo::" curr-fn))
	     (goto-char pos)))))
       

(require 'f)
(defun grep-extension (extension pattern dir &optional clear-buffer)
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
	  
	  (ext (read-string
		"enter extension (eg 'js'): "
		(f-ext (or (buffer-file-name (current-buffer)) ""))))
	  (dir (if current-prefix-arg
		   (read-directory-name "enter directory: ")
		 default-directory)))
     (list
      (and (not (string= "" ext)) ext)
      pattern
      (expand-file-name dir)
      t)))
  
  (let ((buff-name "grep-extension"))

    (when clear-buffer
      (switch-to-buffer buff-name)
      (erase-buffer))
    
    (if extension
	(start-process buff-name buff-name 
		       "find"
		       dir
		       "-name" (concat "*" extension)
		       "-exec" "grep" "-Hins" pattern "{}" ";")
      
      (start-process buff-name buff-name
		     "grep" "-RHins" pattern dir))
    (switch-to-buffer buff-name)
    (beginning-of-buffer)))

(defun kill-current-buffer-filename ()(interactive)
       (let ((fn
	      (replace-regexp-in-string
	       "^/sudo:root@.*?:" ""
	      (buffer-file-name (current-buffer)))))
	 
	 (kill-new fn)
	 (message "killed: %s" fn)
	 fn))

(defun message-current-buffer-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
	(message "%s" (process-command proc))
      (message "buffer has no process"))))

(provide 'command-mode-commands)
;;; command-mode-commands.el ends here
