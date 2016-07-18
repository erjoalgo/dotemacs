;;; dired-quick-tagger.el --- 

;; Copyright (C) 2014  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: convenience, extensions, tools

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
;;; Adds functionality to dired by enabling batch-tagging of files,
;;; via 'dired-quick-tagger-tag-all-files', and renaming (moving) each tag class to
;;; specific directories, via 'dired-quick-tagger-rename-all-tags'.

;;; Code:


(defun dired-quick-tagger-rename-char (&optional char noconfirm)
  (interactive "cEnter char of file class to rename")
  (when (find char '(42 93))
    (error "char 42 and 93 are reserved. use a different one"))
  
  (dired-change-marks 42 93)
  (dired-change-marks char 42)
      
  ;prevent an error from leaving us in a bad state
  (try-catch-finally-no-quit
   (progn
   (dired-quick-tagger-move-marked-to-dest nil noconfirm)
   (dired-change-marks 93 42))
   (progn ;undo mark changes
     (dired-change-marks 42 char)
     (dired-change-marks 93 42))
   (revert-buffer))
  )
(defun dired-quick-tagger-move-marked-to-dest (dest noconfirm)
  (let ((dired-marked (dired-get-marked-files)))
  
  (with-output-to-temp-buffer "*dired-move-various*"
    (princ "Moving the following files:\n\n")
    (mapcar (lambda (fn) (princ (basename fn)) (princ "\n"))
	    dired-marked)
    )
    (unless dest
      (setq dest (read-directory-name
		  (format "Enter destination directory for '%c' tag(it need not exist): " char))))
    (maybe-mkdir dest (not noconfirm))
    (unless (or noconfirm
		(y-or-n-p (format "confirm moving to %s?" dest)))
      (error "user failed to confirm")
      )
  ;(message (format "mv ? %s " dir))
  ;(dired-do-shell-command (format "mv ? %s " dir) nil (dired-get-marked-files))
    (mapc (lambda (fn)
	    (rename-file fn (concat (expand-file-name (file-name-as-directory dest))  (basename fn))))
	  dired-marked)
    (when (not noconfirm) (message "Files probably moved successfully!"))
    )
  )
(defun maybe-mkdir  (fn &optional confirm)
  (if (file-exists-p fn)
	;(unless (car (file-attributes fn));tests whether it is directory
      (unless (file-directory-p fn) (error "Must specify a directory, not a file"))
    (if  (or (not confirm) (y-or-n-p "Specified dir does not exist. Confirm directory creation: "))
	;(shell-command (format "mkdir -p %s" dir));
	(mkdir fn t)
      (error "directory creation not confirmed"))
    )
  )
(defun dired-quick-tagger-rename-all-chars () (interactive)
  (let ((oldp (point)) (chars nil) (char nil))
    (goto-char (point-min))
    (while (search-forward-regexp "^\\([a-z0-9]\\)" (point-max) t)
      
      (setq char (string-to-char (match-string 1)))
      (when (not (equal char 68)) ;68 is the 'D' (Delete) mark
	(dired-quick-tagger-rename-char char t )
	(goto-char (point-min))
	)
      )
    (goto-char oldp)
    )
  )

(defun dired-quick-tagger-tag-all-files () (interactive)
  ;(goto-line 5);it seems dired hides two extra lines from the beginning
  
  (while t (dired-quick-tagger-mark-char) )
  )

(defun dired-quick-tagger-mark-char (&optional char)
  (interactive)
  (unless char
  (setq char (read-char "Enter char for this file (space to skip, backspace to go up) ")))
  (message "char was %c" char)
  (cond
   ((equal char 127) (previous-line))
   ;((equal char 32) (next-line))
   (char (progn
	   (dired-change-marks 42 93)
           ;(message (format "Placed along these files: %s" (dired-get-marked-files)))
	   (dired-mark 1)  
	   (dired-change-marks 42 char)
	   (dired-change-marks 93 42))
    )
   )
  )
(defmacro try-catch-finally-no-quit (try catch finally)
  `(condition-case err
      (let ((inhibit-quit t))
	(unless (with-local-quit
		  ,try
		  ,finally
		  t
		  )
	  (progn
	    (setq quit-flag nil )
	    (error "user pressed quit"))
	)
	(dired-change-marks 93 42)
	(revert-buffer))
    ('error
     (progn
	,catch
       ,finally
       (signal (car err) (cdr err))
       )
     )
    )
  )
(defun basename (fn)
  (let ((expanded (expand-file-name fn)))
    (string-match ".*/\\(.+?\\)/?$" expanded)
    (match-string 1 expanded))
  )

;;TODO this was added from .emacs-bloated
(defun dired-view-mark-files () (interactive)
  (while t
    (let (char)
      (dired-view-file)
      (setq char (read-char "Enter char for this file"))
      (when (not char) (throw 'break))
      (quit-window t)
      (markchardired char)
      ;(when (not (dired-next-line 1)) (throw 'break))
      )
    )
  )

(provide 'dired-quick-tagger)
;;; dired-quick-tagger.el ends here

