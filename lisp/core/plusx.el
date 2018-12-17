;;; plusx.el --- Make files executable

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

;;; Code:


(require 'f)

(defvar plusx-interpreter-line-alist
  "An alist where each entry is a cons cell of the form
(MAJOR-MODE . INTERPRETER-LINE)")

(setf plusx-interpreter-line-alist
  '((python-mode "#!/usr/bin/python")
    (sh-mode "#!/bin/bash -x\n\nset -euo pipefail")
    (lisp-mode)
    (perl-mode "#!/usr/bin/perl")
    (js-mode "#!/usr/bin/env node")))

(defun plusx-maybe-insert-interpreter-line ()
  "Try to insert the appropriate interpreter line in the current buffer."
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
	 (shebang
	  (cadr (assoc major-mode plusx-interpreter-line-alist))))

    (unless shebang (error "No match for %s" major-mode))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "#!")
	(insert shebang)
	(open-line 1)))))


(defvar plusx-bin-directory
  (expand-file-name "~/bin")
  "The bin directory where to symlink executables.")

(defun plusx (filename &optional link-bin-p)
  "Make executable the file FILENAME.

  If LINK-BIN-P is non-nil, link FILENAME to ‘plusx-bin-directory'"
  (interactive (list (buffer-file-name (current-buffer))
		     current-prefix-arg))
  (plusx-maybe-insert-interpreter-line)
  (shell-command (format "chmod +x '%s'" filename))

  (unless (file-name-absolute-p filename) (setq filename (expand-file-name filename)))
  (let* ((bin-dir plusx-bin-directory)
	 (bin-link (f-join bin-dir (f-filename filename))))

    (when (and link-bin-p
	       (not (file-exists-p bin-link)))
      (message "creating symlink for %s" filename)
      (shell-command (format "ln -s %s %s" filename bin-link)))
    (unless (f-ext filename)
      (add-file-local-variable 'mode major-mode))))

(provide 'plusx)
;;; plusx.el ends here
