;;; plusx.el ---

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

(defcustom plusx-interpreter-line-rules
  '(("[.]py$" "#!/usr/bin/python")
    ("[.]sh$" "#!/bin/bash")
    ("[.]bash$" "#!/bin/bash")
    ("[.]lisp$" "#!/usr/bin/sbcl --script")
    ("[.]perl$" "#!/usr/bin/perl")
    )
  "regexp --> shebang")


(defun plusx-maybe-insert-interpreter-line ()
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
	 (shebang
	  (loop for (regexp shebang) in plusx-interpreter-line-rules
		thereis (and (string-match regexp fn) shebang))))

    (if (not shebang) (message "no match for %s" fn)
      (save-excursion
	(goto-char (point-min))
	(unless (looking-at "#!")
	  (insert shebang)
	  (open-line 1)
	  ;;(save-buffer)
	  ;;(chmodx)
	  )))))


(defun plusx (fn &optional link-bin-p)
  (interactive (list (buffer-file-name (current-buffer))
		     nil))
  (plusx-maybe-insert-interpreter-line)
  (shell-command (format "chmod +x '%s'" fn))

  (unless (file-name-absolute-p fn) (setq fn (expand-file-name fn)))
  (let* ((bin-dir "~/bin")
	 (bin-link (f-join bin-dir (f-filename fn))))

    (when (and link-bin-p
	       (not (file-exists-p bin-link)))
      (message "creating symlink for %s" fn)
      (shell-command (format "ln -s %s %s" fn bin-link)))))

(defun plusx-and-link (fn)
  (plusx fn))


(provide 'plusx)
;;; plusx.el ends here
