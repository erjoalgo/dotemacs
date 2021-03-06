;;; org-omnibar.el ---

;; Copyright (C) 2017  Ernesto Alfonso <ernesto.alfonsogonzalez@ge.com>

;; Author: Ernesto Alfonso <ernesto.alfonsogonzalez@ge.com>
;; Keywords: convenience
;; Created: 13 Jan 2017

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



(defvar org-top-dir)

(defun org-all-headings (top-level &optional max-depth)
  (cl-loop with org-files = (remove-if-not
			  (lambda (fn) (equal "org"
					      (f-ext fn)))
			  (f--collect-entries top-level t))
	for org-file in org-files nconc
	(org-file-headings org-file max-depth)))

(defun org-file-headings (org-filename &optional max-depth)
  (interactive (list (buffer-file-name)))
  (let ((heading-regexp
	 (format "^[*]%s \\(.*\\)"
		 (if (not max-depth) "+"
		   (format "\\{1,%d\\}" max-depth)))))
    ;;(message "heading regexp: %s" heading-regexp)
    (save-excursion-file
     org-filename
     (goto-char (point-min))
     (cl-loop while (search-forward-regexp heading-regexp nil t)
	   as heading = (match-string 1)
	   collect (cons heading (cons org-filename (point)))))))

(defun org-goto-heading (org-heading)
  (interactive (let* ((org-headings (org-all-headings org-top-dir 4))
		      (heading-text (completing-read "enter org heading: " org-headings)))
		 (list (assoc heading-text org-headings))))
  (cl-destructuring-bind (heading-text . (filename . point)) org-heading
    (find-file filename)
    (goto-char point)))

(provide 'org-omnibar)
;;; org-omnibar.el ends here
