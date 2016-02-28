;;; isearch-fast-reverse.el --- 

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
;;Make switching directions in isearch mode faster. Go to previous item with a single keystroke.
;;Make regexp search default
;;Bind forward search, reverse search to f3, M-f3
;;Esc to exit isearch at current position
;;f4 to cancel isearch and return to original, pre-search position


;;; Code:
(define-key isearch-mode-map [f3] 'isearch-forward-or-backward)
(define-key isearch-mode-map [M-f3] 'isearch-reverse-search-and-search)
(define-key isearch-mode-map [escape] 'isearch-exit)
(define-key isearch-mode-map [f4] 'isearch-abort)

(global-set-key [f3] 'isearch-forward-regexp)
;should work because it is a recedit
(global-set-key [M-f3] (lambda () (interactive)
			 (let* ((isearch-reverse-direction-p t))
			   (isearch-backward-regexp))))


(defvar isearch-reverse-direction-p nil)

(defun isearch-forward-or-backward ()
  (interactive)
  (let* ((case-fold-search t))
    (if isearch-reverse-direction-p (isearch-repeat-backward) (isearch-repeat-forward))))


(defun isearch-reverse-search-and-search ()
  (interactive)
  (setq isearch-reverse-direction-p (not isearch-reverse-direction-p))
  (isearch-forward-or-backward)
  (isearch-forward-or-backward))


(add-hook 'isearch-mode-end-hook (lambda () (setq isearch-reverse-direction-p nil)))


(defadvice isearch-exit (before dired-search-maybe-follow activate)
  (when (and (eq major-mode 'dired-mode)
	     (or
	      (equal (this-command-keys) "")
	      (equal (this-command-keys) [return])))
    (dired-find-file)))

(defadvice isearch-forward-regexp (around force-case-fold activate)
  (let* ((case-fold-search t))
    ad-do-it))

(defadvice isearch-reverse-exit (after dired-search-maybe-follow activate)
  (when (and (eq major-mode 'dired-mode)
	     (or
	      (equal (this-command-keys) "")
	      (equal (this-command-keys) [return])))
    (dired-find-file)))

(provide 'isearch-fast-reverse)
;;; isearch-fast-reverse.el ends here
