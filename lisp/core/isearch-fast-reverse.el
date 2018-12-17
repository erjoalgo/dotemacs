;;; isearch-fast-reverse.el --- Make it easier to reverse isearch direction

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
;;;  Make switching directions in isearch mode faster.
;;;  Changes:
;;;  Make regexp search the default search
;;;  Reverse directions and get next match in a single keystroke.
;;;  Bind forward search, reverse search to f3, M-f3
;;;  Esc to exit isearch at current position
;;;  C-g or f4 to cancel isearch and return to original, pre-search position
;;;  Exiting while on isearch-forward will move point to beginning of match, not at the end.
;;;  This makes it easier to move point where desired via isearch-forward
;;;  In dired or gnus, RET automatically opens file/directory at point


;;; Code:
(define-key isearch-mode-map [f3] 'isearch-forward-or-backward)
(define-key isearch-mode-map [M-f3] 'isearch-reverse-search-and-search)
(define-key isearch-mode-map [escape] 'isearch-exit)
(define-key isearch-mode-map [f4] 'isearch-abort)

(global-set-key [f3] 'isearch-forward-regexp)
;should work because it is a recedit
(global-set-key [M-f3] (lambda () (interactive)
			 (let* ((isearch-backwards-p t))
			   (isearch-backward-regexp))))


(defvar isearch-backwards-p nil)

(defun isearch-forward-or-backward ()
  "Do an isearch either forward or backward based on ISEARCH-BACKWARDS-P."
  (interactive)
  (let* ((case-fold-search t))
    (if isearch-backwards-p (isearch-repeat-backward)
      (isearch-repeat-forward))))


(defun isearch-reverse-search-and-search ()
  "Reverse isearch direction and search."
  (interactive)
  (setq isearch-backwards-p (not isearch-backwards-p))
  (isearch-forward-or-backward)
  (isearch-forward-or-backward))


(add-hook 'isearch-mode-end-hook
	  (lambda ()
	    "Clean up ‘isearch-backwards-p' state."
	    (setq isearch-backwards-p nil)))

(defadvice isearch-forward-regexp (around force-case-fold activate)
  "Force isearch case fold always."
  (let* ((case-fold-search t))
    (when current-prefix-arg
      (set-mark-command nil))
    ad-do-it))

(defadvice isearch-reverse-exit (after dired-search-maybe-follow activate)
  "Maybe re-issue the RET key used to stop an isearch onto the major mode map."
  (isearch-ret-maybe-passthrough))

(defadvice isearch-exit (before dired-search-maybe-follow activate)
  "Maybe re-issue the RET key used to stop an isearch onto the major mode map.

   When exiting isearch foward normally, move point to the begining
   of the match, not the end.
   This means point should be left at the begining of a variable or another
   interesting place for further edit, saving a call to e.g. ‘backward-word'."
  (isearch-ret-maybe-passthrough)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


(defun isearch-ret-maybe-passthrough ()
  "Maybe re-issue the RET key used to stop an isearch onto the major mode map."
  (when (member (this-command-keys) '([return] ""))
    (cl-case major-mode
      ('dired-mode (dired-find-file))
      ('gnus-summary-mode (gnus-summary-scroll-up nil)))))


(provide 'isearch-fast-reverse)
;;; isearch-fast-reverse.el ends here
