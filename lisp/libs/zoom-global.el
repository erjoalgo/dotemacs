;;; zoom-global.el ---

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
;; I did not produce this code but I'm unable to trace who did.
;; Convenient increase-decrease text size for emacs

;;; Code:



(define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
    (text-scale-set 1)
    (kill-local-variable 'text-scale-mode-amount)
    (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
    (global-text-scale-mode 1))

(defun global-text-scale-reset ()
  (interactive)
  (global-text-scale-adjust (- text-scale-mode-amount))
  (global-text-scale-mode -1))

(defun global-text-scale-reset-to (amount)
  ;;(setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (setq-default text-scale-mode-amount amount)
  (global-text-scale-mode 1))

(defun global-text-scale-lower () (interactive)
       (if (not (equal major-mode 'doc-view-mode))
	   (global-text-scale-adjust -1)
	 (doc-view-shrink 1.125)
	 )
       '(global-text-scale-adjust -1))

(defun global-text-scale-higher () (interactive)
       (if (not (equal major-mode 'doc-view-mode))
	   (global-text-scale-adjust 1)
	 (doc-view-enlarge 1.125)
	 )
       '(global-text-scale-adjust 1))

;;(global-set-key (kbd "M-0") 'global-text-scale-reset)
;;(global-set-key (kbd "M-+") '(lambda () (interactive) (global-text-scale-adjust 1)))
;;(global-set-key (kbd "M-_") '(lambda () (interactive) (global-text-scale-adjust -1)))

(provide 'zoom-global)
;;; zoom-global.el ends here
