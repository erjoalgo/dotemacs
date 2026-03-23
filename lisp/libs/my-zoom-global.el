;;; zoom-global.el ---

;; Copyright (C) 2016

;; Author: unknown
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
;; I did not write this code but I'm unable to trace who did.
;; Conveniently increase-decrease text size for emacs

;;; Code:



(define-globalized-minor-mode
  my-global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defvar-local text-scale-mode-amount 0)
(setq-default text-scale-mode-amount 0)

(defun my-global-text-scale-adjust (inc) (interactive)
       (message "DDEBUG my-global-text-scale-adjust: %s" inc)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (my-global-text-scale-mode 1))

(defun my-global-text-scale-reset ()
  (interactive)
  (message "DDEBUG g8yj my-global-text-scale-reset TRACE")
  (my-global-text-scale-adjust (- text-scale-mode-amount))
  (my-global-text-scale-mode -1))

(defun my-global-text-scale-reset-to (amount)
  ;;(setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (message "DDEBUG dyeb my-global-text-scale-reset-to TRACE")
  (setq-default text-scale-mode-amount amount)
  (my-global-text-scale-mode 1))

;;;###autoload
(defun my-global-text-scale-lower () (interactive)
       (message "DDEBUG my-global-text-scale-lower TRACE")
       (if (not (equal major-mode 'doc-view-mode))
	   (progn (message "DDEBUG lpwd non-doc-view modeTRACE")
                  (my-global-text-scale-adjust -1))
	 (progn (message "DDEBUG pj0c doc-view mode TRACE")
                (doc-view-shrink 1.125)))
       '(my-global-text-scale-adjust -1))

;;;###autoload
(defun my-global-text-scale-higher () (interactive)
       (message "DDEBUG my-global-text-scale-higher TRACE")
       (if (not (equal major-mode 'doc-view-mode))
           (progn
             (message "DDEBUG non-doc-view-mode TRACE")
             (my-global-text-scale-adjust 1))
	 (progn
           (message "DDEBUG doc-view-mode TRACE")
           (doc-view-enlarge 1.125)))
       '(my-global-text-scale-adjust 1))

;;(global-set-key (kbd "M-0") 'my-global-text-scale-reset)
;;(global-set-key (kbd "M-+") '(lambda () (interactive) (my-global-text-scale-adjust 1)))
;;(global-set-key (kbd "M-_") '(lambda () (interactive) (my-global-text-scale-adjust -1)))

(provide 'my-zoom-global)
;;; zoom-global.el ends here
