;;; sexchange-mode.el --- 

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

;;;stands for stack exchange. bindings for creating stack-* questions/answers

;;; Code:

(define-minor-mode sexchange-mode
  "mode for creating stack-exchange questions/answers"
  0
  "s-exchange"
  (make-sparse-keymap))

(defun sexchange-insert-cmd-and-output (cmd)
  (interactive (list (read-shell-command
		      "senter shell command: ")))
  (insert (format "\n\n%s\n%s\n\n"
		  (sexchange-code-block
		   (concat "$" cmd))
		  (sexchange-code-block (shell-command-to-string cmd)))))

(defun sexchange-code-block (code)
  (replace-regexp-in-string
   "^" "    " code))


(defun sexchange-code-block-region (a b)
  (interactive "r")
  (save-excursion
    (goto-char a)
    (while (<= (point) b)
      (beginning-of-line)
      (insert "    ")
      (next-logical-line))))

(loop for (key binding) in
      `((,(kbd "s-k") sexchange-code-block-region)
	
	(,(kbd "s-!") sexchange-insert-cmd-and-output))
      do 
      (define-key sexchange-mode-map key binding))

(provide 'sexchange-mode)
;;; sexchange-mode.el ends here
