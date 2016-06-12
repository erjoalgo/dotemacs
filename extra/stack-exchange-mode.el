;;; stack-exchange-mode.el --- 

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

;TODO this should be implemented in org mode and md export
(define-minor-mode stack-exchange-mode
  "mode for creating stack-exchange questions/answers"
  nil
  "s-exchange"
  (make-sparse-keymap))

(defun stack-exchange-insert-cmd-and-output (cmd)
  (interactive (list (read-shell-command
		      "senter shell command: ")))
  (insert (format "\n\n%s\n%s\n\n"
		  (stack-exchange-code-block
		   (concat "$" cmd))
		  (stack-exchange-code-block (shell-command-to-string cmd)))))

(defun stack-exchange-code-block (code)
  (replace-regexp-in-string
   "^" "    " code))


(defun stack-exchange-code-block-region (a b)
  (interactive "r")
  (save-excursion
    (goto-char a)
    (while (<= (point) b)
      (beginning-of-line)
      (insert "    ")
      (next-logical-line))))

(loop for (key binding) in
      `((,(kbd "s-k") stack-exchange-code-block-region)

	(,(kbd "s-!") stack-exchange-insert-cmd-and-output))
      do
      (define-key stack-exchange-mode-map key binding))

(provide 'stack-exchange-mode)
;;; stack-exchange-mode.el ends here
