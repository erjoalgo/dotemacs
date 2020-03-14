;;; selcand.el --- Select a candidate from a tree of hint characters
;;
;; Filename: selcand.el
;; Description:
;; Author: Ernesto Alfonso
;; Maintainer: (concat "erjoalgo" "@" "gmail" ".com")
;; Created: Thu Jan 24 00:18:56 2019 (-0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defcustom selcand-default-hints
  ;; "qwertasdfzxcv1234"
  "1234acdefqrstvwxz"
  "Default hint chars."
  :type 'string
  :group 'selcand)

(defun selcand-hints (cands &optional chars)
  "Return an alist (HINT . CAND) for each candidate in CANDS.

  each hint consists of characters in the string CHARS."
  (setf chars (or chars selcand-default-hints))
  (cl-assert cands)
  (let* ((w (ceiling (log (length cands) (length chars))))
         (hints (cl-loop with curr = '("")
                         for wi below w do
                         (setf curr
                               (cl-loop for c across chars
                                        append (mapcar (apply-partially
                                                        'concat (char-to-string c))
                                                       curr)))
                         finally (return curr))))
    (cl-loop for hint in hints
             for cand in cands
             collect (cons hint cand))))

(cl-defun selcand-select (cands &optional prompt stringify autoselect-if-single
                                initial-input read-char)
  "Use PROMPT to prompt for a selection from CANDS candidates."
  (let* ((hints-cands (selcand-hints cands))
         (sep ") ")
         (stringify (or stringify #'prin1-to-string))
         (initial-candidate nil)
         (choices (cl-loop for (hint . cand) in hints-cands
                           as string = (funcall stringify cand)
                           as choice = (concat hint sep string)
                           when (equal string initial-input)
                           do (setq initial-candidate choice)
                           collect choice))
         (prompt (or prompt "select candidate: "))
         (choice (if (and autoselect-if-single (null (cdr choices)))
                     (car choices)
                   (cond
                    (read-char
                     (char-to-string
                      (read-char
                       (concat prompt "\n" (s-join "\n" choices)))))
                    (t (minibuffer-with-setup-hook
                           #'minibuffer-completion-help
                         (completing-read prompt choices
                                          nil
                                          t
                                          initial-candidate))))))
         (cand (let* ((hint (car (split-string choice sep))))
                 (cdr (assoc hint hints-cands #'equal)))))
    cand))

(provide 'selcand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; selcand.el ends here
