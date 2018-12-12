;;; genpass.el --- Generate a password

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: tools

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

(require 'cl-lib)
(require 'seq)
(require 'select)

(defun genpass-ranges-to-bag (ranges)
  "Expand character ranges, specified as start-end character pairs in the string RANGES."
  (cl-assert (cl-evenp (length ranges)))
  (cl-loop for i below (length ranges) by 2
	   as a = (aref ranges i)
	   as b = (aref ranges (1+ i))
	   concat (let* ((b (max a b))
                         (a (min a b))
                         (len (1+ (- b a)))
                         (str (make-string len 0)))
		    (cl-loop for c from a upto b
		             for i from 0 do
		             (aset str i c))
		    str)))

;(defvar genpass-alnum (genpass-ranges-to-bag "azAZ09"))
(defvar genpass-letters-lower (genpass-ranges-to-bag "az"))
(defvar genpass-letters-upper (genpass-ranges-to-bag "AZ"))
(defvar genpass-letters (concat genpass-letters-upper genpass-letters-lower))
;(defvar genpass-letters (genpass-ranges-to-bag "azAZ"))
(defvar genpass-num (genpass-ranges-to-bag "09"))
(defvar genpass-alnum (concat genpass-num genpass-letters))
(defvar genpass-special-chars (genpass-ranges-to-bag "!/:@"))
(defvar genpass-all (concat genpass-special-chars genpass-alnum))
(defvar genpass-default-len 13)

(defun genpass-set-clipboard (text)
  "Set the clipboard to the string TEXT."
  (gui-set-selection 'CLIPBOARD text)
  (gui-set-selection 'PRIMARY text))

(defun genpass-genpass (n bag)
  "Generate a passsword of length N using char-bag BAG."
  (interactive (list (if current-prefix-arg
			 (read-number "password length: ")
		       genpass-default-len)
		     genpass-special-chars))
  (cl-loop with str = (make-string n 0)
	   for i below n do
	   (aset str i (seq-random-elt bag))
	   finally (progn (message str)
		          (if window-system
			      (genpass-set-clipboard str)
			    (kill-new str))
		          (cl-return str))))

(ert-deftest test-genpass ()
  (cl-loop for n below 20 do
           (cl-loop for (fmt bag)
                    on (list
                        "[a-z]" genpass-letters-lower
                        "[A-Z]" genpass-letters-upper
                        "[a-zA-Z]" genpass-letters
                        "[0-9]" genpass-num
                        "[a-zA-Z0-9]" genpass-num)
                    by #'cddr
                    as regexp = (format "%s\\{%d\\}" fmt n) do
                    (should
                     (string-match regexp (genpass-genpass n bag))))))

(provide 'genpass)
;;; genpass.el ends here
