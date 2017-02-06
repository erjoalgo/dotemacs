;;; genpass.el ---

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


(defun genpass-ranges-to-bag (ranges)
  (assert (evenp (length ranges)))
  (loop with bag = ""
   for i below (length ranges) by 2
	as a = (aref ranges i)
	as b = (aref ranges (1+ i))
	as str = (let ((b (max a b)) (a (min a b)) len str)
		   (setf len (1+ (- b a)))
		   (setf str (make-string len 0))
		   (loop for c from a upto b
			 for i from 0 do
			 (aset str i c))
		   str)
	do (setf bag (concat bag str))
	finally (return bag)))

;(defvar *genpass-alnum* (genpass-ranges-to-bag "azAZ09"))
(defvar *genpass-letters-lower* (genpass-ranges-to-bag "az"))
(defvar *genpass-letters-upper* (genpass-ranges-to-bag "AZ"))
(defvar *genpass-letters* (concat *genpass-letters-upper* *genpass-letters-lower*))
;(defvar *genpass-letters* (genpass-ranges-to-bag "azAZ"))
(defvar *genpass-num* (genpass-ranges-to-bag "09"))
(defvar *genpass-alnum* (concat *genpass-num* *genpass-letters*))
(defvar *genpass-special-chars* (genpass-ranges-to-bag "!/:@"))
(defvar *genpass-all* (concat *genpass-special-chars* *genpass-alnum*))
(defvar *genpass-default-len* 13)

(defun genpass-genpass (n bag)
  ;;(interactive "npassword length: ")
  (interactive (list (if current-prefix-arg
			 (read-number "password length: ")
		       *genpass-default-len*)
		     *genpass-alnum*))
  (loop with str = (make-string n 0)
	with bag-len = (length bag)
	for i below n do
	(aset str i (aref bag (random bag-len)))
	finally (progn (message str)
		       (if window-system
			   (set-clipboard str)
			 (kill-new str))
		       (return str))))

(provide 'genpass)
;;; genpass.el ends here

