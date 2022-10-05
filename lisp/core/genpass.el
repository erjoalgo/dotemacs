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

(defvar genpass-bag-syms nil "Symbols denoting genpass character bags.")

(defmacro genpass-def-char-bags (&rest name-spec-pairs)
  "Convenience macro defines a series of named character bags NAME-SPEC-PAIRS."
  (let (syms)
  `(progn
     ,@(cl-loop for (name spec) in name-spec-pairs
                as sym = (intern (concat "genpass-" (symbol-name name)))
                collect `(defvar ,sym nil)
                collect `(setq ,sym (genpass-ranges-to-bag ,spec))
                do (push sym syms))
     (setf genpass-bag-syms ',syms))))

(genpass-def-char-bags
  (alpha-lower "az")
  (alpha-upper "AZ")
  (alpha "azAZ")
  (num "09")
  (alnum "azAZ09")
  (special-chars "!/:@")
  (all "azAZ09!/:@"))

(defvar genpass-default-len 13)

(defun genpass-set-clipboard (text)
  "Set the clipboard to the string TEXT."
  (gui-set-selection 'CLIPBOARD text)
  (gui-set-selection 'PRIMARY text))

(defun genpass-genpass (n bag &optional no-kill)
  "Generate a passsword of length N using char-bag BAG.  NO-KILL skips clipboard."
  (interactive (list (read-number "password length: " genpass-default-len)
                     (symbol-value
                      (intern
                       (completing-read "select character bag: "
                                        (mapcar #'symbol-name genpass-bag-syms)
                                        nil t "genpass-alnum")))))
  (cl-loop with str = (make-string n 0)
	   for i below n do
	   (aset str i (seq-random-elt bag))
	   finally
           (cl-return
            (prog1
                str
              (unless no-kill
                (message "%s" str)
                (if window-system
                    (genpass-set-clipboard str)
                  (kill-new str)))))))

(ert-deftest genpass-test ()
  (cl-loop for n below 20 do
           (cl-loop for (fmt bag)
                    on (list
                        "[a-z]" genpass-alpha-lower
                        "[A-Z]" genpass-alpha-upper
                        "[a-zA-Z]" genpass-alpha
                        "[0-9]" genpass-num
                        "[a-zA-Z0-9]" genpass-num)
                    by #'cddr
                    as regexp = (format "%s\\{%d\\}" fmt n) do
                    (should
                     (string-match regexp (genpass-genpass n bag))))))

(defun genpass-explode (tmpl fuzz)
  "Return all possible strings such that TMPL[i] \\in (TMPL[i] \\cup poss)

   where poss is a string in FUZZ that includes character TMPL[i]"
  (cl-loop with cands = '("")
           for c across tmpl
           as opts = (or (cl-loop for s in fuzz thereis (and (cl-find c s) s))
                         (char-to-string c))
           do
           (setq cands
                 (cl-loop for cc across opts
                          append
                          (cl-loop for cand in cands
                                   collect
                                   (concat cand
                                           (char-to-string cc)))))
           finally (return cands)))

(defun genpass-explode-to-file (approx-password filename fuzz)
  "Write to FILENAME each candidate passwod generated by (GENPASS-EXPLODE APPROX-PASSWORD FUZZ)."
  (interactive (list
                (read-string "enter approx passoword: ")
                (read-file-name "enter filename: " "/tmp/" nil nil "cands")
                (split-string (read-string "enter space-separted fuzz strings: ")
                              " ")))
  (with-current-buffer (find-file-noselect filename)
    (erase-buffer)
    (dolist (cand (genpass-explode approx-password fuzz))
      (insert cand)
      (newline))
    (save-buffer)))

(ert-deftest genpass-test-explode ()
  (should (eq 81 (length (genpass-explode "internet" '("e3E" "7tT"))))))

(provide 'genpass)
;;; genpass.el ends here
