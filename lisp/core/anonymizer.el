;;; anonymizer.el --- Anonymize sensitive text before making it public

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
(require 's)
(require 'genpass)

(defcustom anonymizer-custom-words
  ()
  "Custom words to anonymize.

   May be a list of strings, a function, or a string"
  :type '(choice list string function)
  :group 'anonymizer)

(defun anonymizer-anonymize (a b)
  "Anonymize region A B."
  (interactive "r")
  (unless (region-active-p)
    (setf a nil b nil))
  (let ((words `((,(system-name) "my-hostname")
		 (,user-login-name "USER" )
		 (,user-real-login-name "MY-NAME")
		 (,user-mail-address "me@example.com")
		 (,(cl-first (s-split " " user-full-name)) "MY-FIRST-NAME")
		 (,(cl-second (s-split " " user-full-name)) "MY-LAST-NAME")))
        (custom-words (cl-typecase anonymizer-custom-words
                        (list anonymizer-custom-words)
                        (function (funcall anonymizer-custom-words))
                        (string (split-string anonymizer-custom-words))))
	(regexps `(("[[:alnum:]]+@[[:alnum:]]+[.][[:alnum:]]+" "user@example.com")
		   ("@[[:alnum:]]+[.][[:alnum:]]+" "@example.com")
		   ("http://[^/[:space:]]+" "http://example.com")
		   ("https://[^/[:space:]]+" "https://example.com")
		   ("\\(\\([0-9a-fA-F]\\{2\\}:\\)\\{5\\}[0-9a-fA-F]\\)" "00:00:00:00:00:00")
		   ("\\(\\([0-9]\\{1,3\\}[.]\\)\\{3\\}[0-9]\\{1,3\\}\\)" "8.8.8.8")))
        (case-fold-search t)
	(total-count 0))

    (setf words (cl-loop for (word replacement) in words
		         collect (cons (downcase word) replacement)))

    (cl-labels ((replace-regexp-in-buffer (regexp replacement)
					  (message "replacing occurrences of '%s'" regexp)
					  (let ((count 0))
					    (save-excursion
					      (goto-char (or a (point-min)))
					      (while (re-search-forward regexp b t)
						(let* ((match (match-string 0))
						       (replacement (funcall replacement match)))
						  (cl-assert replacement nil "no replacement for '%s'" match)
						  (message "replacing '%s' with '%s'"
							   match replacement)
						  (replace-match replacement t t)
						  (cl-incf count))))
					    (message "%d matches replaced for '%s'" count regexp)
					    count))
		(ors-regexp (regexps)
			    (s-join "\\|" (mapcar 'regexp-quote (mapcar 'car regexps)))))

      (cl-incf total-count (replace-regexp-in-buffer
                            (ors-regexp (append words custom-words)) (lambda (match)
						                       (cdr (assoc (downcase match) words)))))

      (cl-incf total-count (cl-loop for (regexp replacement) in regexps sum
			            (replace-regexp-in-buffer regexp `(lambda (match) ,replacement))))
      (message "%d total matches replaced" total-count))))

(defun anonymizer-scramble-region (a b)
  "Replace any alnum chars in region A B with random ones.

  May be used for anonymizing UIDS, RSA keys, while maintaining the overall structure."
  (interactive "r")
  (let ((scrambled (buffer-substring-no-properties a b)))
    (cl-loop for i below (length scrambled) do
	     (let ((char-string (substring scrambled i (1+ i)))
		   bag
		   (case-fold-search nil))

	       (setf bag
		     (cond
	              ((string-match "[a-z]" char-string) genpass-letters-lower)
	              ((string-match "[A-Z]" char-string) genpass-letters-upper)
	              ((string-match "[0-9]" char-string) genpass-num)))

	       (when bag
	         (let ((new-char (aref bag (random (length bag)))))
		   (aset scrambled i new-char)))))
    (save-excursion
      (goto-char a)
      (let ((region-text (buffer-substring a b)))
        (while (search-forward region-text nil t)
          (replace-match scrambled))))))


(provide 'anonymizer)
;;; anonymizer.el ends here
