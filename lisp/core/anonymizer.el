;;; anonymizer.el ---

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


(defun anonymizer-anonymize ()
  (interactive)
  (let ((words `((,system-name "my-hostname")
		       (,user-login-name "MY-NAME" )
		       (,user-real-login-name "MY-NAME")
		       (,user-mail-address "me@example.com")
		       (,(first (s-split " " user-full-name)) "MY-FIRST-NAME")
		       (,(second (s-split " " user-full-name)) "MY-LAST-NAME")
		       ("erjoalgo" "user")))
	(regexps `(("[[:alnum:]]+@[[:alnum:]]+[.][[:alnum:]]+" "user@example.com")
		   ("@[[:alnum:]]+[.][[:alnum:]]+" "@example.com")
		   ("http://[^/[:space:]]+" "http://example.com")
		   ("https://[^/[:space:]]+" "https://example.com")
		   ))
	(total-count 0))

    (setf words (loop for (word replacement) in words
		      collect (cons (downcase word) replacement)))

    (cl-labels ((replace-regexp-in-buffer (regexp replacement)
					  (message "replacing occurrences of '%s'" regexp)
					  (let ((count 0))
					    (save-excursion
					      (beginning-of-buffer)
					      (while (re-search-forward regexp nil t)
						(let* ((match (match-string 0))
						      (replacement (funcall replacement match)))
						  (assert replacement nil "no replacement for '%s'" match)
						  (message "replacing '%s' with '%s'"
							   match replacement)
						  (replace-match replacement t t)
						  (incf count))))
					    (message "%d matches replaced for '%s'" count regexp)
					    count))
		(ors-regexp (regexps)
			    (s-join "\\|" (mapcar 'regexp-quote (mapcar 'car regexps)))))

      (incf total-count (replace-regexp-in-buffer (ors-regexp words) (lambda (match)
						     (cdr (assoc (downcase match) words)))))

      (incf total-count (loop for (regexp replacement) in regexps sum
			      (replace-regexp-in-buffer regexp `(lambda (match) ,replacement))))
      (message "%d total matches replaced" total-count))))

(defun anonymizer-scramble-region (a b)
  "replace any alnum chars in region with random ones. useful for anonymizing UIDS, RSA keys, while keeping structure"
  (interactive "r")
  (let ((region (buffer-substring-no-properties a b)))
    (loop for i below (length region) do
	  (let ((char-string (substring region i (1+ i)))
		bag
		(case-fold-search nil))

	    (setf bag
		  (cond
	     ((string-match "[a-z]" char-string) *genpass-letters-lower*)
	     ((string-match "[A-Z]" char-string) *genpass-letters-upper*)
	     ((string-match "[0-9]" char-string) *genpass-num*)))

	    (when bag
	      (let ((new-char (aref bag (random (length bag)))))
		    (aset region i new-char)))))
    (delete-region a b)
    (goto-char a)
    (insert region)))


(provide 'anonymizer)
;;; anonymizer.el ends here


