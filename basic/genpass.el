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
		       (set-clipboard str)
		       (return str))))
(defun alnum-scramble-region (a b)
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
  
