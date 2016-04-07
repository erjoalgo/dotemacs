(defun genpass-ranges-to-bag (ranges)
  (assert (evenp (length ranges)))
  (loop for i below (length ranges) by 2
	as a = (aref ranges i)
	as b = (aref ranges (1+ i))
	nconc (loop for c from a upto b collect c)))

(defvar *genpass-alnum* (genpass-ranges-to-bag "azAZ09"))
(defvar *genpass-num* (genpass-ranges-to-bag "09"))
(defvar *genpass-special-chars* (genpass-ranges-to-bag "!/:@"))
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
	(aset str i (nth (random bag-len) bag))
	finally (progn (message str)
		       (set-clipboard str)
		       (return str))))
