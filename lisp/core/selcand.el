(defcustom selcand-default-hints
  "qwertasdfzxcv1234"
  "Default hint chars."
  :type 'string
  :group 'selcand)

(defun selcand-hints (cands &optional chars)
  "Return an alist (HINT . CAND) for each candidate in CANDS.

  each hint consists of characters in the string CHARS."
  (setf chars (or chars selcand-default-hints))
  (assert cands)
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

(defun selcand-select (cands &optional prompt stringify)
  "Use PROMPT to prompt for a selection from CANDS candidates."
  (let* ((hints-cands (selcand-hints cands))
         (sep ") ")
         (stringify (or stringify #'prin1-to-string))
         (choices (cl-loop for (hint . cand) in hints-cands
                           collect (concat hint sep (funcall stringify cand))))
         (prompt (or prompt "select candidate: "))
         (choice (completing-read prompt choices
                                  nil
                                  t))
         (cand (let* ((hint (car (s-split sep choice))))
                 (cdr (assoc hint hints-cands #'equal)))))
    cand))

(provide 'selcand)
