(defun explode (tmpl fuzz)
  (loop with cands = '("")
        for c across tmpl
        as opts = (or (loop for s in fuzz thereis (and (find c s) s))
                      (char-to-string c))
        do
        (setq cands
              (loop for cc across opts
                    append
                    (loop for cand in cands
                          collect
                          (concat cand
                                  (char-to-string cc)))))
        finally (return cands)))

(defun explode-to-file (aprox-password filename fuzz)
  (interactive (list
                (read-string "enter approx passoword: ")
                (read-file-name "enter filename: " "/tmp/" nil nil "cands")
                (s-split " "
                         (read-string "enter space-separted fuzz strings: "))))
  (with-current-buffer (find-file-noselect filename)
    (erase-buffer)
    (dolist (cand (explode approx-password))
      (insert cand)
      (newline))
    (save-buffer)))

(ert-deftest test-explode ()
  (should (eq 81 (length (explode "internet" '("e3E" "7tT"))))))
