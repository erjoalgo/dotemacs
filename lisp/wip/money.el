(defun zip (a b)
  (cl-assert (eq (null a) (null b)))
  (when a
    (cons (cons (car a) (car b))
          (zip (cdr a) (cdr b)))))

(defun csv-parse (filename)
  (with-current-buffer (find-file-noselect filename)
    (let* ((all (split-string (buffer-string) "\n" t))
           (header (->> all car downcase
                        (replace-regexp-in-string
                         ", " ",")
                        (replace-regexp-in-string
                         "[^a-z,]" "-")
                        (s-split  ",")
                        (mapcar #'intern)))
           (rows (mapcar (apply-partially #'zip header)
                         (mapcar (lambda (row)
                                   (->>
                                       row
                                     (replace-regexp-in-string
                                      "\".*?\""
                                      (apply-partially #'replace-regexp-in-string "[\",]" ""))
                                     (s-split ",")))
                                 (cdr all)))))
      rows)))

(defun money-agg (data categories)
  (cl-loop with agg = (mapcar (lambda (category) (cons (car category) 0))
                              categories)
           for row in data
           as amount = (-> (alist-get 'amount row) (string-to-number) (* -1))
           as desc = (or (alist-get 'original-description row)
                         (alist-get 'description row))
           as category =
           (cl-loop for (category . regexps) in categories
                    thereis (and (some
                                  (lambda (regexp)
                                    (string-match regexp desc))
                                  regexps)
                                 category))
           unless category collect (cons desc amount) into unclassified
           when category do
           (incf (alist-get category agg) amount)
           finally (progn
                     (let ((buffer "*money-unclassified*"))
                       (with-current-buffer (get-buffer-create buffer)
                         (erase-buffer)
                         (->>
                             (mapcar (lambda (desc-amnt)
                                       (cl-destructuring-bind (desc . amnt) desc-amnt
                                         (format "%d\t%s" amnt desc)))
                                     (sort-by unclassified #'car :pred #'string<))
                           (s-join "\n")
                           insert))
                     (switch-to-buffer buffer))
                     (return agg))))

(defun money-agg-from-csv (categories csv-files)
  (let ((data (mapcan #'csv-parse csv-files)))
    (-> (loop for (cat . amt) in (money-agg data categories)
              collect (cons cat (round amt)))
      (sort-by #'cdr :descending t))))
