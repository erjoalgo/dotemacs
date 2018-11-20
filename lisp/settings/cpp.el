(defun cpp-sort-includes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(^#include.*\n\\)+" nil t)
      (sort-lines nil (match-beginning 0)
                  (match-end 0)))))
