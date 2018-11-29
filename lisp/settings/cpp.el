(defun cpp-sort-includes ()
  (interactive)
  (when (eq major-mode 'c++-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(^#include.*\n\\)+" nil t)
        (sort-lines nil (match-beginning 0)
                    (match-end 0))))))


(add-hook 'before-save-hook 'cpp-sort-includes)
