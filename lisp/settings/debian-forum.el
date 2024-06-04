(defun debian-forum-post-format ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "```\\(\\(.*\\|\n\\)+\\)```" nil t)
      (replace-match (format "[code]\n%s\n[/code]" (match-string 1)))))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\\(^    .*\n\\)+\\)" nil t)
      (let* ((code-with-spaces (match-string 1))
             (code (save-match-data
                     (string-join
                      (mapcar #'s-trim (split-string code-with-spaces "\n"))
                      "\n"))))
        (replace-match (format "[code]\n%s\n[/code]\n" code))))))


