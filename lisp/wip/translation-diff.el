(defun translation-diff-tuples-normalize (buff)
  (cl-loop for (regexp rep) in
           '(("[`‘’'”“]" "\"")
             (".," "")
             ("[—]" "-"))
           do
           (progn (goto-char (point-min))
                  (replace-regexp regexp rep)))
  (save-buffer))

(defun translation-diff-wdiff-marker (type start-end)
  (concat "<"
          (case type ('insert "insert")  ('delete "delete"))
          (case start-end ('start "") ('end "/"))
          ">"))

(defun find-file-copy-noselect (file directory)
  (let ((dest (f-join directory (f-filename file))))
    (copy-file file dest)
    (find-file-noselect dest)))


(defun translation-diff-wdiff (buff-a buff-b)
  (let* ((out-buff (get-buffer-create "*translation-diff-wdiff*"))
        (ret (call-process "wdiff" nil out-buff nil
                  "--start-insert" (translation-diff-wdiff-marker 'insert 'start)
                  "--end-insert" (translation-diff-wdiff-marker 'insert 'end)
                  "--start-delete" (translation-diff-wdiff-marker 'delete 'start)
                  "--end-delete" (translation-diff-wdiff-marker 'delete 'end)
                  "-n" (buffer-file-name buff-a)
                  (buffer-file-name buff-b))))
    (message "DEBUG iqhc ret: %s" ret)
    out-buff))

(defun translation-diff-tuples (file-a file-b)
  (let* ((directory (-> "mktemp -d test-XXXX -p /tmp"
                      (shell-command-to-string)
                      (s-trim)))
         (buff-a (find-file-copy-noselect file-a directory))
         (buff-b (find-file-copy-noselect file-b directory)))
    (dolist (buff (list buff-a buff-b))
      (with-current-buffer buff
        (translation-diff-tuples-normalize buff)))
    (with-current-buffer (translation-diff-wdiff buff-a buff-b)
      (message "DEBUG 7kzy (current-buffer) (in ): %s"(current-buffer))
      (translation-diff-tuples-from-output))))


(defun translation-diff-tuples-from-output ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "\\(<[^/>]+>\\)\\([^<]+\\)\\(<[^/>]+/>\\)"))
      (cl-loop while (re-search-forward regexp nil t)
               as pos = (match-beginning 0)
               as type = (if (s-contains-p "insert" (match-string 1))
                             'insert 'delete)
               as text = (match-string 2)
               collect (list type pos text)))))


(setq tuples
      (translation-diff-tuples
       "A-original.txt"
       "B-corrección.txt"))
