(defun insert-unique-line ()
  (interactive)
  (let* ((initial (concat "# " (uuid) "-"))
         (line (read-string "enter unique line: " initial))
         (final (format "insert-text-block '%s' " line)))
    (insert final)))

(defun bash-update-getopt-optstring (a b)
  (interactive "r")
  "update bash getopt optstring in a region containing a getopts loop
as in:

    while getopts \"ha:\" OPT; do
        case ${OPT} in
        k)
            KEY=${OPTARG}
            ;;
        v)
          VALUE=${OPTARG}
          ;;
        c)
          COMMENT_START=${OPTARG}
          ;;
        h)
            less $0
            exit 0
            ;;
        esac
    done

to reflect opt-chars actually declared in the loop body"

  (save-excursion
    (unless (region-active-p)
      (setf a (point-min)
            b (point-max)))
    (let* ((optstring-re "while getopts \"\\([^\"]+\\)\"")
           (optstring
            (progn
              (goto-char a)
              (re-search-forward optstring-re b)
              (match-string 1)))
           (missing "")
           (opt-actual-chars
            (loop initially (goto-char a)
                  while (re-search-forward "^[     ]+\\([a-z]\\))$" b t)
                  as opt-char = (match-string 1)
                  concat opt-char
                  unless (s-contains-p opt-char optstring)
                  do (setf missing (concat missing opt-char))))
           (optstring-sans-extra
            (loop with start = 0
                  as start = (string-match "[a-z]:?" optstring start)
                  while start
                  as opt-char = (char-to-string (aref optstring start))
                  when (s-contains-p opt-char opt-actual-chars)
                  concat (match-string 0 optstring)
                  do (setf start (match-end 0)))))

      (goto-char a)
      (re-search-forward optstring-re b)
      (replace-match (concat optstring-sans-extra missing)
                     nil nil nil 1))))
