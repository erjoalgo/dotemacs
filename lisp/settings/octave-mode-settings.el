(push '("[.]m$" . octave-mode) auto-mode-alist)


(defvar octave-sync-function-file-names-ignored-files nil
  "Files ignored by octave-sync-function-file-names")

(defun octave-sync-function-file-names ()
  "Ensure function name agree with function file name.
See Info node `(octave)Function Files'."
  (interactive)
  (when buffer-file-name
    (pcase-let ((`(,start ,_end ,name-start ,name-end)
                 (octave-function-file-p)))
      (when (and start name-start)
        (let* ((func (buffer-substring name-start name-end))
               (file (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name)))
               (help-form (format-message "\
a: Use function name `%s'
b: Use file name `%s'
q: Don't fix
i: Don't fix and don't ask again\n" func file))
               (c (unless
                      (or (equal file func)
                          (member file
                                  octave-sync-function-file-names-ignored-files))
                    (save-window-excursion
                      (help-form-show)
                      (read-char-choice
                       "Which name to use? (a/b/q/i) " '(?a ?b ?q ?i))))))
          (pcase c
            (`?a (let ((newname (expand-file-name
                                 (concat func (file-name-extension
                                               buffer-file-name t)))))
                   (when (or (not (file-exists-p newname))
                             (yes-or-no-p
                              (format "Target file %s exists; proceed? " newname)))
                     (when (file-exists-p buffer-file-name)
                       (rename-file buffer-file-name newname t))
                     (set-visited-file-name newname))))
            (`?b (save-excursion
                   (goto-char name-start)
                   (delete-region name-start name-end)
                   (insert file)))
            (`?i (push file octave-sync-function-file-names-ignored-files))))))))
