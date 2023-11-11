#!/usr/bin/env -S emacs --script

(require 'cl-lib)
(require 'subr-x)

(defun sh-getopts-check (&optional auto-fix)
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "while getopts \"\\(.*?\\)\"" nil t)
      (let* ((spec-end (match-end 0))
             (end (save-excursion
                    (save-match-data
                      (goto-char (match-beginning 0))
                      (forward-sexp)
                      (point))))
             (spec (split-string (match-string 1) "" t))
             (missing nil)
             (actual nil))
        (cl-loop
         while (re-search-forward "^[[:space:]]+\\(.\\))[[:space:]]*$" end t)
         as opt = (match-string 1)
         do (progn
              (push opt actual)
              (unless (member opt spec)
                (push opt missing))))
        (when missing
          (when auto-fix
            (goto-char (1- spec-end))
            (insert (string-join missing ""))
            (save-buffer))
          (error "getopts: missing %s from spec" missing))))))

(defun sh-getopts-check-main ()
  (cl-loop for filename in command-line-args-left
           with errors = nil
           do (condition-case ex
                  (with-current-buffer
                      (let ((sh-make-vars-local nil)
                            (inhibit-message t))
                        (find-file-noselect filename t))
                    (sh-getopts-check t))
                (error (push (format "%s: %s" filename ex) errors)))
           finally
           (progn
             (when errors
               (message "%s" (string-join errors "\n"))
               (kill-emacs (length errors))))))

(defun sh-getopts-check-hook ()
  (when (eq major-mode 'sh-mode)
    (sh-getopts-check)))

(if command-line-args-left
    (sh-getopts-check-main)
  (add-hook 'write-file-functions 'sh-getopts-check-hook))
