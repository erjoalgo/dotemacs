;;; project-opener.el --- Quickly resume work on an existing project

;; Copyright (C) 2018  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: lisp
;; Created: 07 Oct 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'f)
(require 'cl)
(defun add-one-time-hook (hook-var fn)
  "Add to HOOK-VAR a function which calls FN and then removes itself from HOOK-VAR."
  (let ((fun-sym (gensym "anon-hook-fn")))
    ;; (lexical-let ((fn fn) (fun-sym fun-sym))
    (fset fun-sym
          `(lambda (&rest args)
             "autogenerated, self-destroying hook"
             (apply 'safe-funcall ,fn args)
             (remove-hook ',hook-var ',fun-sym)))
    ;; some functions, like remove hook, need the symbol-value
    (set fun-sym (symbol-function fun-sym))
    (add-hook hook-var fun-sym)))

(defun add-one-time-log-hooks (hook-syms)
  "Use add-one-time-hooks to log a one-time message after each hook in HOOK-SYMS runs."
  (interactive
   (list (symbols-matching-regexp
          (read-string "enter hook symbol regexp: " "-hook"))))
  (cl-loop for hook in hook-syms do
        (add-one-time-hook
         hook
         `(lambda (&rest args)
            (message "%s invoked with args %s on directory %s"
                     ',hook args default-directory)))))

(defun symbols-matching-regexp (regexp)
  "List of symbols matching REGEXP."
  (save-match-data
    (let (syms)
      (mapatoms (lambda (atom) (when (and (symbolp atom)
                                          (string-match regexp (symbol-name atom)))
                                 (push atom syms))))
      syms)))
;; '(add-one-time-hook ' post-command-hook
;;                             (lambda () (message "one-time-hook ran")))

(defun find-files-recursively (top &optional pred action)
  "Recursively find files in directory TOP.  Filter by PRED and invoke ACTION on each."
  (assert (f-absolute? top))
  ;; (lexical-let ((default-directory (f-expand top)))
  (setf pred (or pred 'identity)
        action (or action 'find-file-noselect))

  (dolist (file (directory-files top))
    (let ((abs (f-join top file)))
      (cond
       ((f-file? abs) (when (funcall pred abs)
                        (funcall action abs)))
       ((f-directory? abs) (unless (member file '(".." "." ".git"))
                             (find-files-recursively abs pred)))
       ((f-symlink? abs) (warn "broken symlink: %s" abs))
       (t (require 'edebug) (edebug)
          (error "Not a file or directory? %s" abs))))))

(defun project-open-default-matcher (top-level-directory &rest ...)
  "Matcher invoked when no other matcher matched.

   Opens all project files recursively under TOP-LEVEL-DIRECTORY."
  (find-files-recursively top-level-directory))

(defvar project-open-matchers-list nil
  "List of project matchers.

   Each matcher is invoked with the project's top-level-directory
   and a list of top-level files/directories.")

(defun file-extension-matches-p (ext file)
  "Return t if FILE's extension is EXT."
  (equal (f-ext file) ext))

(defun project-open-cl-post-slime-connected (original-directory
                                             &optional
                                             top-level-files
                                             asd-filename)
  "Eval starting expressions in a slime invocation started to open a CL project rooted at ORIGINAL-DIRECTORY."
  (setf top-level-files (or top-level-files
                            (directory-files original-directory))
        asd-filename (or asd-filename
                         (->>
                          top-level-files
                          (remove-if-not
                           (apply-partially 'file-extension-matches-p "asd"))
                          car)))

  (assert asd-filename)

  (message "on project-open-cl-post-slime-connected")

  (cl-loop for form in '((current-buffer)
                      major-mode
                      default-directory
                      original-directory
                      asd-filename)
        do (message "\t%s" (cons form (eval form))))

  (cl-labels
      ((read-top-level-sexps (string) (read (format "(%s)" string)))
       (read-file-defs-matching
        (filename def-syms)
        (with-temporary-current-file
         filename
         (cl-loop for sexp in (read-top-level-sexps (buffer-string))
               when (and (consp sexp) (member (car sexp) def-syms))
               collect (cadr sexp)))))

    (let* ((default-directory original-directory)
           (packages-filename "packages.lisp")
           (load-candidate-filenames
            '("repl-load.lisp"
              "repl-start.lisp"))
           (load-file-names (-intersection
                             load-candidate-filenames
                             top-level-files))
           (systems
            (read-file-defs-matching asd-filename '(defsystem asdf:defsystem)))
           (packages
            (when (file-exists-p packages-filename)
              (read-file-defs-matching packages-filename '(defpackage asdf:defpackage)))))

      (message "pacakges %s" packages)
      (message "systems %s" systems)
      (message "dir %s" default-directory)

      (slime-change-directory original-directory)
      (slime-cd original-directory)
      (let ((sexp `(CL:progn
                    (CL:load ,asd-filename)
                    ,@(cl-loop for system in systems collect
                            `(ql:quickload ',system)))))
        (message "sexp: %s" sexp)
        ;; TODO wrap in handler-case
        (slime-eval sexp))

      (when packages
        (slime-repl-set-package
         (upcase (symbol-name (car packages)))))

      (dolist (filename load-file-names)
        (when (file-exists-p filename)
          ;; (message "loading %s %s" filename `(CL:LOAD ,filename))
          (slime-eval `(CL:LOAD ,filename)))))))

(defun project-open-cl (top-level-directory &optional top-level-files)
  "project-opener matcher for a CL project rooted at TOP-LEVEL-DIRECTORY.

   Start slime, load the .asd file,
   quickload all the systems defined in it.
   Also load a file 'repl-startup.lisp' if it exists."

  (cl-loop for file in top-level-files thereis
        (when (file-extension-matches-p "asd" file)
          (find-files-recursively top-level-directory
                                  (lambda (filename)
                                    (not (file-extension-matches-p
                                          "fasl" filename))))
          (let ((default-directory top-level-directory))
            (save-excursion
              (goto-char (point-min))
              (add-one-time-hook
               ' slime-editing-mode-hook
               `(lambda (&rest args)
                  (project-open-cl-post-slime-connected
                   ,default-directory
                   ',top-level-files
                   ,file)))
              (slime))
            t))))
(add-to-list 'project-open-matchers-list 'project-open-cl)

(defun project-open (top-level-directory)
  "Open a project rooted at TOP-LEVEL-DIRECTORY."
  (interactive (list (read-directory-name "enter project directory: "
                                          "~/git/")))
  (cl-loop for matcher in (append project-open-matchers-list
                               (list 'project-open-default-matcher))
        with top-level-filenames = (directory-files top-level-directory)
        do (message "trying matcher %s" matcher)
        thereis (and
                 (funcall matcher top-level-directory
                          top-level-filenames)
                 (message "matched %s" matcher)
                 t)))

(provide 'project-opener)
;;; project-opener.el ends here
