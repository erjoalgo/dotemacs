
(defun java-set-indentation ()
					;(setf tab-width 8)
  (setf tab-width 4)
  (setf c-basic-offset 4)
					;(setf indent-tabs-mode t)
  (setf indent-tabs-mode nil))

(add-hook 'java-mode-hook 'java-set-indentation)

(defun buffer-indentations ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop as
	  match = (search-forward-regexp "^[ \t]+" nil t nil)
	  while match
	  collect (match-string 0)
	  do (goto-char (match-end 0)))))

(defun buffer-spaces-or-tabs-p ()
  (interactive)
  (let (
	(tab (string-to-char ""))
	(space (string-to-char " "))
	(indentations (buffer-indentations))
	(indentation-type
	 (lambda (whitespace)
	   (if (>= (count tab whitespace)
		   (/ (count space whitespace) 2.0))
	       'TAB 'SPACE))))

    (let ((tabs-vs-spaces (mapcar indentation-type indentations)))
      (if (> (count 'TAB tabs-vs-spaces)
	     (count 'SPACE tabs-vs-spaces))
	  'TABS 'SPACES))))

(defun buffer-indentation-fix-tabs-spaces ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\t" "")
    (indent-region (point-min) (point-max))))

(defun mvn-offline-p-toggle (&optional offline)
  (interactive (list (not (and (boundp 'mvn-offline-p) mvn-offline-p))))
  (setf mvn-offline-p offline)
  (message (if offline "offline" "online")))

(when (functionp 'java-imports-scan-file)
  (add-hook 'java-mode-hook #'java-imports-scan-file))

(defun java-find-class-name ()
  "Find the class name in the current java file."
  (cl-assert (eq major-mode 'java-mode))
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (or
       (cl-loop with class-name-or-comment-regexp =
                "/[*]\\|^\\(\\(public\\|static\\|final\\)[ \t\n]+\\)*\\(class\\|interface\\)[ \t\n]+\\([^ \t\n]+\\)[^{]*{"
                with class-name = nil
                as has-match-p =
                (re-search-forward class-name-or-comment-regexp nil t)
                while (and (null class-name) has-match-p)
                do (if
                       (equal "/*" (match-string 0))
                       ;; skip to the end of the comment. do err if not found,
                       ;; since we expect comment to close
                              (re-search-forward "[*]/" nil nil)
                       (setq class-name (match-string 4)))
                finally (cl-return class-name))
       (warn "no java class name found")))))

(ert-deftest test-java-find-class-name ()
  (cl-labels
      ((test-find-class-name
        (java-code expected-class-name)
        (with-temp-buffer
          (java-mode)
          (insert java-code)
          (should (equal (java-find-class-name) expected-class-name)))))
    (test-find-class-name "public class FooNameService {" "FooNameService")
    (test-find-class-name "/*\npublic class caca*/\npublic class FooNameService {" "FooNameService")
    (test-find-class-name "\npublic class caca\npublic class FooNameService {" "caca")))

(ert-run-tests-batch "test-java-find-class-name")


(defun java-sync-function-file-names ()
  ;; adapted from octave.el: octave-sync-function-file-names
  ;; TODO make generic
  "Ensure function name agree with function file name.
See Info node `(octave)Function Files'."
  (interactive)
  (when buffer-file-name
    (let* ((java-class-name (java-find-class-name)))
      (when java-class-name
        (let* ((func java-class-name)
               (file (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name)))
               (help-form (format "\
a: Use java class name `%s'
b: Use file name `%s'
q: Don't fix\n" func file))
               (c (unless (equal file func)
                    (save-window-excursion
                      (help-form-show)
                      (read-char-choice
                       "Which name to use? (a/b/q) " '(?a ?b ?q))))))
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
		   (goto-char (point-min))
		   (re-search-forward
		    java-public-class-regexp nil)
		   (replace-match file t t nil 2)))))))))

(add-hook 'java-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'java-sync-function-file-names nil t)))


(require 'autobuild)

(autobuild-defvar-file-local mvn-extra-args
  "enter additional mvn args: ")

(defmacro def-region-regexp-cmd (name regexp replacement &optional body)
  (let* ((a-sym (gensym "a"))
         (b-sym (gensym "b"))
         (replace-form
          `(save-excursion
             (goto-char ,a-sym)
             (while (re-search-forward ,regexp ,b-sym t)
               (replace-match ,replacement))))
         (replacement-placeholder-sym 'DO-REPLACEMENT)
         (new-body
          (if (null body)
              replace-form
            (cl-loop for elt in body collect
                  (if (eq elt replacement-placeholder-sym)
                      replace-form elt)))))
  `(defun ,name (,a-sym ,b-sym)
     (interactive "r")
     ,new-body)))

(def-region-regexp-cmd underscore-to-camel-case
  "[-_]\\([a-z]\\)"
  (upcase (match-string 1))

  (progn
    (downcase-region (region-beginning) (region-end))
    DO-REPLACEMENT))

(def-region-regexp-cmd underscore-to-dash
  "_" "-")

(def-region-regexp-cmd dash-to-underscore
  "-" "_")

(defun camel-case-to-underscore (a b &optional use-dash)
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (save-excursion
    (let ((sep (if use-dash "-" "_"))
          (case-fold-search nil))
      (goto-char a)
      (while (re-search-forward "\\([[a-z]\\)\\([A-Z][a-z]\\)" b t)
        (replace-match (concat (match-string 1) sep
                               (downcase (match-string 2))))
        (backward-char); avoid skipping overlap
        (cl-incf b (length sep))))
    (downcase-region a (1+ a))))
