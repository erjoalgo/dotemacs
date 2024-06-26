(defun leetcode-find-solution-name ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (cl-assert (re-search-forward "^public:\n +.*? +\\([^ (]+\\)("))
      (match-string 1))))

(defun leetcode-insert-test-case (text)
  (interactive "senter leetcode testcase text: ")
  (let ((fn-name (leetcode-find-solution-name)))
    (buttons-macrolet nil
      (indent-for-tab-command)
      (ins "{") (nli)
      (ins "auto res = sol." fn-name "(")
      (leetcode-test-case-to-c++ text)
      (ins ");") (nli)
      (ins "cout << \"result: \" << res << endl;") (nli)
      (ins "for (const auto resi : res) {") (nli)
      (ins "cout << \"result: \" << resi << endl;") (nli)
      (ins "}")  (indent-for-tab-command) (nli)
      (ins "}") (indent-for-tab-command) (nli))))

(defun leetcode-insert-c++-main ()
  (interactive)
  (cpp-maybe-insert-includes)
  (goto-char (point-max))
  (buttons-macrolet nil
    (nli)
    (ins "int main (int argc, char* argv[]) {") (nli)
    (ins "Solution sol;") (nli)
    (leetcode-insert-test-case (read-string "enter test case: " (x-get-selection)))
    (ins "}")
    (indent-for-tab-command)))

(defun leetcode-test-case-to-c++ (text)
  (interactive (list (x-get-selection)))
  (setq text (string-trim-left text "Input: "))
  (setq text (string-trim-right text "\n"))
  (cl-loop
   with defs
   with args
   for var in (split-string text ", \\|\n")
   for i from 1
   as kv = (split-string var " = ")
   do (cl-destructuring-bind (k v)
          (if (cdr kv) kv (list (format "var%s" i) (car kv)))
        (if (not (s-contains-p "]" v))
            (push v args)
          (when (> (length v) 10000)
            (setq v (s-replace-all '(("," . ",\n")) v)))
          (let* ((body (s-replace "]" "}"
                                 (s-replace "[" "{" v)))
                (dim (save-match-data
                       (string-match "^{+" body)
                       (length (match-string 0 body))))
                (base-type-guess (if (save-match-data (string-match "\"" body))
                                       "string"
                                       "int"))
                (vector-type
                 (apply #'concat
                        (append
                         (cl-loop for _ below dim
                                  collect "vector<")
                         (list base-type-guess (make-string dim (string-to-char ">")))))))
            (push (format "%s %s = %s;" vector-type k body) defs)
            (push k args))))
   finally
   (progn
     (save-excursion
       (beginning-of-line)
       (cl-loop for def in (seq-reverse defs)
                do (indent-for-tab-command)
                do (insert def)
                do (newline-and-indent)))
     (insert (string-join (seq-reverse args) ", ")))))

(defun leetcode-new-solution ()
  (interactive)
  (with-current-buffer (get-buffer-create "*leetcode-scratch*")
    (erase-buffer)
    (insert (read-string "enter solution code: " (x-get-clipboard)))
    (let* ((name (leetcode-find-solution-name))
           (directory (expand-file-name "~/git/leetcode"))
           (filename  (f-join directory (format "%s.cc" (s-dashed-words name)))))
      (make-directory directory t)
      (write-file filename)
      (newline-and-indent)
      (switch-to-buffer (current-buffer))
      (leetcode-insert-c++-main))))


(defun cpp-maybe-insert-includes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (save-excursion (re-search-forward "^using namespace std" nil t))
      (buttons-macrolet nil
       (ins "#include <algorithm>") (nli)
       (ins "#include <array>") (nli)
       (ins "#include <assert.h>") (nli)
       (ins "#include <cmath>") (nli)
       (ins "#include <iostream>") (nli)
       (ins "#include <map>") (nli)
       (ins "#include <queue>") (nli)
       (ins "#include <set>") (nli)
       (ins "#include <string>") (nli)
       (ins "#include <unordered_map>") (nli)
       (ins "#include <unordered_set>") (nli)
       (ins "#include <vector>") (nli)
       (nli)
       (ins "using namespace std;") (nli)))))

(defvar c++-debugging-macros nil)
(setf c++-debugging-macros
      (debian-file->string
       (f-join (if load-file-name
                   (file-name-directory load-file-name)
                 default-directory)
               "cpp-macro.cpp")))
