;; (require 'flymake-jslint) ;; Not necessary if using ELPA package

(setf jsl-err-line-pattern
      '("^\\(.*?\\)(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; (push jsl-err-line-pattern flymake-proc-err-line-patterns)
