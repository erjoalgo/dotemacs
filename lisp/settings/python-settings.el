;; adapted from https://masteringemacs.org/article/compiling-running-scripts-emacs
;(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
;;(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)
(define-key python-mode-map (kbd "<f5>") nil)


(defadvice compilation-start (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p 'python-mode)
    (ad-set-arg 1 t)))
