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


(defadvice compilation-start (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p 'python-mode)
    (ad-set-arg 1 t)))

(defun my-python-indentation ()
  ;;taken from the internet
  (setq tab-width 4
      python-indent-offset 4
      indent-tabs-mode nil
      py-smart-indentation nil))

(add-hook 'python-mode-hook 'my-python-indentation)
(add-hook 'python-mode-hook 'set-python-custom-check-command)
(defun set-python-custom-check-command ()
  ;; ./socks.go:210: undefined: retur
  (when (buffer-file-name nil)
    (setf python-check-custom-command
	  (let ((basename (-> (buffer-file-name nil) f-filename)))
	    (format "pylint %s | sed 's/^[A-Z]: *\\([0-9]*\\),/%s:\\1: /g'"
		    basename basename)))))

