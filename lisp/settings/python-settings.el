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

(flycheck-define-checker pylint
  "flycheck for Pylint"
  :command ("pylint" source)
  :error-patterns
  ((error line-start alpha ":" line ", " column ":" (message) line-end))
  :modes python-mode)

(defvar python-argparse-template
  "import argparse
parser = argparse.ArgumentParser()
parser.add_argument(\"line\", help = \"universally unique line\")
parser.add_argument(\"filename\", help = \"input/output file\")
parser.add_argument(\"-b\", \"--begining_append\",  action=\"store_true\",
                    help = \"preppend instead of append\")
parser.add_argument(\"-o\", \"--output\",
                    help = \"specify a different output file\")
parser.add_argument(\"-n\", \"--no_strip_newline\",  action=\"store_true\",
                    help = \"don't strip trailing newline from stdin\")

args=vars(parser.parse_args())
globals().update(args)
")


(defun python-check-read-check-command ()
  (read-string "Check command: "
               (or python-check-custom-command
                   (concat python-check-command " "
                           (shell-quote-argument
                            (or
                             (let ((name (buffer-file-name)))
                               (and name
                                    (file-name-nondirectory name)))
                             ""))))))

(defun python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default."
  (interactive
   (list (or (unless current-prefix-arg python-check-custom-command)
             (python-check-read-check-command))))
  (setq python-check-custom-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (python-shell-with-environment
    (compilation-start command nil
                       (lambda (_modename)
                         (format python-check-buffer-name command)))))
