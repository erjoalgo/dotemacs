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
      python-indent-offset (if (bound-and-true-p google-emacs-version) 2 4)
      indent-tabs-mode nil
      py-smart-indentation nil))

(add-hook 'python-mode-hook 'my-python-indentation)

(defvar python-argparse-template
  "import argparse
parser = argparse.ArgumentParser()
parser.add_argument(\"line\", help=\"universally unique line\")
parser.add_argument(\"filename\", help=\"input/output file\")
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

(with-eval-after-load "flycheck"
  (flycheck-define-checker pylint
    "Flycheck for pylint."
    :command ("pylint" source)
    :error-patterns
    ((error line-start alpha ": " line ", " column
            ":" (message) line-end))
    :modes python-mode))

(defun python-autodetect-indent-level ()
  (let ((level (or (detect-indent-level) 4)))
    (setq tab-width level)
    (setq python-indent-offset level)))

(add-hook 'python-mode-hook 'python-autodetect-indent-level)

(defun python-space-imports ()
  "Add spaces around multiline import ... from statements."
  (interactive)
  (when (eq major-mode 'python-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^from .*( *\n[^)]+)" nil t)
        (save-excursion
          (let ((beginning (match-beginning 0))
                (end (match-end 0)))
            (goto-char end) ;; modify end first to avoid corrupting match end
            (unless (looking-at "\n\n")
              (open-line 1))
            (goto-char beginning)
            (unless (looking-back "\n\n" nil)
              (open-line 1))))))))


(defun python-sort-imports ()
  "Sort python imports."
  (interactive)
  (when (eq major-mode 'python-mode)
    (save-excursion
      (python-space-imports)
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\(import \\|from \\).*\n\\)+" nil t)
        (sort-lines nil (match-beginning 0)
                    (match-end 0))))))

(add-hook 'before-save-hook 'python-sort-imports)

(add-hook 'python-mode-hook
          (lambda () (setq forward-sexp-function nil)))


(defun python-shell-completion-native-get-completions--sort (orig-fn &rest args)
  (let ((ret (apply orig-fn args)))
    (when ret
      ;; (edebug)
      (sort-key ret (apply-partially #'cl-count (string-to-char "_"))))))

(advice-add #'python-shell-completion-native-get-completions
            :around
            #'python-shell-completion-native-get-completions--sort)

(require 'subr-x)

(defun pylint-current-warning-code ()
  "Return the last-echoed pylint warning code."
  (save-excursion
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (cl-loop for i below 30
               as line = (progn
                           (forward-line -1)
                           (buffer-substring (line-beginning-position)
                                             (line-end-position)))
               thereis (and
                        (string-match
                         ;; Unused variable 'borg_service' [unused-variable]
                          ".*[[]\\([a-z-]+\\)]\\(  [[0-9]+ times]\\)?"
                          line)
                         (match-string 1 line))))))

(defun python-dir-on-expression (&optional a b help-p)
  "Surround python expression (A, B) with dir(...)."
  (interactive "r\np")
  (setq a (or a (line-beginning-position))
        b (or b (line-end-position)))
  (save-excursion
    (let ((text (buffer-substring a b)))
      (delete-region a b)
      (goto-char a)
      (insert (if help-p "help(" "dir("))
      (insert text)
      (insert ")")))
  (comint-send-input)
  (comint-send-input))

(defun python-help-on-expression (&optional a b)
  (python-dir-on-expression a b t))

1

(defun python-set-custom-check-command ()
  (interactive)
  (when (buffer-file-name nil)
    (setf
     python-check-custom-command
     (let ((basename (-> (buffer-file-name nil) f-filename)))
       (format
        "pylint %s | sed 's/^[A-Z]: *\\([0-9]*\\),/%s:\\1: /g'"
        basename basename)))))

(setq python-shell-interpreter "python3")

(defun python-auto-def-init ()
  (interactive)
  (when (save-excursion
          (re-search-backward "def __init__(\\(.*\\))"))
    (cl-loop with match = (match-string 1)
             with param-regexp = " *\\([^: =]+\\)"
             for param-text in (s-split "," match)
             as param = (or (cl-second (s-match param-regexp param-text))
                            (error "could not parse param text: %s"
                                   param-text))
             unless (equal "self" param)
             do (progn
                  (insert (format "self.%s = %s" param param))
                  (newline-and-indent)))))
