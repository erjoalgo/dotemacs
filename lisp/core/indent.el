(require 'slime-cl-indent)
(setq-default indent-tabs-mode nil)

(defun indent-file-and-save (&optional filename)
  "Indent and save a file. Used for indenting with emacs in batch mode."
  (save-excursion
    (when filename (find-file-noselect filename))
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))
    (save-buffer)))
