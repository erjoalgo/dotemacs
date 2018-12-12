(require 'slime-cl-indent)
(setq-default indent-tabs-mode nil)

(defun indent-current-buffer ()
  "Indent and untabify the current buffer.  Return t if file was modified."
  (save-excursion
    ;; (when filename (find-file-noselect filename))
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))
    (buffer-modified-p)))

(defun indent-current-buffer-and-save ()
  "Indent, untabify and save a FILENAME.

    Used for indenting with EMACS in batch mode."
  (when (indent-current-buffer)
    (save-buffer)))

(defun indent-check-indented ()
  "Throw an error if the current buffer is not properly indented."
    (interactive)
  (let ((orig-buff (current-buffer)))
    (with-temp-buffer
      (insert-buffer orig-buff)
      (funcall (buffer-local-value 'major-mode orig-buff))
      (when (indent-current-buffer)
        (error "Buffer needs to be indented: %s" orig-buff)))))
