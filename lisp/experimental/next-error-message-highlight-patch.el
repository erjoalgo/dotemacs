(defcustom next-error-message-highlight-p nil
  "If non-nil, highlight the current error message in the ‘next-error’ buffer"
  :type 'boolean
  :group 'next-error
  :version "27.1")

(defface next-error-message
  '((t (:inherit highlight)))
  "Face used to highlight the current error message in the ‘next-error’ buffer"
  :group 'next-error
  :version "27.1")

(defvar next-error-message-highlight-overlay
  nil
  "Overlay highlighting the current error message in the ‘next-error’ buffer")
(make-variable-buffer-local 'next-error-message-highlight-overlay)

(defun next-error-message-highlight ()
  "Highlight the current error message in the ‘next-error’ buffer."
  (when next-error-message-highlight-p
    (with-current-buffer next-error-last-buffer
      (make-variable-buffer-local 'hl-line-range-function)
      ;; (message "setting hl-line-range-function")
      (setf hl-line-range-function
            (lambda ()
              (save-excursion
                (goto-char compilation-current-error)
                (let ((range
                       (cons (line-beginning-position) (line-end-position))))
                  (message "hl-line-range-function caled. range is %s" range)
                  range)))))))

(add-hook 'next-error-hook 'next-error-message-highlight)
