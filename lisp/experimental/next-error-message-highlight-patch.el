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
      (when next-error-message-highlight-overlay
        (delete-overlay next-error-message-highlight-overlay))
      (save-excursion
        (goto-char (point))
        (let ((ol (make-overlay (line-beginning-position) (line-end-position))))
          ;; do not override region highlighting
          (overlay-put ol 'priority -50)
          (overlay-put ol 'face 'next-error-message)
          (overlay-put ol 'window (get-buffer-window))
          (setf next-error-message-highlight-overlay ol))))))

(add-hook 'next-error-hook 'next-error-message-highlight)
;; (add-hook 'slime-next- 'next-error-message-highlight)
