(defvar erjoalgo-mode-line-format
  '("%e"
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    " "
    mode-line-position
    (:propertize mode-line-misc-info 0 32)
    mode-line-end-spaces
    ""))

(defun mode-line-update-erjoalgo ()
  (put 'org-mode-line-string 'risky-local-variable nil)
  (setf mode-line-format erjoalgo-mode-line-format))

;; setting the value once does not seem to be enough...
(add-hook
 ;; 'change-major-mode-hook
 'after-change-major-mode-hook
 'mode-line-update-erjoalgo)
