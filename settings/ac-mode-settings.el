

(setf ac-number-candidates-p t)
(setf ac-complete-select-nth-kdb-fun
      (lambda (i) (kbd (format "s-%d" i))))


;;(setf ac-delay .005)
(setf ac-delay 0)
(setf ac-auto-show-menu 0)

;;start when first character typed, don't wait until 2nd
(setf ac-auto-start 1)


(require 'auto-complete)


(require 'auto-complete-config)
(ac-config-default)

;;command-mode and goto-last-change
(define-key ac-completing-map [f1] nil )
(define-key ac-completing-map [M-f1] nil )
