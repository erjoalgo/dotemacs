(let ((prompt "$ "))
  (setf eshell-prompt-function `(lambda () ,prompt))
  (setf eshell-prompt-regexp (concat "^" (regexp-quote prompt))))
