(defun stumpwm-message (text)
  (if (> 0 slime-connection-counter)
      (stumpwm-eval (message ",text"))
    (error "not connected to stumpwm")))


(defun erjoalgo-compile-post-compile-message ()
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (stumpwm-message "compilation ended"))

(add-hook 'erjoalgo-compile-post-compile-hook
          'erjoalgo-compile-post-compile-message)
