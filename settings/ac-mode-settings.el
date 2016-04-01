(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

'(loop for i from 1 upto 9 
      as key = (kbd (int-to-string i))
      as fun = `(lambda () (interactive)
		 (if (or t (ac-menu-live-p))
		     (if (popup-select ac-menu ,(1- i))
			 (ac-complete)
		       (error "something failed in goto completion"))
		   (error "ac menu not live")))
      do
      (define-key ac-completing-map key fun))

(setf ac-delay .005)

(setf ac-auto-show-menu .1)

(dotimes (i 9)
  (let ((symbol (intern (format "ac-complete-select-%d" (1+ i)))))
    (fset symbol
	  `(lambda ()
	     (interactive)
	     (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
	       (ac-complete))))
    (define-key ac-completing-map (read-kbd-macro (format "s-%s" (1+ i))) symbol)))




(setf ac-auto-start 1)
