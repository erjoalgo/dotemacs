(require 'auto-complete)

(require 'auto-complete-config)

(ac-config-default)

(loop for i from 1 upto 9 
      as key = (kbd (format "s-%s" i))
      as sym = (intern (format "ac-complete-select-%d" i)) do
      (fset sym `(lambda () (interactive)
		   (if (ac-menu-live-p)
		       (if (popup-select ac-menu ,(1- i))
			   (ac-complete)
			 (error "something failed in goto completion"))
		     (error "ac menu not live"))))
      do
      (define-key ac-completing-map key sym))


;;(setf ac-delay .005)
(setf ac-delay 0)
(setf ac-auto-show-menu 0)

(setf ac-auto-start 1)


(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
