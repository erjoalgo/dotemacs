(defun ensure-packages-exist (&rest packages )
  (let (refreshed-p)
  (dolist (package packages)
    (when
	(and (not (package-installed-p package))
	(loop for i below 2 always
	      (y-or-n-p (format "connect to the internet to install %s? (%d)"
				package i))))
      (or refreshed-p (progn
			(package-refresh-contents)
			(setf refreshed-p t)))
      (package-install package)))))

(ensure-packages-exist
 'clojure-mode 'cider)

(setf cider-repl-display-help-banner nil )
(setf cider-show-error-buffer nil)
;;https://github.com/bbatsov/projectile

