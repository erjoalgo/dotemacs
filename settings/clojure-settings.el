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

(defun cider-buffer-or-jack-in ()
  (interactive)
  (let ((ciders (remove-if-not (lambda (buf)
			  (s-starts-with? "*cider-repl" (buffer-name buf)))
			(buffer-list))))
    (if ciders (switch-to-buffer (car ciders))
      (cider-jack-in))))
