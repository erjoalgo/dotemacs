;;(add-to-list 'load-path (concat external_libraries_dir "emacs-calfw"))


;;google integration
;;from here: https://github.com/myuhe/calfw-gcal.el
(require 'calfw)
(require 'calfw-ical)
(require 'calfw-gcal)


(assert (every 'boundp '(*cfw-calendar-url*
			 cfw:gcal-pass
			 cfw:gcal-user)))


(defun open-google-calendar ()
  (interactive)
  (cfw:open-ical-calendar *cfw-calendar-url*))

(mapc (lambda (ab) (define-key cfw:calendar-mode-map (cdr ab) (car ab)))
	'(
	 (cfw:navi-previous-day-command . "a")
	 (cfw:navi-next-day-command . "s")
	 (cfw:navi-previous-week-command . "w")
	 (cfw:navi-next-week-command . "d")

	 (cfw:navi-next-month-command . "n")
	 (cfw:navi-previous-month-command . "p")
	 )
	)

(defadvice cfw:navi-next-month-command (before fix-text-scale-month activate)
  ;;(global-text-scale-reset-to .5)
  (global-text-scale-reset-to .4))

