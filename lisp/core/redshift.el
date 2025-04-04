(defun redshift-period ()
  (let* ((default-directory (expand-file-name "~"))
         (period (s-trim (shell-command-to-string "redshift-period.sh"))))
    (if (s-match "Daytime\\|Transition\\|Night" period)
        (intern (downcase period))
      (error "could not determine redshift period: %s" period))))

(defvar light-mode-theme 'aalto-light)
(setf dark-mode-theme '(renegade wombat))
(defvar redshift-last-manual-dark-mode-override nil)


(defvar redshift-dark-mode-enabled nil)

(defun redshift-dark-mode-enabled-p ()
  redshift-dark-mode-enabled)

(defun redshift-load-dark-theme ()
  (unless (redshift-dark-mode-enabled-p)
    (message "loading dark theme")
    (disable-theme light-mode-theme)
    (cl-loop for theme in dark-mode-theme
             do (load-theme theme t))
    (setf *erjoalgo-command-mode-color-on* "light green"
	  *erjoalgo-command-mode-color-off* "light gray")
    (set-cursor-color "#ffffff")
    (setq redshift-dark-mode-enabled t)))

(defun redshift-unload-dark-theme ()
  (when (redshift-dark-mode-enabled-p)
    (message "unloading dark theme")
    (cl-loop for theme in dark-mode-theme
             do (message "unloading %s" theme)
             do (disable-theme dark-mode-theme))
    (load-theme light-mode-theme t)
    (setf *erjoalgo-command-mode-color-on* "dark green"
	  *erjoalgo-command-mode-color-off* "dark gray")
    (setq redshift-last-manual-dark-mode-override (float-time))
    (set-cursor-color "#000000")
    (setq redshift-dark-mode-enabled nil)))

(defun redshift-dark-theme-toggle ()
  "Toggle dark background theme."
  (interactive)
  (if (redshift-dark-mode-enabled-p)
      (redshift-unload-dark-theme)
    (redshift-load-dark-theme)))


(defun redshift-adjust-theme-from-period ()
  (let ((period (redshift-period)))
    (if (eq period 'day)
        (redshift-unload-dark-theme)
      (redshift-load-dark-theme))))

(setq redshift-load-dark-theme-disable t)

(defun redshift-maybe-adjust-theme-from-period ()
  (let* ((day-seconds (* 24 60 60))
         (deadline
          (when
              redshift-last-manual-dark-mode-override
            (time-add redshift-last-manual-dark-mode-override day-seconds)))
         (now nil))
    (when
        (or
         (null
          redshift-last-manual-dark-mode-override)
         (time-less-p deadline now))
      (redshift-adjust-theme-from-period))))

(defvar redshift-adjust-theme-timer nil)

(defun redshift-adjust-theme-timer-start ()
  (when redshift-adjust-theme-timer
    (cancel-timer redshift-adjust-theme-timer))
  (setq redshift-adjust-theme-timer
        (run-at-time nil 60 #'redshift-maybe-adjust-theme-from-period)))

(redshift-adjust-theme-timer-start)
