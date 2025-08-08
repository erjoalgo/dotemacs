(defun redshift-period ()
  (let* ((default-directory (expand-file-name "~"))
         (period (s-trim (shell-command-to-string "redshift-period.sh"))))
    (if (s-match "Daytime\\|Transition\\|Night" period)
        (intern (downcase period))
      (error "could not determine redshift period: %s" period))))

(defun first-available-theme-group (groups)
  (cl-loop with available-themes = (custom-available-themes)
           for group in groups
           thereis
           (when (seq-every-p
                  (lambda (theme) (member theme available-themes))
                  group)
             group)))

(defun dark-mode-theme ()
  (first-available-theme-group
   '((renegade wombat)
     (wombat))))

(defun light-mode-theme  ()
  (first-available-theme-group
   '((aalto-light)
     (adwaita))))

(defvar redshift-last-manual-dark-mode-override nil)


(defvar redshift-dark-mode-enabled nil)

(defun redshift-dark-mode-enabled-p ()
  redshift-dark-mode-enabled)

(defun redshift-load-dark-theme ()
  (unless (redshift-dark-mode-enabled-p)
    (message "loading dark theme")
    (unload-theme-group (light-mode-theme))
    (load-theme-group (dark-mode-theme))
    (setf *erjoalgo-command-mode-color-on* "light green"
	  *erjoalgo-command-mode-color-off* "light gray")
    (set-cursor-color "#000000")
    (setq redshift-dark-mode-enabled t)))

(defun load-theme-group (group)
  (cl-loop for theme in group
           do (message "unloading %s" theme)
           do (disable-theme theme)))

(defun unload-theme-group (group)
  (cl-loop for theme in group
           do (load-theme theme t)))

(defun redshift-unload-dark-theme ()
  (when (redshift-dark-mode-enabled-p)
    (message "unloading dark theme")
    (unload-theme-group (dark-mode-theme))
    (load-theme-group (light-mode-theme))
    (setf *erjoalgo-command-mode-color-on* "dark green"
	  *erjoalgo-command-mode-color-off* "dark gray")
    (setq redshift-last-manual-dark-mode-override (float-time))
    (set-cursor-color "#ffffff")
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
