(defun redshift-period ()
  (let* ((default-directory (expand-file-name "~"))
         (output (shell-command-to-string "redshift -p"))
         (period (progn
                   (cl-assert
                    (string-match "Period: \\(.*\\)" output)
                    t "unrecognized redshift output: %s" output)
                   (match-string 1 output))))
    (if (s-match "Daytime\\|Transition\\|Night" period)
        (intern (downcase period))
      (error "unrecognized redshift period `%s' from output: %s"
             period output))))

(defvar dark-mode-theme 'wombat)
(defvar redshift-last-manual-dark-mode-override nil)


(defun redshift-load-dark-theme ()
  (unless (custom-theme-enabled-p dark-mode-theme)
    (message "loading dark theme")
    (load-theme dark-mode-theme)
    (setf *erjoalgo-command-mode-color-on* "light green"
	  *erjoalgo-command-mode-color-off* "light gray")
    (set-cursor-color "#ffffff")))

(defun redshift-unload-dark-theme ()
  (when (custom-theme-enabled-p dark-mode-theme)
    (message "unloading dark theme")
    (disable-theme dark-mode-theme)
    (setf *erjoalgo-command-mode-color-on* "dark green"
	  *erjoalgo-command-mode-color-off* "dark gray")
    (setq redshift-last-manual-dark-mode-override (float-time))
    (set-cursor-color "#000000")))

(defun redshift-dark-theme-toggle ()
  "Toggle dark background theme."
  (interactive)
  (if (custom-theme-enabled-p dark-mode-theme)
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
