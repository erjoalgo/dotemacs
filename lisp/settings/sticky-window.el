(defadvice sticky-window-keep-window-visible (after message-sticky-window-activation activate)
  (message "sticky window enabled"))
