;; TODO find appropriate modes
(push `("[.]sdl" . sql-mode) auto-mode-alist)
(push `("BUILD" . python-mode) auto-mode-alist)

;; don't use chromium
(setf browser-name (car (which "chrome" "google-chrome")))
