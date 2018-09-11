;; TODO find appropriate modes
(push `("[.]sdl" . sql-mode) auto-mode-alist)
(push `("BUILD" . python-mode) auto-mode-alist)

;; don't use chromium

(require 'google)
(require 'google3-build)
(require 'csearch)
(setf browser-name (car (which "chrome" "google-chrome")))


(require 'google3-language-services)
(google3-language-services-setup)
