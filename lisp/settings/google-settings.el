(when (require 'google nil t)
  ;; TODO find appropriate modes
  (push `(".pkgdef$" . borg-mode) auto-mode-alist)
  ;; (push `("[.]sdl" . sql-mode) auto-mode-alist)
  ;; (push `("BUILD" . python-mode) auto-mode-alist)

  (require 'google3-build)
  (require 'csearch)
  ;; don't use chromium
  (setf browser-name (car (which "chrome" "google-chrome")))


  (require 'google3-language-services)
  (google3-language-services-setup))
