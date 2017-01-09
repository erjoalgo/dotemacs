;;; proxy-mode.el ---

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun proxy-on ()
  (interactive)
  (proxy-internal t))

(defun proxy-off ()
  (interactive)
  (proxy-internal nil))

(defun proxy-internal (on)
  (let ((value (if on proxy-mode-proxy ""))
	(envs '("http_proxy" "http_proxy" "HTTP_PROXY" "HTTPS_PROXY")))
    (message "value is: '%s'" value)
    (dolist (env envs)
      (setenv env value))
    (let ((value
	   (gnus-replace-in-string value "https?://" "")))
      (setq url-proxy-services
	    `(("no_proxy" . "^\\(localhost\\|10.*\\)")
	      ("http" . ,value)
	      ("https" . ,value))))))

(defun proxy-toggle ()
  (interactive)
  (proxy-internal (equal "" (getenv "http_proxy"))))

(defalias 'sp 'proxy-toggle)
(defcustom proxy-mode-proxy "" "proxy for proxy mode")


(provide 'proxy-mode)
;;; proxy-mode.el ends here
