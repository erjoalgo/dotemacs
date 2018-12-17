;;; proxy-mode.el --- Toggle a proxy on/off

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

(require 'url-vars)

(defvar proxy-mode-proxy-url
  nil
  "The proxy URL to use when proxy is turned on.")

(defun proxy-on (off)
  "Turn proxy on or OFF."
  (interactive (list current-prefix-arg))
  (let ((value (if off "" proxy-mode-proxy-url))
	(envs '("http_proxy" "http_proxy" "HTTP_PROXY" "HTTPS_PROXY")))
    (message "new proxy value is: '%s'" value)
    (dolist (env envs)
      (setenv env value))
    (let ((value-no-scheme
	   (replace-regexp-in-string value "https?://" "")))
      (setq url-proxy-services
	    `(("no_proxy" . "^\\(localhost\\|10.*\\)")
	      ("http" . ,value-no-scheme)
	      ("https" . ,value-no-scheme))))))

(defun proxy-off ()
  "Turn off proxy."
  (interactive)
  (proxy-on t))

;;;###autoload
(defun proxy-toggle ()
  "Toggle proxy on/off."
  (interactive)
  (let ((currently-off (zerop (length (assoc "http" url-proxy-services #'equal)))))
    (proxy-on (not currently-off))))


(defalias 'sp 'proxy-toggle)


(provide 'proxy-mode)
;;; proxy-mode.el ends here
