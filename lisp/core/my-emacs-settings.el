;;; emacs-settings.el ---

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: local

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
;; some of my emacs settings

;;; Code:


(let ((backups-dir (expand-file-name "~/.emacs.backups"))
      (auto-save-dir (expand-file-name "~/.emacs.auto-save"))
      (tmp-files-dir (expand-file-name "~/.emacs.tmp")))
  (dolist (dir (list backups-dir
		     auto-save-dir
		     tmp-files-dir))
    (unless (file-exists-p dir) (make-directory dir)))
  (setq backup-directory-alist `((".*" . ,backups-dir))
	auto-save-file-name-transforms `((".*" ,backups-dir t))
	auto-save-list-file-prefix (concat backups-dir "/")
	backup-by-copying t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t
	temporary-file-directory tmp-files-dir
	))




;;disable tool-bars, menu-bars
(menu-bar-mode -1)
(tool-bar-mode -1)

(when (>= emacs-major-version 24);;add melpa repo
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(column-number-mode);;enable columns

(setq vc-follow-symlinks t)
(setq read-file-name-completion-ignore-case t);;case-insensitive fn completion
(setq next-error-message-highlight-p t)
(setf next-error-highlight-no-select t)
;;TODO the same for async-shell-command file names

(require 'server)
(progn
  (server-force-delete)
  (server-start))

;(setq split-window-preferred-function 'split-window-vertically) doesn't work
;(setq split-height-threshold 0)

(setq inhibit-startup-screen t)

(show-paren-mode t)
(setf show-paren-style 'expression)
(setf show-paren-delay 0)

(setf enable-local-variables t)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'Man-mode-hook 'visual-line-mode)

(when (string-match "GNU Emacs 23.*" (emacs-version))
  ;;split window right
  (fset 'split-window-right 'split-window-horizontally)
  (fset 'split-window-below 'split-window-horizontally))


(add-to-list 'auto-mode-alist '("\\.cs$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.graphml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]y$" . bison-mode))
(add-to-list 'auto-mode-alist '("[.]ya?ml$" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("[.]log$" . auto-revert-mode));;auto tail file

(when (get-buffer "*scratch*") (kill-buffer "*scratch*"))

(setf find-function-C-source-directory
      (expand-file-name "~/programs/source/emacs/emacs24-24.4+1/src/"))

(setf max-source-line-width 100)

(defun highlight-long-lines ()
  (interactive)
  (highlight-lines-matching-regexp
   (format ".\\{%d\\}" (1+ max-source-line-width)) 'hi-yellow))

(setf source-modes
      '(emacs-lisp-mode clojure-mode go-mode java-mode js-mode c-mode
			lisp-mode nxml-mode sh-mode python-mode))
(add-hook-to-modes 'highlight-long-lines source-modes)
(add-hook-to-modes (lambda () (setf show-trailing-whitespace t)) source-modes)



(put 'upcase-region 'disabled nil)

(defun firefox-new-tab (url &optional unknown-arg)
  (let ((proc
	 (open-network-stream "netcat-firefox-mozrepl" nil "127.0.0.1" 4242))
	(payload (format
		  ;;newline is important
		  "gBrowser.selectedTab = gBrowser.addTab(\"%s\");\n"
		  url)))
    (process-send-string proc payload)
    (sit-for 10)
    (delete-process proc)
     (message "opened %s" url)))

(defun which (&rest names)
  (let* ((cmd (format "which %s 2> /dev/null" (s-join " " names) ))
         (out (shell-command-to-string cmd)))
    (s-split "\n" out t)))


(setf browser-name
      (car (which "chromium" "chromium-browser" "chrome" "google-chrome")))

(defun chromium-new-tab (url &optional unknown-arg)
  (unless browser-name
    (error "browser-name is nil"))
  (start-process "browse-url" nil browser-name url)
  (message "opened %s" url))

(fset 'browser-new-tab #'chromium-new-tab)

(setq browse-url-browser-function 'browser-new-tab)

(setq custom-file "~/.emacs-custom.el")
(when (f-exists? custom-file)
  (load custom-file))

;; disable annoying tutorial
(define-key help-map (kbd "t") nil)
;; disable accidentally entering h h
(define-key help-map (kbd "h") nil)

(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

(provide 'my-emacs-settings)
;;; emacs-settings.el ends here
