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



(defconst emacs-backups-dir "~/.emacs-backups")

;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        

(setq backup-directory-alist
      `((".*" . ,emacs-backups-dir))
      
      auto-save-file-name-transforms
      `((".*" ,emacs-backups-dir t))
      
      setq auto-save-list-file-prefix
      emacs-backups-dir)
;;taken from the internet
(setq
 backup-by-copying t      ; don't clobber symlinks
 ;backup-directory-alist
 ;`(("." . ,emacs-backups-dir))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups



(setq apropos-do-all t);;include functions in apropos, not just commands

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
;;TODO the same for async-shell-command file names

(require 'server)
(unless (server-running-p)
  (server-start))

;(setq split-window-preferred-function 'split-window-vertically) doesn't work
;(setq split-height-threshold 0)

(setq inhibit-startup-screen t)

(show-paren-mode t)
(setf show-paren-style 'expression)
(setf show-paren-delay 0)

(setq enable-local-variables :safe)

(put 'erase-buffer 'disabled nil)

(add-hook 'Man-mode-hook 'visual-line-mode)

(when (string-match "GNU Emacs 23.*" (emacs-version))
  ;;split window right
  (fset 'split-window-right 'split-window-horizontally)
  (fset 'split-window-below 'split-window-horizontally))


(add-to-list 'auto-mode-alist '("\\.cs$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.graphml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]y$" . bison-mode))

(kill-buffer "*scratch*")
(provide 'emacs-settings)
;;; emacs-settings.el ends here
