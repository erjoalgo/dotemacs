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



(defvar emacs_backups_dir "~/.emacs-backups")

;;taken from the internet
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 `(("." . ,emacs_backups_dir))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" ,emacs_backups_dir t)))


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
(setq split-height-threshold 0)

(provide 'emacs-settings)
;;; emacs-settings.el ends here
