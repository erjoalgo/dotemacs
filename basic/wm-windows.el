;;; wm-windows.el --- 

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: convenience, lisp

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

;;simple emacs lisp bindings based on wmctrl (for manipulating window manager windows from emacs)
;;; Code:



;; sample output wmctrl -xpl
;; 0x00800013  0 3429   emacs.Emacs           debian-lenovo emacs@debian-lenovo
;; 0x0040007f  0 1872   Navigator.Firefox     debian-lenovo Mozilla Firefox
;; 0x00a00007  0 1869   x-terminal-emulator.X-terminal-emulator  debian-lenovo ROXTerm


(require 's);;only needed for s-join

(defstruct wm-window
  id desktop pid class hostname title)

(defun wm-windows-list ()
  ;;hexid desktop-num-or-neg1 pid host title
  (let ((output (shell-command-to-string "wmctrl -xpl")))
    
    (loop for line in (split-string output "\n" t)
	  collect
	  (destructuring-bind (id desktop pid class hostname . title)
	      (split-string line " " t)
					;only title can have spaces, hopefully
					;actually, hostname or class may have spaces
					;but in practice do not
					;TODO ask wmctrl to tab-separate
	    (make-wm-window
	     :id id
	     :desktop desktop
	     :pid pid
	     :class class
	     :hostname hostname
	     :title (s-join " " title))))))

(defun wm-window-raise (wm-window)
  ;;it's really called 'activate' in wmctrl
  (let ((buff-name "wm-windows-raise"))
    (start-process buff-name buff-name
		   "wmctrl" "-ia" (wm-window-id wm-window))))

(defun wm-windows-find-window-by-class (class)
  (loop for win in (wm-windows-list)
	thereis (and (string= (wm-window-class win) class) win)))

(defun wm-windows-find-window-by-title-regexp (regexp)
  (loop for win in (wm-windows-list)
	thereis (and (string-match regexp (wm-window-title win)) win)))

(provide 'wm-windows)
;;; wm-windows.el ends here
