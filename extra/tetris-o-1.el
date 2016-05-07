;;; tetris-o-1.el --- 1
;; Copyright (C) 2014  Ernesto Alfonso <erjoalgo@gmail.com>


;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: games

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

;;; Commentary: Adds functions to emacs tetris to provide an interface for moving
;;; a tetromino to horizontal position x and rotation state r, in one step.

;;; Code:
(defcustom tetris-o-1-horizontal-keys
  ;;(list "1" "2" "3" "4" "5" "q" "w" "e" "r" "t")
  (list "1" "2" "3" "4" "q" "w" "e" "r" "s" "d")
  ;;(list [134217777] [134217778] [134217779] [134217780] [134217841] [134217847] [134217829] [134217842 134217825] [134217825] [134217843]  )
  "bindings for move-to-0 through move-to-8")
(defcustom tetris-o-1-rotation-keys
  ;(list "h" "j" "k" "l" )
  ;;(list [left] [up] [right] [down]  )
  ;;(list  [97]  [115]  [100]  [102])
  ;;(list [134217777]  [134217778]  [134217779]  [134217780])
  ;;(list "1"  "2"  "3"  "4")
  ;;(list [8388657] [8388658] [8388659] [8388660] )
  (list [f1] [f2] [f3] [f4] )
  "bindings for rotate-to-0 through rotate-to-3")
(defcustom tetris-o-1-enable-hints
  t
  "enable displaying hints")


(defun tetris-move-x-factory (i)
    `(lambda ()
      ,(format "Move the shape to the %d-th column." i)
      (interactive)
      (unless tetris-paused
	(let* (
	       (newpos (tetris-get-leftmost-x))
	       (movefun (if (< ,i newpos) 'tetris-move-left 'tetris-move-right))
	       oldpos)
	  (while (and (not (equal newpos ,i)) (not (equal oldpos newpos)))
	    (setq oldpos newpos)
	    (funcall movefun)
	    (setq newpos (tetris-get-leftmost-x)))
	  (tetris-move-bottom)))))

(defun tetris-rot-x-factory (i)
  `(lambda ()
  "Put the shape into the i-th rotation."
  (interactive)
  (unless (or tetris-paused (not (< ,i (tetris-shape-rotations))))
    (let (oldrot)
      (while (and (not (equal tetris-rot ,i)) (not (equal tetris-rot oldrot)))
	(setq oldrot tetris-rot )
	(tetris-rotate-next))))))

;on emacs-23, things are different
(if (string-match "^24.*" emacs-version)
    (defun tetris-get-leftmost-x ()
      (loop for i upto 3
	    minimize (+ tetris-pos-x (aref (tetris-get-shape-cell i) 0))))
  (progn
    (defun tetris-get-leftmost-x () tetris-pos-x)
    (defun tetris-shape-rotations ()	4)))
  




(defun tetris-key-to-char (key)
  (let* ((str  (if (stringp key)
		   key
		 (key-description key)))
	 (chr (cond
	       ((eq (length str) 1) str)
	       ((string-match "\\(M\\|s\\)-" str) (substring str 2))
	       ((t str)))))
    (string-to-char chr)))

  
(defadvice tetris (after tetris-o-1-bindings activate)
;not necessary to use advice. will use tetris-mode-hook
;(defun tetris-o-1-bindings ()
  
  (loop for key in tetris-o-1-horizontal-keys
	for i from 0 do
	(define-key tetris-mode-map key (tetris-move-x-factory i)))

  (loop for key in tetris-o-1-rotation-keys
	for i from 0 do
	(define-key tetris-mode-map key (tetris-rot-x-factory i)))
  
  
  (when tetris-o-1-enable-hints
    (let
	((hor-chars
	  (loop with str = (make-string (length tetris-o-1-horizontal-keys) 0)
		for key in tetris-o-1-horizontal-keys
		for i from 0
		as c = (unwind-protect (tetris-key-to-char key) 63)
		do (aset str i c)
		finally (return str)))
	 join-space)

      (fset 'join-space (lambda (str)
			  (loop
			   with len = (1- (* 2 (length str)))
			   with new-str = (make-string len 0)
			   for i below len
			   do (aset new-str i
				    (if (evenp i)
					(aref str (floor (/ i 2)))
				      32))
			   finally (return new-str))))
      
      (loop for keystring across (join-space hor-chars)
	    for i from 0 do
	    (gamegrid-set-cell
	     (+ tetris-top-left-x i)
	     (+ tetris-top-left-y tetris-height)
	     keystring)))))

(defun tetris-o-1-no-speedup ()
  (interactive)
  (setq tetris-update-speed-function
	;;(lambda (a b) (/ 20.0 50.0)))
	(lambda (a b) (/ 20.0 20.0)))
  (message "speedup disabled"))
  
;(add-hook 'tetris-mode-hook 'tetris-o-1-bindings)
(provide 'tetris-o-1)
;;; tetris-o-1.el ends here
