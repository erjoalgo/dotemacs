;;; music-player.el --- 

;; Copyright (C) 2014  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: multimedia

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

;;; Commentary: Provide a simple interface for music playing from within emacs,
;;; using external programs to play each music file

;;; Code:



(defgroup music-player nil
  "music player"
  :group 'music-player
  :prefix "music-player-")

(defcustom music-player-top-dirs
  ;'("~/Music/" "~/Downloads/Music/")
  nil ;user must define this forcefully
  "directories where to search for songs"
:group 'music-player)

(defcustom music-player-song-cache nil
  "cache containing all songs found previously under `music-player-top-dir'"
:group 'music-player)

(defcustom music-player-basename-only-p nil
  "perform regexp search only on file basenames. set this to nil when generic filenames like 'unkown-artist-track-01.mp3' are found under meaningful directory names like 'Juan_Luis_Guerra'."
:group 'music-player)

(defcustom music-player-extension-rules
  '(
    (("wav") . ("aplay"))
    (("midi?" "not") . ("timidity"))
    (("m4a" "flac") . ("mplayer"))
    (("mp3") . ("mpg321"))
    )
  "mapping of extensions to programs that can play them, with their arguments"
  :group 'music-player)

(defcustom music-player-key-bindings
  '(
    ([f9] . music-player-toggle-pause-song)
    ([f10] . music-player-next-song)
    )
  "key bindings for two main music-player functions"
  :group 'music-player)

(defvar music-player-suspended-song-p nil  "interal. t when song is currently suspended")
(defvar music-player-current-song-process nil "internal. process of currently playing or paused song")
(defvar music-player-current-song-playlist nil "internal. list of pending filenames to play")
(defvar music-player-song-index -1 "interal. used to enable 'rewinding' to previous song. not implemented")
(defvar music-player-loop nil  "if non-nil, repeat last search and play")
(defvar music-player-last-regex nil  "last user regex search for music")


(require 'cl);need this for reduce and remove-if-nil (or filter) functions

(defun music-player-get-all-songs (&optional regex reload)
  (when (not music-player-top-dirs) (error "music-player-top-dirs is not defined."))
  (when (or (not music-player-song-cache) reload)
    (let (find-fn-args find-cmd)
      (setq find-fn-args
	    (reduce
	     (lambda (a b) (concat a " '" (expand-file-name b)  "'"))
	     (remove-if-not 'file-exists-p music-player-top-dirs) :initial-value ""))
      (setq find-cmd (format "find -L %s -type f -print0" find-fn-args))
      ;(message "searching music dir")
      (message "%s" find-cmd)
      (setq music-player-song-cache
	    (split-string (shell-command-to-string find-cmd) "\0" t))
      )
    )
  music-player-song-cache
  )

(defun music-player-play-songs (arg regex)
  (interactive "P\nsenter regex for songs: ")
  (setq music-player-last-regex regex)
  (let ((case-fold-search t ) (regexp (format ".*%s.*" regex)))
    (setq music-player-current-song-playlist
	  (remove-if-not
	   (lambda (s) (string-match regexp (if music-player-basename-only-p (basename s) s) ))
	   (music-player-get-all-songs nil arg))))
  ;(message (prin1-to-string current-song-playlist))
  (if (not music-player-current-song-process)
      (music-player-next-song)
    (message "new songs will play after current one finishes (call (next-song) to play immediately)")
    )
  )



(defun music-player-get-song-program (song rules-alist)
  (let (
	(exts-to-program (car rules-alist))
	)
    (when exts-to-program
      (if (matches-extension (car exts-to-program) song)
	  (cdr exts-to-program)
	(music-player-get-song-program song (cdr rules-alist)))
      )
    )
  )

(defun music-player-toggle-pause-song ()
  "pause or unpause current song in `music-player-current-song-process'"
  (interactive)
  (if music-player-current-song-process
      (progn 
	(kill (if music-player-suspended-song-p "CONT" "STOP") music-player-current-song-process)
	(setq music-player-suspended-song-p (not music-player-suspended-song-p))
	)
    (message "no song is currently playing or paused")
    )
  )

(defun music-player-next-song ()
  "play next song in `music-player-current-song-playlist'"
  (interactive)
  (when music-player-current-song-process
    (set-process-sentinel music-player-current-song-process nil )
    (delete-process music-player-current-song-process)
    (setq music-player-current-song-process nil )
    )

  (if music-player-current-song-playlist
      (let* ((song (car music-player-current-song-playlist))
	     (song-program (music-player-get-song-program song music-player-extension-rules)))
	
	(setq music-player-current-song-playlist (cdr music-player-current-song-playlist))
	(if (not song-program)
	    (progn
	      (message (format "don't know how to play this song: %s. skipping..." song))
	      ;(sleep-for 1)
	      (music-player-next-song))
	  (progn 
	    (setq music-player-current-song-process
		  (apply
		   'start-process
		   "music-player"
		   "music-player"
		   (car song-program)
		   (cons song (cdr song-program))))
	    
	    (set-process-sentinel
	     music-player-current-song-process
	     (lambda (process-name change)
	       (message "change: %s" change)
	       (setq pchange change)
	       (unless (equal "stopped (signal)\n" change)
		 (music-player-next-song)))
	     )
	    
	    (setq music-player-suspended-song-p nil )
	    (message (format "now playing: %s" song)))
	))
    (progn
      (setq music-player-current-song-process nil )
      (if music-player-loop
	  (music-player-play-songs nil music-player-last-regex)
	(when  (y-or-n-p "no more queued song. search again?")
	  (call-interactively 'music-player-play-songs))))
    )
  )

;;duplicated
(defun kill (signl proc)
  (shell-command
   (format "kill -%s %s"
	   (if (integerp signl) (format "%d" signl) signl)
	   (if (processp proc) (process-id proc) proc)))
  )
(defun matches-extension (extensions fn)
  ;(string-match ".*\\.\\(midi?\\|not\\)\\(([0-9]+)\\)?$" song)  
  (string-match (format ".*\\.\\(%s\\)\\(([0-9]+)\\)?$" (ors-regex extensions)) fn)
  )
(defun ors-regex (strings)
 "Returns a regex matching union of regexes in list"
 (interactive (list (read-string-list "Enter next regex")))
 (substring
  (reduce '(lambda (x y) (concat x "\\|\\(" y "\\)")) strings :initial-value "" )  2)
 )

(mapc (lambda (key-fun) (define-key global-map (car key-fun) (cdr key-fun))) music-player-key-bindings)


(provide 'music-player)

(defun music-player-toggle-loop ()
  (interactive)
  (message "loop on? %s"
	   (setq music-player-loop (not music-player-loop)))
  )

;;; music-player.el ends here
