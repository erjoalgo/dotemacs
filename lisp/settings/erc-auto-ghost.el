;;; erc-auto-ghost.el --- auto-ghost a user's own nick on IRC
;;
;; Filename: erc-auto-ghost.el
;; Description:
;; Author: ?
;; Maintainer:
;; Created: Mon Dec 17 20:32:56 2018 (-0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'erc)

(defun erc-ghost-maybe (_server nick)
  ;; taken from https://www.emacswiki.org/emacs/ErcTips
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s$" erc-nick-uniquifier) nick)
    (let ((nick-orig (match-string 1 nick))
          (password erc-session-password))
      (when (y-or-n-p (format "Current nick is '%s'.  Do you want to ghost? "
                              nick))
        (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
				       nick-orig password))
	(erc-cmd-NICK nick-orig)
	(erc-message "PRIVMSG" (format "NickServ identify %s %s"
				       nick-orig password))))))

(defadvice erc-nickname-in-use (before erc-set-random-nick-uniquifier activate)
  "Pick a random nick uniquifier to minimize chance of colliding again with ghosts."
  (setq erc-nick-uniquifier (char-to-string (seq-random-elt "`123456789aeiou"))))

(add-hook 'erc-after-connect 'erc-ghost-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erc-auto-ghost.el ends here
