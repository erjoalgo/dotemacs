;;; command-mode.el --- 

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
;; Provides command mode for easy access to usually Ctrl-prefixed commands in emacs
;; f1 to toggle mode on/off,
;; clear visual indicator when mode is on
;; Most of the keybindings in this mode are obtained by simply removing the Ctrl or Ctrl+x prefix of emacs
;;

;;; Code:

(defvar command-mode-map (make-sparse-keymap))

;;sub maps
(defvar open-init-files-map (make-sparse-keymap))
(defvar open-interpreter-map (make-sparse-keymap))

(define-minor-mode command-mode
    "command mode"
    0 "-CM" command-mode-map)
(define-globalized-minor-mode global-command-mode command-mode command-mode)

(defun command-mode-hook-add-color ()
  "add a visual indicator of current mode"
  (let ((color (if command-mode "dark green" "dark gray")))
  ;(let ((color (if command-mode "dark blue" "dark gray")))
	(set-face-background 'mode-line color)
	(set-cursor-color color)))
(add-hook 'command-mode-hook 'command-mode-hook-add-color)

(defun global-command-mode-toggle ()
  (interactive)
  (global-command-mode (if command-mode 0 1)))



(define-key global-map [f1] 'global-command-mode-toggle)
(define-key global-map [f12] 'global-command-mode-toggle)

'(defun define-key-tuples (kbd-cmd-tuples &optional kmap)
  (unless kmap (setq kmap (make-sparse-keymap)))
  (loop for (key command) in kbd-cmd-tuples
	do (define-key (kbd key) kmap command))
  kmap)

(defmacro define-key-tuples-macro (kmap command-maker &rest kbd-cmd-tuples)
  (unless command-maker (setq command-maker ''identity))
  (unless kmap (setq kmap '(make-sparse-keymap)))
  `(let ((command-maker ,command-maker)
	 (kmap ,kmap))
     (loop for (key command) in (backquote ,kbd-cmd-tuples)
	   do (define-key kmap (if (vectorp key) key (kbd key)) (funcall ,command-maker command)))
     kmap))

(defun string-insert-command (str)
  `(lambda ()
     (interactive)(insert ,str)))




;;to change, simply add/remove bindings, then eval the sexp, ie x e
(define-key-tuples-macro
  command-mode-map
  nil
  
  ("1" scroll-up-keep-cursor);;originally M-v
  ("2" scroll-down-keep-cursor);;originally C-v

  

  ("n" next-line);;originally C-n
  ("N" copy-line-down);; copy full line down
  ("M-n" move-line-down);; move line down
  
  ("p" previous-line);;originally C-p
  ("P" copy-line-up);; copy full line up
  ("M-p" move-line-up);; move line up

  ("a" beginning-of-line);;originally C-a
  ("a" my-move-beginning-of-line);;originally C-a
  ("e" move-end-of-line);;originally C-e
  ("k" kill-line);;originally C-k
  ("K" my-kill-whole-line);; kill entire current line
  ("j" newline-and-indent);;originally C-j
  ("o" open-line);;originally C-o


  ("v" yank-or-pop);;originally C-y (make it like windows Ctrl+v)
  ("." end-of-buffer);;originally M->
  ("," beginning-of-buffer);;originally M-<
  ("f" forward-char);;originally C-f
  ("b" backward-char);;originally C-b
  ("/" undo);;originally C-/
  

  ("M-f" forward-sexp);;originally C-M-f
  ("M-b" backward-sexp);;originally C-M-b
  ("M-k" kill-sexp);;originally C-M-k
  ;("DEL" backward-delete-char)
  ;([127] backward-delete-char)
  ("M-DEL" backward-kill-sexp);;originally ESC C-backspace
  ("r" set-mark-command);;originally C-SPC
  ;("SPC" set-mark-command)
  ;("SPC" nil)

  ("w" kill-ring-save);;originally M-w
  ("W" kill-surrounding-sexp);; incrementally kill backward-sexp. incrementally displays what is being killed
  ("M-w" kill-region);;originally C-w

  
  ("M-1" delete-other-windows);;originally C-x 1
  ("M-2" my-split-window-below);;originally C-x 2
  ("M-3" my-split-window-right);;originally C-x 3
  ("M-q" delete-window);;originally C-x 0
  ([f2] other-window);;originally C-x o


  ("c" cycle-buffer);; cycle buffers
  ("C" duplicate-current-buffer);; open current buffer in split-window-below
  ("z" kill-this-buffer);;originally C-x k
  ("q" bury-buffer);; move current buffer to end of the list
  

  ("3" find-file-at-point-cmd);;originally C-x f
  ("4" switch-to-buffer);;originally C-x b
  
  
  ("s" save-buffer);;originally C-x s

  
  ("l" recenter-top-bottom);;originally C-l
  ("d" delete-char);;originally C-d
  ("M-d" kill-word);;originally M-d
  ("D" my-forward-delete);;like kill-word, but sexp, and no kill.


  ;("," tags-loop-continue)
  ;("" my-backward-delete)
  ([134217849] quick-yes-answer-yes);; type "yes RET" for those annoying prompts. the key is s-SPC (super space)


  
  ("-" global-text-scale-lower);; increase text size
  ("=" global-text-scale-higher);; decrease text size
  
  ("g" goto-line)

  ("x" ,open-interpreter-map);;custom map
  ("m" ,open-init-files-map);;custom map

  ("	" indent-for-tab-command)
  
  ("9" ,(string-insert-command "("));; insert "("
  ("0" ,(string-insert-command ")"));; insert ")"
  
  ("" universal-argument)
  ("u" universal-argument)

  ("'" ,(string-insert-command "\""));; insert double-quote
  
  ("i" one-char-insert-mode)
  
  ("h" (lambda (arg) (interactive "P")
	 "with prefix arg, keep command-mode while on help-command map
	  ie for inspecting command-mode bindings
	  ie u h k g
	  g runs the command goto-line, which is an interactive compiled Lisp..."
	 (set-temporary-overlay-map 'help-command)
	 (unless arg
	   (global-command-mode 0))))
  
  ([f1] global-command-mode-toggle);; f1 toggle command mode
  )



(defun curry (fun &rest fixed-args)
  `(lambda (&rest args)
     ,(when (commandp fun) '(interactive))
     (apply ',fun (append (list ,@fixed-args) args))))
  

;(fset 'join-base-dir (curry 'f-join basic-top))
(fset 'join-base-dir (curry 'concat "~/repos/dotemacs/basic/"))

(define-key-tuples-macro
  open-init-files-map
  (lambda (fn) (interactive) 
    (curry (if (member fn '("*Messages*" "*scratch*" "*slime-repl sbcl*"))
	       'switch-to-buffer 'find-file) fn))
  ("e" "~/.emacs")
  ("E" "~/repos/emacs-dirty/.emacs-bloated.el")
  ("C" (join-base-dir "command-mode.el"))
  ("c" (join-base-dir "command-mode-commands.el"))
  ("b" "~/.bashrc")
  ("a" "~/.bash_aliases")
  ("m" "*Messages*")
  ("s" "*scratch*")
  ("t" (join-base-dir "python_buttons.el"))
  ("M" (concat "/var/mail/" (getenv "USER")))
  ("x" (concat  "~/repos/stumpwm/xmodmap/.xmodmaprc"))
  ("y" (concat stumpwm_dir ".my_startups.sh"))
  ;("s" "/sudo::/var/log/syslog")
  ("v" (concat stumpwm_dir "keynavs/.keynavrc"))
  ("w" "~/repos/stumpwm/.stumpwmrc")
  ;;("c" "/sudo::/etc/anacrontab")
  ;;("C" "/sudo::/etc/crontab")
  ("r" "~/repos")
  ("7" "~/repos/unix_utils")
  ("8" "~/repos/starter/data/packages")
  ("9" "~/programs")
  )

(define-key-tuples-macro
  open-interpreter-map
  nil 
  ("s" (lambda (arg)(interactive "P")(eshell arg)))
  ("p"  (lambda () (interactive)(call-interactively 'run-python)(switch-to-buffer (get-buffer-by-regex "*Python*")))) 
  ;;("p" 'run-python)
  ("i" ielm)
  ("c" music-player-play-songs)
  ("e" my-eval-defun)
  ("E" (lambda () (interactive)(eval-defun t)))
  ("x" execute-extended-command)
  ("X" sudo-buffer)
  (";" eval-expression)
  ("m" switch_to_planner)
  ("M" (lambda () (interactive) (call-interactively 'man)))
  ("a" async-shell-command)
  ("v" revert-buffer-no-confirm)
  ("y" (lambda () (interactive) async-shell-command "youtube_browser.py" ))
  ("n" find-new-buffer)
  ("f" find-file-other-window)
  ("D" (lambda () (interactive)(call-interactively 'pdb)))
  ("g" grep-search-default)
  ("u" universal-argument)
  ([f2] call-last-kbd-macro)
  ("z" airmacs_read_key)
  ("l" alert)
  ;;("w" switch-to-slime-repl)
  ("w" sbcl-slime)
  ("W" slime-connect-stumpwm)
  ("S" goto-slime-buffer)
  ("t" untarprogram)
  ("r" (lambda () (interactive) (start-process (current-buffer-name) nil (current-buffer-name))))
  ("R" erc-autologin)
  ("A" (lambda () (interactive) (switch-to-buffer (get-buffer-by-regex "\\*Async Shell Command\\*"))))
  ;;("k" wiki)
  ("k" goto-slime-buffer)
  ("o" goto-inbox)
  ("0" open-google-calendar)
  ("P" (lambda () (interactive)(message "point is %d" (point))))
  ;; ("b" matlab-shell)
  ;; ("B"  run-octave)
  ("b" run-or-switch-to-matlab-or-octave)
  ("B" run-or-switch-to-matlab-or-octave)
  )


;;http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797#5340797
(defun force-mode-first (mode-symbol)
  "Try to ensure that my keybindings always have priority."
  (when (not (eq (car (car minor-mode-map-alist)) mode-symbol))
    (let ((mykeys (assq mode-symbol minor-mode-map-alist)))
      (assq-delete-all mode-symbol minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

;;not sure this is necessary
(defadvice load (after give-my-keybindings-priority)
  (force-mode-first 'command-mode))
(ad-activate 'load)

(global-set-key [M-f1] 'goto-last-change)
(global-set-key (kbd "M-Z") 'replace-regexp)
(global-set-key (kbd "M-z") 'query-replace-regexp)
(global-set-key [escape] 'exit-recursive-edit)
(global-set-key [f4] 'keyboard-escape-quit)
(define-key 'help-command "y" 'find-function);;find source for function-at-point
(global-set-key [s-backspace] (string-insert-command "`"))
(global-set-key (kbd "s-SPC") (string-insert-command ", "))

(loop for map in (list global-map command-mode-map)
      do (define-key map [f5] (lambda ()
			      (interactive)
			      (insert "(")
			      (recursive-edit)
			      (insert ")"))))


(defun one-char-insert-mode ()
  "insert the next char as text"
  (interactive)
  (set-temporary-overlay-map global-map))


(defun define-sfn-prefix-args-to-map (kmap)
  "installs s-f[1-9] command-repeat prefixes
  eg s-f3 K kills next 3 lines"
  (loop for i from 1 to 9 do
	(define-key kmap (kbd (format "<s-f%d>" i))
	  `(lambda () (interactive)
	     (setf prefix-arg ,i)))))


(mapc 'define-sfn-prefix-args-to-map
      (list command-mode-map global-map))
      ;(list command-mode-map global-map isearch-mode-map))



;;this is actually part of command-mode
;;make mode-line text very big and easy to read
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "dark gray" :foreground "white" :box (:line-width -1 :style released-button) :weight normal :height 3.0 :width extra-expanded))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light :height 1.1 :width normal))))
 ;(set-face-attribute 'region nil :background "green")
'(region ((t :background "#666" :foreground "#ffffff")))
 )





;;automatically disable command-mode when entering minibuffer
;;on minibuffer-exit, enable it if it was originally on
(defvar was-in-command-mode-before-minibuf)
(add-hook 'minibuffer-setup-hook
	  (lambda () (setq was-in-command-mode-before-minibuf command-mode)
	    (global-command-mode 0)))

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (when (eq was-in-command-mode-before-minibuf t)
	      (global-command-mode 1))))

(global-command-mode 1)

(require 'command-mode-commands);;maybe should not be separate package
(provide 'command-mode)
;;; command-mode.el ends here
