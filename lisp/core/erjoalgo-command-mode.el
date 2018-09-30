;;; erjoalgo-command-mode.el ---

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
;; Provides command mode for single-key access to
;; Ctrl-prefixed and other common commands in emacs
;; f1 to toggle mode on/off,
;; clear visual indicator when mode is on
;; Most of the keybindings in this mode are obtained
;; by removing the Ctrl or Ctrl+x prefix of emacs
;;

;;; Code:

(defvar erjoalgo-command-mode-map (make-sparse-keymap))

;;sub maps
(defvar open-interpreter-map (make-sparse-keymap))

(define-minor-mode erjoalgo-command-mode
    "command mode"
    0 "-CM" erjoalgo-command-mode-map)

(define-globalized-minor-mode global-erjoalgo-command-mode
  erjoalgo-command-mode erjoalgo-command-mode)

(defvar *erjoalgo-command-mode-color-on* "dark green")
(defvar *erjoalgo-command-mode-color-off* "dark gray")

(defun erjoalgo-command-mode-hook-add-color ()
  "add a visual indicator of current mode"
  (let ((color (if erjoalgo-command-mode
		   *erjoalgo-command-mode-color-on*
		 *erjoalgo-command-mode-color-off*)))
    (set-face-background 'mode-line color)))

(add-hook 'erjoalgo-command-mode-hook 'erjoalgo-command-mode-hook-add-color)
(add-hook 'global-erjoalgo-command-mode-hook 'erjoalgo-command-mode-hook-add-color)

(defun global-erjoalgo-command-mode-toggle ()
  (interactive)
  (setf prefix-arg current-prefix-arg)
  (global-erjoalgo-command-mode (if erjoalgo-command-mode 0 1)))



(define-key global-map [f1] 'global-erjoalgo-command-mode-toggle)
(define-key global-map (kbd "<s-f11>") 'global-erjoalgo-command-mode-toggle)
(define-key global-map (kbd "ë") 'global-erjoalgo-command-mode-toggle)

(defun erjoalgo-command-mode-meta-pn ()
  (interactive)
  (if (member major-mode '(slime-repl-mode inferior-emacs-lisp-mode))
      (call-interactively (lookup-key
                           (symbol-value (intern (format "%s-map" major-mode)))
                           (this-command-keys-vector)))
    (call-interactively
     (if (equal (key-description (this-command-keys-vector)) "M-n")
         'move-line-down 'move-line-up))))

(buttons-macrolet
 ((dir (dir) `(find-file-under-dir-completing-read ,dir))
  (buff (buff &optional on-nonexistent)
        `(cmd (or (switch-to-buffer-matching ,buff)
                  ,on-nonexistent
                  (error (format "no such buffer: %s" ,buff)))))
  (file (file) `(cmd (find-file ,file))))
 (defbuttons
   erjoalgo-command-mode-buttons
   nil
   (erjoalgo-command-mode-map)
   (buttons-make
    nil
    ("1" 'scroll-up-keep-cursor);;originally M-v
    ("2" 'scroll-down-keep-cursor);;originally C-v
    ("n" 'next-line);;originally C-n
    ("N" 'copy-line-down);; copy full line down
    ((kbd "M-n") 'erjoalgo-command-mode-meta-pn);; move line down
    ((kbd "M-p") 'erjoalgo-command-mode-meta-pn);; move line up
    ("p" 'previous-line);;originally C-p
    ("P" 'copy-line-up);; copy full line up
    ("a" 'beginning-of-line);;originally C-a
    ("a" 'my-move-beginning-of-line);;originally C-a
    ("e" 'move-end-of-line);;originally C-e
    ("k" 'kill-line);;originally C-k
    ("K" 'my-kill-whole-line);; kill entire current line
    ("j" (lambda (arg)
           (interactive "P")
           (dotimes (_ (or arg 1)) (newline-and-indent))));;originally C-j
    ("o" 'open-line);;originally C-o
    ("v" 'yank-or-pop);;originally C-y (make it like windows Ctrl+v)
    ("." 'end-of-buffer);;originally M->
    ("," 'beginning-of-buffer);;originally M-<
    ("f" 'forward-char);;originally C-f
    ("b" 'backward-char);;originally C-b
    ("/" 'undo);;originally C-/
    ((kbd "M-f") 'forward-sexp);;originally C-M-f
    ((kbd "M-b") 'backward-sexp);;originally C-M-b
    ((kbd "M-k") 'kill-sexp);;originally C-M-k
    ((kbd "M-DEL") 'backward-kill-sexp);;originally ESC C-backspace
    ("r" 'set-mark-command);;originally C-SPC
    ("w" 'kill-ring-save);;originally M-w
    ;; incrementally kill backward-sexp. incrementally displays what is being killed
    ("W" 'kill-surrounding-sexp)
    ((kbd "M-w") 'kill-region);;originally C-w
    ((kbd "M-1") 'delete-other-windows);;originally C-x 1
    ((kbd "M-2") 'my-split-window-below);;originally C-x 2
    ((kbd "M-3") 'my-split-window-right);;originally C-x 3
    ((kbd "M-q") 'delete-window);;originally C-x 0
    ([f2] 'other-window);;originally C-x o
    ((kbd "<S-f2>") (cmd (other-window -1)));;originally C-x o
    ("c" 'cycle-buffer);; cycle buffers
    ("C" 'duplicate-current-buffer);; open current buffer in split-window-below
    ("z" (cmd (kill-buffer (current-buffer))));;originally C-x k
    ("Z" 'new-buffer-focus)
    ("q" 'bury-buffer);; move current buffer to end of the list
    ("3" 'find-file-at-point-cmd);;originally C-x f
    ("4" 'switch-to-buffer);;originally C-x b
    ("5" (buttons-make
          nil
          ("g" (cmd (dir "~/git")))
          ("l" (cmd (dir "~/private-data/leet/")))))
    ("6" (cmd (dir org-top-dir)))
    ("s" 'save-buffer);;originally C-x s
    ("l" 'recenter-top-bottom);;originally C-l
    ("d" 'delete-char);;originally C-d
    ((kbd "M-d") 'kill-word);;originally M-d
    ("D" 'my-forward-delete);;like kill-word, but sexp, and no kill.
    ;; type "yes RET" for those annoying prompts. the key is s-SPC (super space)
    ("-" 'global-text-scale-lower);; increase text size
    ("=" 'global-text-scale-higher);; decrease text size
    ("g" 'goto-line)
    ("	" (lambda ()
            (interactive)
            (call-interactively
             (case major-mode
               ((Custom-mode) 'widget-forward)
               ((help-mode apropos-mode debugger-mode) 'forward-button)
               ((Info-mode) 'Info-next-reference)
               (t 'indent-for-tab-command)))))
    ("9" "(");; insert "("
    ("0" ")");; insert ")"
    ("" 'universal-argument)
    ("u" 'universal-argument)
    ("'" "\"");; insert double-quote
    ("i" 'one-char-insert-mode)
    ("h" (lambda (arg) (interactive "P")
           "with prefix arg, keep erjoalgo-command-mode while on help-command map
	  ie for inspecting erjoalgo-command-mode bindings
	  ie u h k g
	  g runs the command goto-line, which is an interactive compiled Lisp..."
           (set-temporary-overlay-map 'help-command)
           (unless (or arg
                       global-erjoalgo-command-mode)
             (global-erjoalgo-command-mode 0))))
    ;; ([f1] nil);; f1 toggle command mode
    ([f1] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ([s-f11] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ([ë] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ("y" (search-engine-search-cmd "ddg"))
    ("Y" (search-engine-search-cmd "linguee"))
    ("J" (lambda (arg)
           (interactive "P")
           (loop for _ below (or arg 1)
                    do (join-line '(4)))))
    ("m"
     (buttons-make
      nil
      ("e" (file "~/.emacs"))
      ("C" (buff "regexp:[*]ansi-term[*].*" (ansi-term "/bin/bash")))
      ("c" (buff "*compilation*"))
      ("r" (buff "*Backtrace*"))
      ("b" (file "~/.bashrc"))
      ("a" (file "~/.bash_aliases"))
      ("A" (file "~/.my-bash-funs"))
      ("m" (buff "*Messages*"))
      ("s" (buff "*Org Agenda*" (org-todo-list)))
      ("S" (buff "*Org Agenda*"))
      ("t" (file (f-join emacs-top "settings" "buttons-data.el")))
      ((kbd "s-t") (file (f-join emacs-top "core" "erjoalgo-command-mode.el")))
      ((kbd "s-c") (file (f-join emacs-top "core" "erjoalgo-compile.el")))
      ("M" (file (concat "/var/mail/" (getenv "USER"))))
      ("x" (file (concat  "~/.stumpwmrc.d/inits/.xmodmap/")))
      ("l" (file "/sudo::/var/log/syslog"))
      ("v" (file "~/.stumpwmrc.d/keynavs/.keynavrc"))
      ("w" (file "~/.stumpwmrc.d/lisp/.stumpwmrc"))
      ("W" (buff "*Warnings*"))
      ("o" (file "~/private-data/org/master.org"))
      ("T" (cmd (org-todo-list org-match)))
      ("O" nil)
      ("j" (buff "*-jabber-roster-*"))))
    ("x"
     (buttons-make
      nil
      ("s" (lambda (arg)(interactive "P")(eshell arg)))
      ("p"  (buff "*Python*" (call-interactively 'run-python)))
      ("P" 'message-current-buffer-process)
      ("i" 'ielm)
      ("I" 'load-dark-theme-toggle)
      ("c" 'music-player-play-songs)
      ("C" 'kill-current-buffer-filename)
      ("e" 'my-eval-defun)
      ("E" (cmd (eval-defun t)))
      ("x" 'execute-extended-command)
      ("X" 'sudo-buffer)
      (";" 'eval-expression)
      ("M" 'man)
      ("a" 'async-shell-command)
      ("v" 'revert-buffer-no-confirm)
      ("n" 'find-new-buffer)
      ("D" (cmd
            (require 'edebug)
            (eval-defun t);;instrument first
            (edebug-set-breakpoint nil)))
      ("g" 'grep-recursive)
      ("f" 'find-iregex)
      ("u" 'universal-argument)
      ([f2] 'call-last-kbd-macro)
      ("w" (buttons-make
            nil
            ("1" 'slime-sbcl)
            ("2" 'slime-stumpwm)
            ("3" 'cider-buffer-or-jack-in)))
      ("S" 'goto-slime-buffer)
      ("r" 'replace-regexp)
      ("R" 'query-replace-regexp)
      ("A" (buff "[*]Async Shell Command[*]"))
      ("o" 'gnus-goto-inbox)
      ("0" 'open-google-calendar)
      ("b" (buff "*Inferior Octave*" (inferior-octave t)))
      ("3" (buff "*eww*" (call-interactively 'eww))))))))

(defun find-file-under-dir-completing-read (dir)
  (find-file (f-join dir
		     (completing-read (concat dir ": ")
				      (directory-files dir)))))

(defun buffer-matching (string &optional regexp-p)
  (let ((prefix "regexp:"))

    (when (s-starts-with-p prefix string)
	(setf regexp-p t
	      string (substring string (length prefix))))

    (if (not regexp-p) (get-buffer string)
      (loop for buff in (buffer-list) thereis
	    (and (string-match string (buffer-name buff))
		 buff)))))

(defun switch-to-buffer-matching (string &optional regexp-p)
  (let ((match (buffer-matching string regexp-p)))
    (when match (switch-to-buffer match))))

;;http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797#5340797
(defun force-mode-first (mode-symbol)
  "Try to ensure that my keybindings always have priority."
  (when (not (eq (car (car minor-mode-map-alist)) mode-symbol))
    (message "forcing mode first")
    (let ((mykeys (assq mode-symbol minor-mode-map-alist)))
      (assq-delete-all mode-symbol minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(define-key global-map [f2] 'other-window)
(define-key global-map (kbd "<s-f12>") (lambda () (interactive)
                                         (save-buffer)
                                         (erjoalgo-command-mode 1)))

(add-hook 'after-load-functions
	  '(lambda (something)
	     (force-mode-first 'erjoalgo-command-mode)))


(global-set-key [M-f1] 'goto-last-change)

(global-set-key [escape] 'exit-recursive-edit)
(global-set-key [f4] 'keyboard-escape-quit)
(define-key 'help-command "y" 'find-function);;find source for function-at-point

(buttons-macrolet
 ()
 (global-set-key [s-backspace] (cmd (ins "`")))
 (global-set-key (kbd "s-SPC") (cmd (ins ", "))))

(global-set-key (kbd "<C-f11>") 'eval-buffer)
(global-set-key (kbd "M-SPC") (lambda (arg) (interactive "P")
                              (capitalize-last arg) (insert " ")))

(loop for map in (list global-map erjoalgo-command-mode-map)
      do (define-key map [f5] nil))


;;apropos
(define-key 'help-command "A" 'apropos-variable)
;;TODO remove this hackery
(defun add-one-time-hook (hook fun)
	 (let ((fun-sym (gensym)))
	   (fset fun-sym `(lambda ()
			   (funcall ,fun)
			   (remove-hook ,hook ,fun-sym)))
	   (add-hook hook fun-sym)))

(add-hook 'apropos-mode-hook
	  (lambda ()
	    (add-one-time-hook
	     'post-command-hook
	     (lambda ()
	       (select-window (get-buffer-window "*Apropos*"))))))

(defun one-char-insert-mode (arg)
  "insert the next char as text"
  (interactive "P")
  (set-temporary-overlay-map global-map))


(setf super-fn-prefix-arg-map
      (loop with kmap = (make-sparse-keymap)
            for i from 1 to 9 do
            (define-key kmap (kbd (format "<s-f%d>" i))
              `(lambda () (interactive)
                 (setf prefix-arg ,i)))
            finally (return kmap)))

(define-keymap-onto-keymap super-fn-prefix-arg-map erjoalgo-command-mode-map)
(define-keymap-onto-keymap super-fn-prefix-arg-map global-map)

;;this is actually part of erjoalgo-command-mode
;;make mode-line text very big and easy to read
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "dark gray"
			      :foreground "white"
			      :box (:line-width -1 :style released-button)
			      :weight normal
			      :height 2.0
			      :width extra-expanded))))
 '(mode-line-inactive ((t (:inherit mode-line
				    :background "grey90"
				    :foreground "grey20"
				    :box (:line-width -1 :color "grey75")
				    :weight light
				    :height 1.1
				    :width normal))))
 ;(set-face-attribute 'region nil :background "green")
'(region ((t :background "#666" :foreground "#ffffff"))))

(defun load-dark-theme-toggle ()
  (interactive)
  (let ((dark-theme 'wombat))
    (if (custom-theme-enabled-p dark-theme)
	(progn
	  (disable-theme dark-theme)
	  (setf *erjoalgo-command-mode-color-on* "dark green"
		*erjoalgo-command-mode-color-off* "dark gray"))
      (progn
	(load-theme dark-theme)
	  (setf *erjoalgo-command-mode-color-on* "light green"
		*erjoalgo-command-mode-color-off* "light gray")))))

;;automatically disable erjoalgo-command-mode when entering minibuffer
;;on minibuffer-exit, enable it if it was originally on
(defvar was-in-erjoalgo-command-mode-before-minibuf)
(add-hook 'minibuffer-setup-hook
	  (lambda () (setq was-in-erjoalgo-command-mode-before-minibuf
			   erjoalgo-command-mode)
	    (global-erjoalgo-command-mode 0)))

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (when (eq was-in-erjoalgo-command-mode-before-minibuf t)
	      (global-erjoalgo-command-mode 1))))

(global-erjoalgo-command-mode 1)

(defvar erjoalgo-command-mode-keep-state nil)

(defadvice recursive-edit (around tmp-disable-command-mode activate)
  (if (and erjoalgo-command-mode
           (not erjoalgo-command-mode-keep-state)
           (not (member major-mode '(sldb-mode))))
      (progn
        ;; (message "disabling erjoalgo mode %s" major-mode)
        (global-erjoalgo-command-mode 0)
        ad-do-it
        (global-erjoalgo-command-mode 1))
    ad-do-it))

(require 'command-mode-commands);;maybe should not be separate package
(provide 'erjoalgo-command-mode)
;;; erjoalgo-command-mode.el ends here
