;;; erjoalgo-command-mode.el --- like vi's concept of command mode

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
;; Ctrl-prefixed and other common commands in Emacs
;; f1 to toggle mode on/off,
;; clear visual indicator when mode is on
;; Most of the keybindings in this mode are obtained
;; by removing the Ctrl or Ctrl+x prefix of Emacs
;;

;;; Code:

(require 'cl-lib)
(require 'buttons)
(require 'f)
(eval-when-compile (require 'subr-x))

(defvar erjoalgo-command-mode-map (make-sparse-keymap))

;;sub maps
(defvar open-interpreter-map (make-sparse-keymap))

(define-minor-mode erjoalgo-command-mode
    "command mode"
    :init-value 0 :lighter "-CM" :keymap erjoalgo-command-mode-map)

(define-globalized-minor-mode global-erjoalgo-command-mode
  erjoalgo-command-mode erjoalgo-command-mode)

(defvar *erjoalgo-command-mode-color-on* "dark green")
(defvar *erjoalgo-command-mode-color-off* "dark gray")

(defun erjoalgo-command-mode-hook-add-color ()
  "Add a visual indicator of current mode."
  (let ((color (if erjoalgo-command-mode
		   *erjoalgo-command-mode-color-on*
		 *erjoalgo-command-mode-color-off*)))
    (set-face-background 'mode-line color)))

(add-hook 'erjoalgo-command-mode-hook 'erjoalgo-command-mode-hook-add-color)
(add-hook 'global-erjoalgo-command-mode-hook 'erjoalgo-command-mode-hook-add-color)

(defun global-erjoalgo-command-mode-toggle ()
  "Toggle global-erjoalgo-command-mode."
  (interactive)
  (setf prefix-arg current-prefix-arg)
  (global-erjoalgo-command-mode (if erjoalgo-command-mode 0 1)))



(defun erjoalgo-command-mode-meta-pn ()
  "Action to take on ‘M-n', ‘M-p' bindings when on erjoalgo-command-mode."
  (interactive)
  (if (member major-mode '(slime-repl-mode inferior-emacs-lisp-mode))
      (call-interactively (lookup-key
                           (symbol-value (intern (format "%s-map" major-mode)))
                           (this-command-keys-vector)))
    (call-interactively
     (if (equal (key-description (this-command-keys-vector)) "M-n")
         'move-line-down 'move-line-up))))

(defun my-tab-command ()
  "Context-sensitive tab command."
  (interactive)
  (call-interactively
   (cl-case major-mode
     ((Custom-mode) 'widget-forward)
     ((help-mode apropos-mode debugger-mode) 'forward-button)
     ((Info-mode) 'Info-next-reference)
     ((gnus-article-mode) 'widget-forward)
     (t 'indent-for-tab-command))))

(defun my-indent ()
  "Context aware indent."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (if (at-end-of-sexp)
	(indent-last-sexp)
      (indent-according-to-mode))))

(defun at-end-of-sexp ()
  "Determine if we are at the end of a sexp."
  (condition-case ex
      (save-excursion
	(= (point)
	   (progn (backward-sexp 1)
		  (forward-sexp 1)
		  (point))))
    ('error nil)))

(defun indent-last-sexp ()
  "Indent the last sexp."
  (interactive)
  (let ((b (point))
	(a (save-excursion (progn
			     (backward-sexp 1)
			     (point)))))
    (indent-region a b)))

(defvar emacs-top
  (file-name-directory (file-truename user-init-file))
  "Find the LISP src directory.")

(defmacro cmd-find-most-recent-file-in-directory (name directories)
  "Define a command NAME to find the nth file in DIRECTORIES."
  (declare (indent 1))
  `(defun ,name (&optional nth kill-only)
     ,(format "find the last file in %s" directories)
     (interactive "P")
     (when nth (cl-assert (> nth 0)))
     (let ((file
     (-> (mapcar #'expand-file-name ,directories)
       (most-recent-file-name-in-directories (when nth (1- nth))))))
       (kill-new file)
       (message "killed %s" file)
       (unless kill-only
         (find-file file))
       file)))

(cmd-find-most-recent-file-in-directory find-last-download '("~/Downloads"))
(cmd-find-most-recent-file-in-directory find-last-download-or-scrot
  '("~/Downloads" "~/pictures/auto-scrots"))

(defalias #'sort-by #'sort-key)

(defvar compilation-contents-regexp-filter nil
  "If nonnil, skip compilation buffers whose contents don't match this regexp.")

(defun set-compilation-contents-regexp-filter (regexp)
  (interactive "sEnter compilation buffer contents regexp: ")
  (setq compilation-contents-regexp-filter (unless (s-blank? regexp) regexp)))

(defun switch-to-nth-most-recent-buffer (buffer-regexp offset)
  "Switch to the OFFSET most recent buffer matching BUFFER-REGEXP.

  OFFSET is relative to the current buffer If it matches BUFFER-REGEXP."
  (interactive "P")
  (cl-assert (or (null offset) (> offset 0)))
  (let* ((buffers
          (sort-by
           (cl-remove-if-not (lambda (buffer)
                               (and
                                (if-let ((buffer-name (buffer-name buffer)))
                                   (string-match buffer-regexp buffer-name))
                                (or (null compilation-contents-regexp-filter)
                                    (s-blank? compilation-contents-regexp-filter)
                                    (save-match-data
                                      (string-match compilation-contents-regexp-filter
                                                    (with-current-buffer buffer
                                                      (buffer-string)))))))
                             (buffer-list))
           (lambda (buffer)
             (if-let ((name (buffer-name buffer))
                      ((string-match "<\\([0-9]+\\)>$" name)))
                 (-> (match-string 1 name) string-to-number)
               (max-char)))
           :descending t))
         (offset (or offset 1))
         (start (or (cl-position (current-buffer) buffers) -1))
         (idx (if (zerop (length buffers))
                  (user-error "no more compilation buffers")
                (mod (+ start offset) (length buffers)))))
    (message "DEBUG buffers (in cmd-switch...): %s"buffers)
    (switch-to-buffer (nth idx buffers))))

(defun kill-buffer--maybe-switch-to-next-compilation (orig buffer)
  (let ((name (and (bufferp buffer) (buffer-name buffer))))
    ;; capture buffer name before it is killed
    (funcall orig buffer)
    (let ((compilation-regexp "^[*]compilation[*]"))
      (when name
        (cond
         ((string-match-p compilation-regexp name)
          (switch-to-nth-most-recent-buffer compilation-regexp nil))
         ((string-match-p "^[*]Async Shell Command[*].*" name)
          (next-async-shell-command-buffer)))))))

(advice-add #'kill-buffer :around
            #'kill-buffer--maybe-switch-to-next-compilation)

(defun kill-buffer--maybe-kill-compilation-process (buffer)
  "Advice to maybe kill the underyling process of a compilation BUFFER."
  (when-let* ((buffer (or buffer (current-buffer)))
              (regexp "^[*]compilation[*]")
              (name (and (bufferp buffer)
                         (buffer-name buffer)))
              (_is-compilation (string-match-p regexp name))
              (proc (get-buffer-process buffer))
              (_proc-live (process-live-p proc)))
    (kill-process proc)))

(advice-add #'kill-buffer :before
            #'kill-buffer--maybe-kill-compilation-process)

(defvar inferior-sql-mode-providers nil
  "Functions that may be called to provide a *SQL* eval buffer.")

(defmacro switch-to-buff-or-else-command
    (buff-spec &optional on-nonexistent)
  (let ((buff-sym (gensym "buff-")))
    `(buttons-defcmd
      (let ((,buff-sym (buffer-matching ,buff-spec)))
        (or (when ,buff-sym (switch-to-buffer ,buff-sym))
            ,on-nonexistent
            (error (format "no such buffer: %s" ,buff-spec)))))))

(defun markdown-indent-code-by-4-spaces (a b)
  "Indent code in region (A, B) by 4 spaces."
  (interactive "r")
  (when (and a (= a b)) (setq a nil b nil))
  (let ((a (or a (point-min)))
        (b (or b (point-max)))
        (rep (if google3-mode "  " "    ")))
    (save-match-data
      (replace-regexp
       "^" rep nil a b))))

(defun find-file-or-url-at-point ()
  "FFAP."
  (interactive)
  (if (or (thing-at-point 'url)
          (let ((text (prin1-to-string (sexp-at-point))))
            (when text
              (s-matches-p "^\\(b\\|cl\\|go\\)/" text))))
      (call-interactively #'browse-url-at-point)
    (call-interactively #'find-file-at-point)))

(defun next-async-shell-command-buffer()
  "Open the next async shell command buffer."
  (let* ((buffer-spec
          "regexp:[*]Async Shell Command[*]\\(<[0-9]+>\\)?")
         (buffer (buffer-matching buffer-spec)))
    (if (not buffer)
        (error (format "no such buffer: %s" buffer-spec))
      (switch-to-buffer buffer)
      (let ((proc (get-buffer-process buffer)))
        (if proc
            (message "cmd: %s" (process-command proc))
          (message "buffer has no process: %s" buffer))))))

(buttons-macrolet
 ((dir (dir) `(read-file-name "select file: " ,dir))
  (buff (buff-spec &optional on-nonexistent)
        `(switch-to-buff-or-else-command
          ,buff-spec ,on-nonexistent))
  (file (file) `(cmd (find-file ,file))))
 (defbuttons
   erjoalgo-command-mode-buttons
   nil
   (erjoalgo-command-mode-map)
   (buttons-make
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
    ("j" (cmd (dotimes (_ (or current-prefix-arg 1))
                (newline-and-indent))));;originally C-j
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
    ((kbd "M-1") #'sticky-window-delete-other-windows);;originally C-x 1
    ((kbd "M-2") 'my-split-window-below);;originally C-x 2
    ((kbd "M-3") 'my-split-window-right);;originally C-x 3
    ((kbd "M-q") #'sticky-window-delete-window);;originally C-x 0
    ([f2] 'other-window);;originally C-x o
    ((kbd "<S-f2>") (cmd (other-window -1)));;originally C-x o
    ("c" 'cycle-buffer);; cycle buffers
    ("C" 'duplicate-current-buffer);; open current buffer in split-window-below
    ("z" (cmd (kill-buffer (current-buffer))));;originally C-x k
    ("Z" 'new-buffer-focus)
    ("q" 'bury-buffer);; move current buffer to end of the list
    ("3" 'find-file-or-url-at-point);;originally C-x f
    ("4" 'switch-to-buffer);;originally C-x b
    ("5" (buttons-make
          ("g" (cmd (dir "~/git")))
          ("l" (cmd (dir "~/private-data/leet/")))
          ("G" (cmd
                (find-file "~/git/google-utils")))))
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
    ("	" 'my-tab-command)
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
           (set-transient-map 'help-command)
           (unless (or arg
                       global-erjoalgo-command-mode)
             (global-erjoalgo-command-mode 0))))
    ;; ([f1] nil);; f1 toggle command mode
    ([f1] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ([f13] 'global-erjoalgo-command-mode-toggle);; f13 toggle command mode
    ([s-f11] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ([ë] 'global-erjoalgo-command-mode-toggle);; f1 toggle command mode
    ("y" 'stumpwm-search-engine-search-clipboard)
    ("Y" 'stumpwm-search-engine-search)
    ("J" (lambda (arg)
           (interactive "P")
           (cl-loop for _ below (or arg 1)
                    do (join-line '(4)))))
    ((kbd "s->") 'markdown-indent-code-by-4-spaces)
    ("m"
     (buttons-make
      ("e" (file (file-truename "~/.emacs")))
      ("C" (buff "regexp:[*]ansi-term[*].*" (ansi-term "/bin/bash")))
      ("c" (lambda (arg) (interactive "P")
             (switch-to-nth-most-recent-buffer "^[*]compilation[*]" arg)))
      ("r" (buff "*Backtrace*"))
      ("b" (file "~/.bashrc"))
      ("a" (file "~/.bash_aliases"))
      ("A" (file "~/.bash-fns/.my-bash-fns"))
      ("m" (buff "*Messages*"))
      ("s" (buff "*Org Agenda*" (org-todo-list)))
      ("S" (buff "*Org Agenda*"))
      ("t" (file (f-join emacs-top "settings" "buttons-data.el")))
      ((kbd "s-t") (file (f-join emacs-top "core" "erjoalgo-command-mode.el")))
      ((kbd "s-c") (file (f-join emacs-top "core" "autobuild.el")))
      ((kbd "s-C") (file (f-join emacs-top "core" "autobuild-examples.el")))
      ("M" (file (concat "/var/mail/" (getenv "USER"))))
      ("x" (file (concat  "~/.stumpwmrc.d/inits/.xmodmap/")))
      ("l" (buff "regexp:[*]inferior-lisp[*]"))
      ("L" (buff "regexp:[*]sldb sbcl.*[*]"))
      ("v" (file "~/.stumpwmrc.d/keynavs/.keynavrc"))
      ("w" (file "~/.stumpwmrc.d/lisp/.stumpwmrc"))
      ("n" (file "~/.stumpwmrc.d/scripts/bin"))
      ("W" (buff "*Warnings*"))
      ("o" (file "~/private-data/org/master.org"))
      ("T" (cmd (org-todo-list org-match)))
      ("O" nil)
      ("j" (buff "*-jabber-roster-*"))
      ("d"
       (but
        ("f" #'find-last-download)

        ("k" (cmd (find-last-download nil t)))
        ("m" (cmd
              (let*
                  ((last-download (find-last-download nil t))
                   (dest (f-join default-directory (f-filename last-download))))
                (rename-file last-download dest)
                (kill-new dest))))))
      ("p" 'project-open)))
    ("x"
     (buttons-make
      ("s" (lambda (arg)(interactive "P")(eshell arg)))
      ("p"  (buff "*Python*" (call-interactively 'run-python)))
      ("P" 'message-current-buffer-process)
      ("i" 'ielm)
      ("I" 'load-dark-theme-toggle)
      ("c" 'autobuild-rebuild-recent)
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
      ("d"
       (but
        ("e" #'toggle-debug-on-error)
        ("q" #'toggle-debug-on-quit)))
      ("g" 'grep-recursive)
      ("G" 'replace-regexp-dir)
      ("f" 'find-iregex)
      ("u" 'universal-argument)
      ([f2] 'call-last-kbd-macro)
      ("W" (buttons-make
            ("1" 'slime-sbcl)
            ("2" 'slime-stumpwm)
            ("3" 'cider-buffer-or-jack-in)))
      ("w" (switch-to-buff-or-else-command
            "regexp:[*]slime-repl sbcl\\|[*]cider-repl"))
      ("r" 'replace-regexp)
      ("R" 'query-replace-regexp)
      ("A" (cmd (next-async-shell-command-buffer)))
      ("o" 'gnus-goto-inbox)
      ("0" 'open-google-calendar)
      ("b" (buff "*Inferior Octave*" (inferior-octave t)))
      ("3" (buff "*eww*" (call-interactively 'eww)))
      ("q" (buff "regexp:[*]SQL.*[*]"
                 (call-interactively
                  (selcand-select inferior-sql-mode-providers nil nil t))))
      ("m" (cmd (call-interactively 'sip-chat-menu))))))))

(defun buffer-matching (string &optional regexp-p)
  "Find buffers matching STRING, interpreted as a regexp when REGEXP-P."
  (let ((prefix "regexp:"))
    (when (s-starts-with-p prefix string)
      (setf regexp-p t
	    string (substring string (length prefix))))

    (if (not regexp-p) (get-buffer string)
      (cl-loop with matching
            for buff in (buffer-list)
            as name = (buffer-name buff)
            if (string-match string name)
            collect name into matching
            finally
            (return (let ((sorted (sort matching #'string-lessp)))
                      (car sorted)))))))

(defun force-mode-first (mode-symbol &optional kmap)
  "Try to ensure that my keybindings have priority over the newly-loaded MODE-SYMBOL.

  KMAP defaults to the mode's keymap"
  ;;http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797#5340797
  (cl-loop
   with kmap = (or kmap
                   (let ((kmap-sym (->> mode-symbol
                                     (format "%s-map")
                                     intern)))
                     (cl-assert (boundp kmap-sym))
                     (symbol-value kmap-sym)))
   with entry = (cons mode-symbol kmap)
   for alist-var in '(minor-mode-map-alist minor-mode-overriding-map-alist)
   as val = (symbol-value alist-var)
   unless (eq (caar val) mode-symbol)
   do
   (progn
     (assq-delete-all mode-symbol val)
     (add-to-list alist-var entry))))

(defun find-symbol (symbol)
  "Find the source of SYMBOL.  Similar to ‘find-function'."
  (interactive (list (or (symbol-at-point)
                         (read-symbol-completing "enter symbol: "))))
  (find-file (replace-regexp-in-string
              "[.]elc$" ".el"
              (symbol-file symbol))))

(add-hook 'after-load-functions
	  #'(lambda (_something)
	     (force-mode-first 'erjoalgo-command-mode)))

(buttons-macrolet
 ()
 ;; suppress warning about overriding target keymap
 (define-key global-map [f1] nil)
 (define-key global-map [f2] nil)
 (define-key global-map [f13] nil)
 (defbuttons command-mode-global-buttons
   nil
   (global-map)
   (but
    ([f1] 'global-erjoalgo-command-mode-toggle)
    ([f13] 'global-erjoalgo-command-mode-toggle)
    ((kbd "<s-f11>") 'global-erjoalgo-command-mode-toggle)
    ((kbd "ë") 'global-erjoalgo-command-mode-toggle)
    ([f2] 'other-window)
    ((kbd "<s-f12>") (lambda () (interactive)
                                  (save-buffer)
                                  (erjoalgo-command-mode 1)))
    ([M-f1] 'goto-last-change)
    ([f4] 'keyboard-escape-quit)
    ((kbd "s-`") 'exit-recursive-edit)
    ((kbd "s-~") 'buttons-abort-cmd)
    ((kbd "s-SPC") (cmd (ins ", ")))
    ((kbd "<C-f11>") 'eval-buffer)
    ((kbd "M-SPC") (lambda (arg) (interactive "P")
                     (upcase-last (not arg)) (insert " ")))
    ((kbd "<backtab>") 'my-indent)))

 (defbuttons help-buttons nil
   (help-map)
   (but
    ;;find source for function-at-point
    ("y" 'find-function)
    ("Y" 'find-symbol)
    ;;apropos
    ("A" 'apropos-variable)
    ;; disable annoying tutorial
    ((kbd "t") nil)
    ;; disable accidentally entering h h
    ((kbd "h") nil))))

(defun apropos-mode-hook-auto-switch-windows (&rest args)
  (if-let ((apropos-win
            (or (get-buffer-window "*Apropos*")
                (get-buffer-window "*slime-apropos*"))))
      (select-window apropos-win)))

(add-hook 'apropos-mode-hook
          #'apropos-mode-hook-auto-switch-windows)

(defun one-char-insert-mode (&optional _arg)
  "Insert the next char as text."
  (interactive "P")
  (set-transient-map global-map))


(let ((kmap (make-sparse-keymap)))
  (cl-loop
   for i from 1 to 9 do
   (define-key kmap (kbd (format "<s-f%d>" i))
     `(lambda () (interactive)
        (setf prefix-arg ,i)))
   finally (return kmap))
  (buttons-define-keymap-onto-keymap kmap erjoalgo-command-mode-map)
  (buttons-define-keymap-onto-keymap kmap global-map))


;;this is actually part of erjoalgo-command-mode
;;make mode-line text very big and easy to read
(progn
  (face-spec-set 'mode-line
                 '((t (:background "dark gray"
			           :foreground "white"
			           :box (:line-width -1 :style released-button)
			           :weight normal
			           :height 2.0
			           :width extra-expanded))))
  (face-spec-set 'mode-line-inactive
                 '((t (:inherit mode-line
				:background "grey90"
				:foreground "grey20"
				:box (:line-width -1 :color "grey75")
				:weight light
				:height 1.1
				:width normal))))
  (face-spec-set 'region '((t :background "#666" :foreground "#ffffff"))))

(defun load-dark-theme-toggle ()
  "Toggle dark background theme."
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
  "Auto-disable command mode within a recursive edit."
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
