(require 'autobuild)

(add-hook 'c-mode-hook (lambda () (setf comment-start "// ")))

(autobuild-defvar-file-local c-ofast-compilation nil (y-or-n-p "enable -Ofast? "))

(setq-default c-basic-offset 4)
