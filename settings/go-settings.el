(require 'golint)
(require 'f)

(setq gofmt-command (f-join (getenv "GOPATH") "bin/goimports"))
(add-hook 'before-save-hook #'gofmt-before-save)
