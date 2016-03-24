
(require 'f)

(let* ((GOPATH (getenv "GOPATH"))
      (go-lint-path (f-join GOPATH "src/github.com/golang/lint/misc/emacs")))
  
  (if (not (and GOPATH (file-exists-p go-lint-path)))
      (message "WARNING: go or go lint not installed (go get github.com/golang/lint)")
    (progn 
      (add-to-list 'load-path go-lint-path)
      (setq gofmt-command (f-join GOPATH "bin/goimports"))
      (eval-after-load 'go-mode
	(lambda () 
	  (add-hook 'before-save-hook #'gofmt-before-save)
	  (require 'golint))))))

(add-to-list 'auto-mode-alist '("[.]go$" . go-mode))


