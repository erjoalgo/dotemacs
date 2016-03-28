
(require 'f)

(let ((GOPATH (getenv "GOPATH")))
  (when GOPATH
    (let ((go-lint-path (f-join GOPATH "src/github.com/golang/lint/misc/emacs"))
	  (goimports-path (f-join GOPATH "bin/goimports")))
      
      (if (not (file-exists-p go-lint-path))
	  (message "WARNING: go or go lint not installed (go get github.com/golang/lint)")
	(add-to-list 'load-path go-lint-path))
      
      (if (not (file-exists-p goimports-path))
	  (message "WARNING: go or go lint not installed (go get github.com/golang/lint)")
	(setf gofmt-command goimports-path)))))

(add-to-list 'auto-mode-alist '("[.]go$" . go-mode))
