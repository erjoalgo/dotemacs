(require 'f)

(defun goimports-init ()
  (if-let ((GOPATH (getenv "GOPATH")))
    (let ((go-lint-path
	   (f-join GOPATH "src/github.com/golang/lint/misc/emacs"))
	  (goimports-path (f-join GOPATH "bin/goimports")))

      (if (not (file-exists-p goimports-path))
	  (warn (concat "WARNING: goimports not found "
                        "go install golang.org/x/tools/cmd/goimports@latest"))
	(setf gofmt-command goimports-path)
	(add-hook 'before-save-hook #'gofmt-before-save)))))

(add-hook 'go-mode-hook #'goimports-init)

;(add-to-list 'auto-mode-alist '("[.]go$" . go-mode))
;(autoload 'go-mode "go-mode" "go mode" t)
;(require 'go-mode-autoloads)
