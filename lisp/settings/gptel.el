;; -*- lexical-binding: t; -*-

(dolist (cmd '(gptel gptel-send gptel-menu))
  (fset cmd
        (lambda (&rest args)
          (interactive)
          (ensure-packages-exist '(gptel))
          (require 'gptel)
          ;; `cmd' has now been redefined by gptel.el itself
          (if (called-interactively-p 'any)
              (call-interactively cmd)
            (apply cmd args)))))

(with-eval-after-load 'gptel
  (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'gptel-api-key)
  (setq gptel-model 'claude-sonnet-4-6))
