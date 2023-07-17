(defun tmux-set-clipboard (text)
  (start-process "tmux-set-clipboard"
                 "*tmux-set-clipboard*"
                 "tmux" "set-buffer" text))

(advice-add #'stumpwm-clipboard-set :after #'tmux-set-clipboard)
