(defvar linphonerc (expand-file-name "~/.linphonerc"))

(defun sip-linphonerc-next-proxy-number ()
  (with-current-buffer (find-file-noselect linphonerc)
    (goto-char (point-min))
    (1+
     (or
      (cl-loop
       while
       (re-search-forward "^[[]\\(proxy_\\|auth_info_\\)\\([0-9]+\\)" nil t)
       as proxy-number = (string-to-number (match-string 2))
       maximize proxy-number)
      -1))))

(defmacro template-replace (tmpl)
  `(save-match-data
     (->> ,tmpl
       ,@(cl-loop
          with var-names
          with start = 0
          while (string-match "[$]{\\([a-z-]+\\)}"
                              tmpl start)
          as var-name = (match-string 1 tmpl)
          unless (member var-name var-names)
          collect
          `(replace-regexp-in-string
            ,(format "[$]{%s}" var-name)
            ,(intern var-name)
            ;; t t
            )
          do (setq start (match-end 0))
          do (push var-name var-names)))))

(defun sip-linphone-add-proxy (hostname username password realm)
  (interactive "senter sip hostname (e.g. sanjose2.voip.ms): \nsenter username: \nsenter password: \nsenter realm(sanjose.voip.ms): ")
  (let* ((proxy-number (number-to-string
                        (sip-linphonerc-next-proxy-number)))
         (proxy-and-authinfo
          (template-replace
           "[proxy_${proxy-number}]
reg_proxy=<sip:${hostname}>
reg_identity=sip:${username}@${hostname}
reg_expires=3600
reg_sendregister=1
publish=0
dial_escape_plus=0

[auth_info_${proxy-number}]
username=${username}
userid=${username}
passwd=${password}
realm=\"${realm}\"
")))
    (with-current-buffer (find-file-noselect linphonerc)
      (goto-char (point-max))
      (newline)
      (insert proxy-and-authinfo)
      (save-buffer)
      (async-shell-command "linphone-restart.sh"))))
