(defcustom erc-notification-match-types '(current-nick)
  "Types of matches to notify when a match occurs.
The function `erc-notifications-notiy-on-match'
needs to be added to `erc-text-matched-hook' for notifications to work."
  :group 'erc-match
  :type '(choice (repeat :tag "Notify on match" (choice
						 (const current-nick)
						 (const keyword)
						 (const pal)
						 (const dangerous-host)
						 (const fool)))
		 (const :tag "Don't notify" nil)))

(defun erc-notifications-notify-on-match (match-type nickuserhost msg)
  (when (member match-type erc-notification-match-types)
    (let ((nick (nth 0 (erc-parse-user nickuserhost))))
      (unless (or (string-match-p "^Server:" nick)
                  (when (boundp 'erc-track-exclude)
                    (member nick erc-track-exclude)))
        (erc-notifications-notify nick msg)))))
