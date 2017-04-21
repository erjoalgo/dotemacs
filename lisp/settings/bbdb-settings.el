(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-insinuate-message)

(bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-update-records-p 'create)

(add-hook 'message-setup-hook 'bbdb-mail-aliases)
(setf bbdb-silent t)
(setf bbdb-allow-duplicates t)
