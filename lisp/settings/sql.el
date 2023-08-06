(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
