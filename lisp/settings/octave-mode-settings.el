(push '("[.]m$" . octave-mode) auto-mode-alist)
(add-hook 'octave-mode-hook 'octave_install_buttons)
(add-hook 'inferior-octave-mode-hook 'octave_install_buttons)
