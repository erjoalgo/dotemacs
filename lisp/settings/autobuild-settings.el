(autobuild-define-rule autobuild-pdf-zathura (fundamental-mode)
  (autobuild-nice 9)
  (when (and buffer-file-name (f-ext buffer-file-name))
    (format "zathura ./%s"
            (-> buffer-file-name
              f-filename
              shell-quote-argument))))
