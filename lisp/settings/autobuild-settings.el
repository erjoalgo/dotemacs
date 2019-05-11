(autobuild-define-rule autobuild-pdf-zathura (fundamental-mode doc-view-mode)
  (autobuild-nice 9)
  (when (and buffer-file-name (equal "pdf" (f-ext buffer-file-name)))
    (format "zathura ./%s"
            (-> buffer-file-name
              f-filename
              shell-quote-argument))))

(autobuild-define-rule autobuild-eog (image-mode)
  (autobuild-nice 9)
  (format "eog %s" (shell-quote-argument buffer-file-name)))
