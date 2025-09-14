(defun stats-add-note (scrot-filename note-title notes-directory)
  (interactive
   (let* (
          (scrot-filename
           (find-most-recent-file
            '("~/pictures/auto-scrots") nil nil
            hidden-files-regexp))
          (note-title (read-string (format
                                    "enter name for note (%s): "
                                    scrot-filename)))
          (notes-directory (expand-file-name "~/git/18.6501x/notes/")))
     (list scrot-filename note-title notes-directory)))

  (let* ((ext (f-ext scrot-filename))
         (f-name (f-filename scrot-filename))
         (new-name (f-join notes-directory
                           (format "%s.%s" note-title ext))))
    (message "moving %s to %s" scrot-filename new-name)
    (rename-file scrot-filename new-name)))
