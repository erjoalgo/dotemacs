;; Debugger entered--Lisp error: (error "ediff-copy-both-to-C: Bad diff region number, 5.  Valid numbers are 1 to 4")
;;   signal(error ("ediff-copy-both-to-C: Bad diff region number, 5.  Valid numbers are 1 to 4"))
;;   error("%S: Bad diff region number, %d.  Valid numbers are 1 to %d" ediff-copy-both-to-C 5 4)
;;   ediff-get-diff-posn(A beg 4 #<buffer *Ediff Control Panel<2>*>)
;;   ediff-get-region-contents(4 A #<buffer *Ediff Control Panel<2>*>)

(defun ediff-copy-both-to-C ()
  ;; taken from
  ;; https://stackoverflow.com/questions/9656311/
  (interactive)
  (message "on ediff-copy-both-to-C")
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
