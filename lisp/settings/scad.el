(defvar-local compilation-sentinel nil
  "If bound on a specific compilation buffer, this function is invoked with
   the same arguments as those to ``COMPILATION-FINISH-FUNCTIONS''.")


(defun compilation-maybe-run-sentinel (compilation status)
  (when (bound-and-true-p compilation-sentinel)
    (funcall compilation-sentinel compilation status)))

(add-hook 'compilation-finish-functions #'compilation-maybe-run-sentinel)

(defun scad-compilation-sentinel (compilation status)
  (cond
   ((s-contains-p "finished" status)
    ;; hide the compilation buffer on success
    (bury-buffer compilation)
    (delete-window (get-buffer-window compilation 'visible)))

   ((s-contains-p "exited abnormally" status)
    t)

   (t (error "unable to interpret compilation exit status: %s" status))))

(defun scad-compile-on-save ()
  (when (eq major-mode 'scad-mode)
    (let ((compilation (funcall (funcall #'autobuild-openscad))))
      ;; TODO race condition?
      (with-current-buffer compilation
        (setf compilation-sentinel #'scad-compilation-sentinel)))))

(add-hook 'after-save-hook #'scad-compile-on-save)
