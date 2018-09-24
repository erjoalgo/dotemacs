(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (if (memq (process-status proc) '(exit signal))
      (let ((buffer (process-buffer proc)))
	(if (null (buffer-name buffer))
	    ;; buffer killed
	    (set-process-buffer proc nil)
	  (with-current-buffer buffer
	    ;; Write something in the compilation buffer
	    ;; and hack its mode line.
            (safe-wrap
	     (compilation-handle-exit (process-status proc)
				      (process-exit-status proc)
				      msg))
	    ;; Since the buffer and mode line will show that the
	    ;; process is dead, we can delete it now.  Otherwise it
	    ;; will stay around until M-x list-processes.
	    (delete-process proc)))
	(setq compilation-in-progress (delq proc compilation-in-progress)))))
