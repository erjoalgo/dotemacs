(defvar linguee-program
  "/home/ealfonso/git/dotemacs/linguee.js")

(defun linguee-query (text from to)
  (with-current-buffer
      (get-buffer-create "*linguee-process-buffer*")
    ;; (erase-buffer)
    (goto-char (point-max))
    (save-excursion
      (call-process linguee-program
                    nil
                    t
                    nil
                    text from to))
    (json-read)))

(defun linguee-extract-translations (response)
  (message "value of response: %s" response)
  (let ((words (linguee-json-get '(words) response)))
    (unless (zerop (length words))
      (mapcar (apply-partially #'linguee-json-get '(term))
              (linguee-json-get '(0 translations) words)))))


(defun linguee-json-get (path json)
  "Traverse a json object JSON along PATH."
  (reduce (lambda (obj prop)
            (cond ((symbolp prop) (cdr (assoc prop obj)))
                  ((numberp prop) (when obj (aref obj prop)))
                  (t (error "Type not suppored: %s" prop))))
          path
          :initial-value json))

(defvar linguee-from "eng")
(defvar linguee-to "spa")

(defun linguee-translate (query)
  (interactive (list (read-string "enter query: " )))
  (let* ((alternatives (->
                        (linguee-query
                         query
                         linguee-from
                         linguee-to)
                        linguee-extract-translations))
         (choice (selcand-select
                  alternatives
                  (format "select replacement for %s: " query))))
    (if (region-active-p)
        (save-excursion
          (delete-region
           (region-beginning)
           (region-end))
          (goto-char (region-beginning))
          (insert choice))
      (progn
        (message "killing: %s" choice)
        (kill-new choice)))))
