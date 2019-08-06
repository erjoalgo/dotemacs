(defun replace-highlight (match-beg match-end range-beg range-end
                                    search-string regexp-flag delimited-flag
                                    case-fold &optional backward)
  (if query-replace-highlight
      (if replace-overlay
          (move-overlay replace-overlay match-beg match-end (current-buffer))
        (setq replace-overlay (make-overlay match-beg match-end))
        (overlay-put replace-overlay 'priority 1001) ;higher than lazy overlays
        (overlay-put replace-overlay 'face 'query-replace)))
  (if query-replace-lazy-highlight
      (let ((isearch-string search-string)
            (isearch-regexp regexp-flag)
            (isearch-regexp-function delimited-flag)
            (isearch-lax-whitespace
             replace-lax-whitespace)
            (isearch-regexp-lax-whitespace
             replace-regexp-lax-whitespace)
            (isearch-case-fold-search case-fold)
            (isearch-forward (not backward))
            (isearch-other-end match-beg)
            (isearch-error nil))
        (save-match-data (isearch-lazy-highlight-new-loop range-beg range-end)))))
