
(defvar ispell-skip-prompt-on-quit t)

(defun ispell-maybe-skip-prompt-on-quit (oldfn prompt)
  (if (and ispell-skip-prompt-on-quit
           (equal prompt "Really kill Ispell process? "))
      t
    (funcall oldfn prompt)))

(advice-add 'y-or-n-p :around #'ispell-maybe-skip-prompt-on-quit)
