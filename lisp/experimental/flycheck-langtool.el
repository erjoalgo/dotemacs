(require 'flycheck)

(with-eval-after-load "flycheck"
  (defcustom flycheck-langtool-jar-path
    (car (f-glob "~/src/LanguageTool-*/languagetool-commandline.jar"))
    "Path to the languagetool-commandline.jar file"
    :type 'string
    :group 'flycheck-langtool)

  (defvar flycheck-langtool-lang "es"
    "Source language code for the current buffer")
  (make-variable-buffer-local 'flycheck-langtool-lang)

  ;; 107.) Line 85, column 712, Rule ID: MORFOLOGIK_RULE_ES
  ;; Message: Hallado un posible error de ortografía
  ;; Suggestion: Sonsón
  ;; ...al de Kennedy y del principio del mandato de Johnson había perecido.

  '(flycheck-define-checker ada-gnat
                            "An Ada syntax checker using GNAT.

Uses the GNAT compiler from GCC.  See URL
`https://www.adacore.com/community/'."
                            :command ("gnatmake"
                                      "-c"                        ; Just compile, don't bind
                                      "-f"                        ; Force re-compilation
                                      "-u"                        ; Compile the main file only
                                      "-gnatf"                    ; Full error information
                                      "-gnatef"                   ; Full source file name
                                      "-D" temporary-directory
                                      (option-list "-gnat" flycheck-gnat-warnings concat)
                                      (option-list "-I" flycheck-gnat-include-path concat)
                                      (option "-gnat" flycheck-gnat-language-standard concat)
                                      (eval flycheck-gnat-args)
                                      source)
                            :error-patterns
                            ((error line-start
                                    (message "In file included from") " " (file-name) ":" line ":"
                                    column ":"
                                    line-end)
                             (info line-start (file-name) ":" line ":" column
                                   ": note: " (message) line-end)
                             (warning line-start (file-name) ":" line ":" column
                                      ": warning: " (message) line-end)
                             ;; no specific error prefix in Ada
                             (error line-start (file-name) ":" line ":" column
                                    ": " (message) line-end))
                            :modes ada-mode)

  (flycheck-define-checker langtool2
                           "Flycheck for languagetool."
                           :command
                           ("java" "-jar" (eval flycheck-langtool-jar-path)
                            "-l" (eval flycheck-langtool-lang) source)
                           :error-patterns
                           ((error line-start (one-or-more digit) ".)" line "," column
                                   ", Rule ID:" (message) line-end))
                           :modes (translation-mode text-mode)))
