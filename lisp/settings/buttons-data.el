(defun super-add (key-spec)
  "if ‘key-spec' is a string, then prefix it with super
otherwise, leave it intact"
  (if (not (stringp key-spec))
      key-spec
    (if (equal key-spec "\\")
        (kbd "s-\\")
      (let ((fmt
             (if (equal (length key-spec) 1) "(kbd \"s-%s\")"
               "(kbd \"<s-%s>\")")))
        (eval (read (format fmt key-spec)))))))

(buttons-macrolet
 ((inm () `(global-erjoalgo-command-mode 0)))

(let-when-compile
     ((*buttons-make-key-mapper* 'super-add))

 (defbuttons programming-buttons nil nil
   (but
    ("\\" (cmd (ins "\\n")))
    ("%" (cmd (ins "%d")))
    ("3" (cmd (ins "({})")))
    ("2" (cmd (ins "\"{}\"")))
    ("@" (cmd (ins "'{}'")))
    ("8" (cmd (ins "*")))
    ("9" (cmd (ins "[0-9]+")))
    ("e" (cmd (ins " = {(inm)}")))
    ("4" (cmd (ins "[{}]")))
    ("SPC" (cmd (ins ", {(inm)}")))
    ("5" (cmd (ins "%s")))
    ("%" (cmd (ins "%d")))
    ("=" (cmd (ins " == ")))
    ("+" (cmd (ins " != ")))
    ("6" (cmd (ins "[^{}]")))
    ((kbd "M-/") 'my-comment-out)
    ((kbd "M-?") 'my-comment-out-and-duplicate)
    ("." (but
          ("f" 'xref-find-definitions)
          ("s" 'xref-show-location-at-point)
          ("r" 'eglot-rename)
          ("x" 'xref-find-references)
          ("a" 'xref-find-apropos)
          ("c" 'eglot-code-actions)))
    ("u";; util
     (but
      ("c" ;; case
       (but
        ("u" 'upcase-region)
        ("d" 'downcase-region)))))
    ((kbd "M-.") (lambda () (interactive)
                   (if (get-buffer "*compilation*")
                       (call-interactively 'next-error)
                     (call-interactively 'flymake-goto-next-error))))
    ((kbd "M-,") (lambda () (interactive)
                   (if (get-buffer "*compilation*")
                       (call-interactively 'previous-error)
                     (call-interactively 'flymake-goto-prev-error))))
    ("R" (cmd (ins "***REMOVED***")))))

 (defbuttons python-buttons programming-buttons
   (python-mode-map)
   (but
    ("e"
     (lambda nil
       (interactive)
       (insert
        (if
            (looking-back "^[ 	]*[a-zA-Z_.,]+ *")
            " = " "="))))
    ("f" (cmd (ins "for {} in {}:{(nli)}{}{(nli)}{(idt)}")))
    ("F" (cmd (ins "[ {} for {} in {} ]")))
    ("w" (cmd (ins "while {}:{(nli)}{}{(nli)}{(idt)}")))
    ("T" (cmd (ins "try:{(nli)}{}{(nli)}{(idt)}except:{(nli)}")))
    ("z" (cmd (ins "if {}:{(nli)}")))
    ("x" (cmd (ins "elif {}:{(nli)}")))
    ("c" (cmd (ins "else:{(nli)}")))
    ("v" (cmd (ins " if {} else {}")))
    ("1" (cmd (ins "not ")))
    ("d" (cmd (ins "def {}({(inm)}{}):{(nli)}")))
    ("a" (cmd (ins "lambda {}: {(inm)}")))
    ("2" (cmd (ins "\"{}\"")))
    ("@" (cmd (ins "'{}'")))
    ("q"
     (but
      ("x" (cmd (ins "xrange({})")))))
    ("M" (cmd (ins "from {} import *")
              (cmt)
              (ins "{(nli)}")))
    ("N" (cmd (ins "print ( {} ){(nli)}")))
    ("n" (cmd (ins "print ({}){(nli)}")))
    ("r" (cmd (ins "return {}{(nli)}{(idt)}")))
    ("L" (cmd (ins "class {}(object):{(nli)}")))
    ("l" (cmd (ins "len({})")))
    ("'" (cmd (ins "\"\"\"{}\"\"\"{(nli)}")))
    ("W" (cmd (ins "with open({}, \"{}\") as fh:{(nli)}{}{(nli)}")))
    ("SPC" (cmd (ins ", ")))
    ("I" (cmd (ins "def __init__( self ):{(nli)}")))
    ("0" (cmd (insert python-argparse-template)))
    ("t" (cmd (ins "True")))
    ("g" (cmd (ins "False")))
    ("G" (cmd (ins "None")))
    ("." (cmd (ins "import pdb;pdb.set_trace(){(nli)}")))
    ("_" (cmd (ins "if __name__ == \"__main__\":{(nli)}")))
    ("=" (cmd (ins " == ")))
    ("j" (cmd (ins " or {(inm)}")))
    ("k" (cmd (ins " and {(inm)}")))
    (">" 'python-indent-shift-right)
    ("<" 'python-indent-shift-left)
    ("s" (cmd (ins "self{(inm)}")))
    ("h" (cmd (python-dir-on-expression)))
    ("H" (cmd (pdb-help-on-expression)))
    ("i" (cmd (ins " in ")))
    ("[" (cmd (insert "{")
              (ins "{}}")))
    ("[" (cmd (insert "{}")))
    ("]" (cmd (ins ".format({})")))
    ("N" (cmd (ins "a=[{}]{(nli)}print(getattr(Solution(), dir(Solution)[-1])(*a))")))
    ("E" (cmd (ins "raise Exception({})")))
    ("u"
     (but
      ("a" (cmd (ins "assert({})")))))))

 (defbuttons pdb-buttons python-buttons
   (inferior-python-mode-map)
   (but
    ("r" (cmd (cmt "restart")
              (pdb-restart)))
    ("c" (cmd (gud-cont nil)))
    ("d" (cmd (cmt "n")))
    ("x" (cmd (gud-break nil)))
    ("z" (cmd (gud-remove nil)))
    ("b" (cmd (cmt "b")))
    ("escape" (cmd (cmt "")))
    ("X" (cmd (cmt "exit")))))

 (defbuttons emacs-lisp-buttons programming-buttons
   (emacs-lisp-mode-map read-expression-map inferior-emacs-lisp-mode-map)
   (but
    ("d"
     (but
      ("v" (cmd (ins "(defvar {}){(nli)}")))
      ("f" (cmd (ins "(defun {} ({}){(nli)}{})")))
      ("m" (cmd (ins "(defmacro {} ({}){(nli)}{})")))
      ("s" (cmd (ins "(defstruct {}{(nli)}{})")))
      ("b" (cmd (ins "(destructuring-bind ({}){})")))
      ("B" (cmd (ins "(multiple-value-bind ({}){})")))
      ("k"
       (but
        ("p" (cmd (ins "(defpackage {}{(nli)}(:use :cl){})")))
        ("u" (cmd (ins "(:use :{})")))
        ("i" (cmd (ins "(in-package #:{})")))
        ("m" (cmd (ins "(:import-from #:{})")))
        ("e" (cmd (ins "(:export #:{})")))))))
    ("w" (cmd (ins "(while {}){(nli)}")))
    ("a" (cmd (ins "(lambda ({}) {})")))
    ("z" (cmd (ins "(if {})")))
    ("x" (cmd (ins "(when {})")))
    ("c" (cmd (ins "(unless {})")))
    ("v" (cmd (ins "(progn {})")))
    ("l"
     (but
      ("t" (cmd (ins "(let ({}){(nli)}{}){(nli)}")))
      ("T" (cmd (ins "(let* ({}){(nli)}{}){(nli)}")))
      ("x" (cmd (ins "(lexical-let ({}){(nli)}{}){(nli)}")))
      ("X" (cmd (ins "(lexical-let* ({}){(nli)}{}){(nli)}")))
      ("b" (cmd (ins "(labels ({}){(nli)}{}){(nli)}")))))
    ("e" (cmd (ins "(setf {})")))
    ("E" (cmd (forward-sexp)
              (my-eval-defun)))
    ("i" (cmd (ins "(interactive)")))
    ("7"
     (but
      ("r" (cmd (ins "&rest ")))
      ("k" (cmd (ins "&key ")))
      ("b" (cmd (ins "&body ")))
      ("o" (cmd (ins "&optional ")))))
    ("g" (cmd (ins "nil")))
    ("t"
     (but
      ("l"
       (but
        ("t" (cmd (ins "(list {})")))
        ("l" (cmd (ins "(length {})")))))
      ("1" (cmd (ins "(null {})")))
      ("m"
       (but
        ("c" (cmd (ins "(mapcar {}){(nli)}")))
        ("x" (cmd (ins "(macroexpand '{}){(nli)}")))))
      ("g" (cmd (ins "({0}-sym (gensym \"{0}\")){(nli)}")))
      ("e"
       (but
        ("u" (cmd (ins "(equal {})")))
        ("q" (cmd (ins "(eq {})")))
        ("=" (cmd (ins "(= {})")))
        ("l" (cmd (ins "(eql {})")))))
      ("f"
       (but
        ("r" (cmd (ins "(remove-if {})")))
        ("R" (cmd (ins "(remove-if-not {})")))))
      ("+" (cmd (ins "(1+ {})")))
      ("s" (cmd (ins "(subseq {})")))
      ("r" (cmd (ins "(return {})")))
      ("v" (cmd (ins "(reverse {})")))
      ("i" (cmd (ins "(insert {})")))
      ("b" (cmd (ins "(boundp {})")))
      ("n" (cmd (insert "~{~A~^")
                (ins "{}~}")))
      ("a" (cmd (ins "(assert {})")))
      ("p" (cmd (ins "(push {})")))
      ("c"
       (but
        ("d" (cmd (ins "(cdr {})")))
        ("a" (cmd (ins "(car {})")))
        ("n" (cmd (ins "(cons {})")))))
      ("z" (cmd (ins "(zerop {})")))
      ("3" (cmd (ins "#P\"{}\"")))))
    ("n"
     (but
      ("t" (cmd (ins "(format \"{}\"{})")))
      ("m" (cmd (ins "(message \"{}\"{})")))))
    ("\\" (cmd (ins "\\\\({}\\\\)")))
    ("s" (cmd (call-interactively 'insert-emacs-sym)))
    ("j" (cmd (ins "(or {})")))
    ("k" (cmd (ins "(and {})")))
    ("1" (cmd (ins "(not {})")))
    (":" (cmd (ins ": ")))
    ("'" (cmd (if (region-active-p)
                  (save-excursion
                    (let ((region (buffer-substring
                                   (region-beginning)
                                   (region-end))))
                      (delete-region (region-beginning)
                                     (region-end))
                      (insert (format "‘%s'" region))))
                (ins "'"))))
    ("-" (cmd (ins "(-> {})")))
    ("_" (cmd (ins "(->> {})")))
    ("`" (cmd (ins "`{}'")))
    ((kbd "s-\"") (cmd (ins "‘{}'")))
    ("p"
     (but
      ("i" (cmd (ins " in ")))
      ("l" (cmd (ins "(loop {}){(nli)}")))
      ("f" (cmd (ins "for ")))
      ("A" (cmd (ins " across ")))
      ("t" (cmd (ins "with {} = ")))
      ("b" (cmd (ins " below ")))
      ("w" (cmd (ins "while ")))
      ("d" (cmd (ins " do")))
      ("o" (cmd (ins "collect ")))
      ("a" (cmd (ins "as {} = ")))
      ("p" (cmd (ins "append ")))
      ("y" (cmd (ins "finally ")))
      ("z" (cmd (ins "if ")))
      ("Z" (cmd (ins "else ")))
      ("x" (cmd (ins "when ")))
      ("c" (cmd (ins "unless ")))
      ("r" (cmd (ins "(return {})")))))))

 (defbuttons cl-buttons emacs-lisp-buttons
   (lisp-mode-map slime-mode-map)
   (but
    ("d"
     (but
      ("i" (cmd (ins "(declare (ignore {}))")))
      ("c" (cmd (ins "(defcommand {} ({}) ({}){(nli)}{})")))
      ("p" (cmd (ins "(defparameter {})")))))
    ("n"
     (but
      ("g" (cmd (ins "(format nil {})")))
      ("t" (cmd (ins "(format t {})")))
      ("f" (cmd (ins "(format fh {})")))
      ("n" (cmd (ins "(format {})")))
      ("[" (cmd (insert "~{~A~^")
                (ins "{}~}")))
      ("v" (but
            ("i" (cmd (ins "(vom:info \"{}~%\"")))
            ("d" (cmd (ins "(vom:debug \"{}~%\"")))
            ("w" (cmd (ins "(vom:warn \"{}~%\"")))))))
    ("#" (cmd (ins "#:")))
    ("5" (cmd (ins "~A")))
    ("%" (cmd (ins "~D")))
    ("|" (cmd (ins "#\\Newline")))
    ("\\" (cmd (ins "~%")))
    (";" (cmd (ins ":")))
    (":" (cmd (ins "::")))
    ("h"
     (but
      ("D" (cmd (ins "(declaim (optimize (debug 3) (speed 0)))")))))
    ((kbd "M-.") 'slime-next-note)
    ((kbd "M-,") 'slime-previous-note)))

 (defbuttons clojure-buttons cl-buttons
   (clojure-mode-map cider-repl-mode-map)
   (but
    ("\\" (cmd (ins "\\n")))
    ("l" (cmd (ins "(let [{}]{(nli)}{}){(nli)}")))
    ("d"
     (but
      ("f" (cmd (ins "(defn {} [{}]{(nli)}{}){(nli)}")))))
    ("n" (cmd (ins "(printf \"{}\\n\"{})")))
    (";" (cmd (ins ": ")))
    ("[" (cmd (insert "{")
              (ins "{}}")))
    ("c" (cmd (ins "(when-not  {})")))
    ("h" (buttons-make
          ("d" 'cider-doc)
          ("a" 'cider-apropos-select)
          ("A" 'cider-apropos-documentation-select)
          ;; ("A" 'cider-apropos-documentation)
          ("j" 'cider-javadoc)
          ("g" 'cider-grimoire)))
    ("{" (cmd (insert "{:keys [")
              (ins "{}]{(nli)}")
              (insert ":or {")
              (ins "{}}}")))
    ("a" (cmd (ins "(fn [{}]{(nli)}{})")))
    ("e" (cmd (ins "(def {})")))
    ((kbd "M-.") 'next-error)
    ((kbd "M-,") 'previous-error)))

 (defbuttons c-buttons programming-buttons
   (c-mode-map)
   (but
    ("f"
     (but
      ("f" (cmd (ins "for ( int {0} = 0; {0} < {}; {0}++ ){(cbd)}")))
      ("F" (cmd (ins "for ( int {0} = {}; {0} >= 0; {0}-- ){(cbd)}")))))
    ("w" (cmd (ins "while ({}){(cbd)}")))
    ("z" (cmd (ins "if ({}){(cbd)}")))
    ("x" (cmd (ins " else if ({}){(cbd)}")))
    ("c" (cmd (ins " else {(cbd)}")))
    ("v" (cmd (ins "?{}: {}")))
    ("V" (cmd (kill-surrounding-sexp nil)
              (end-of-line)
              (ins "{(nli)}(void)")
              (yank-or-pop)
              (ins ";{(inm)}")))
    ("1" (cmd (ins "!")))
    ("n" (but
          ("a"
           (but
            ("t" (cmd (ins "absl::PrintF(\"{}\\n\"{});")))
            ("s" (cmd (ins "absl::StrFormat(\"{}\\n\"{});")))
            ("m" (cmd (ins "absl::StreamFormat(\"{}\"{});")))
            ("c" (cmd (ins ".c_str()")))
            ("r" (cmd (ins "absl::PrintF(\"autogen random marker: {(number-to-string (random 5000))}\\n\");")))))
          ("t" (cmd (ins "printf(\"{}\\n\"{});")))
          ("." (cmd (ins ".c_str()")))
          ("s" (cmd (ins "scanf( \"{}\"{} );")))
          ("," (cmd (ins "<< {} << endl;{(nli)}")))
          ("<" (cmd (ins "cout << ")))))
    ("l" (cmd (ins "strlen( {} )")))
    ("'" (cmd (ins "/*{}*/{(nli)}")))
    ("/" nil)
    ("t" (cmd (ins "true")))
    ("g" (cmd (ins "false")))
    ("G" (cmd (ins "NULL")))
    ("j" (cmd (ins " || ")))
    ("k" (cmd (ins " && ")))
    (">" (cmd (python-indent-shift-right)))
    ("<" (cmd (python-indent-shift-left)))
    ("[" (cmd (insert "{")
              (ins "{}}")))
    (";" (cmd (move-end-of-line nil)
              (ins ";{(nli)}")))
    ("d"
     (but
      ("f" (cmd (ins " ( {} ){(cbd)}")))
      ("m" (cmd (ins "int main (int argc, char* argv[]){(cbd)}")))))
    ("i"
     (but
      ("u" (cmd (ins "unsigned ") (inm)))
      ("t" (cmd (ins "int ") (inm)))
      ("3" (cmd (ins "int32 ") (inm)))
      ("6" (cmd (ins "int64 ") (inm)))
      ("l" (cmd (ins "long ") (inm)))
      ("c" (cmd (ins "char ") (inm)))
      ("C" (cmd (ins "char* ") (inm)))
      ("v" (cmd (ins "void ") (inm)))
      ("n" (cmd (ins "const ") (inm)))
      ("b" (cmd (ins "bool ") (inm)))))
    ("s" (cmd (ins "sizeof({})")))
    ("S" (cmd (ins "sizeof({0})/sizeof(*{0})")))
    ("-" (cmd (ins "->")))
    ("M" (cmd (ins "#include <stdlib.h>") (nli)
              (ins "#include <stdio.h>") (nli)
              (ins "#include <string.h>") (nli)
              (ins "#include <assert.h>") (nli)
              (ins "#define MAX(a, b) ((a)>(b)? (a):(b))") (nli)
              (ins "#define MIN(a, b) ((a)<(b)? (a):(b))") (nli)
              (ins "#define ABS(a) ((a)>=0? (a):-(a))") (nli)))
    ("r" (cmd (ins "return ")))
    ("5" (but
          ("s" (cmd (ins "%s")))
          ("d" (cmd (ins "%d")))
          ("c" (cmd (ins "%c")))
          ("l" (cmd (ins "%ld")))
          ("L" (cmd (ins "%lld")))))
    ("]" (cmd (ins "[]")))
    ("b"
     (but
      ("c" (cmd (ins "continue;")))
      ("k" (cmd (ins "break;")))))))

 (defbuttons java-buttons c-buttons
   (java-mode-map)
   (but
    ("n" (cmd (ins "System.out.printf( \"{}\\n\"{} );{(nli)}")))
    ("l" (cmd (ins ".length")))
    ("G" (cmd (ins "null")))
    ("d"
     (but
      ("d" (cmd (ins " ( {} ){(cbd)}")))
      ("m" (cmd (ins "public static void main ( String[] argv){(cbd)}")))))
    ("p"
     (but
      ("p" (cmd (ins "public ")))
      ("v" (cmd (ins "private ")))
      ("k" (cmd (ins "package ")))
      ("s" (cmd (ins "static ")))))
    ("s" (cmd (ins "this.{(inm)}")))
    ("S" (cmd (java-new)))
    ("F" (cmd (ins "for ({}: {}){(cbd)}")))
    ("L" (cmd (ins "class {}{(cbd)}")))
    ("i"
     (but
      ("i" (cmd (ins "int {(inm)}")))
      ("I" (cmd (ins "Integer {(inm)}")))
      ("s" (cmd (ins "String ")))))
    ("$" (cmd (ins "new {}")
              (insert "[]{")
              (ins "{}}")))
    ("-" (cmd (ins " -> ")))
    ("m" (cmd (ins "import {};")))
    ("m" 'java-imports-add-import-dwim)
    ("t" (cmd (ins "try {(cbd)}catch ({}){(cbd)}")))))

 (defbuttons xml-buttons
   programming-buttons
   (nxml-mode-map)
   (but
    ("/" (cmd (ins "<!--{}-->{(nli)}")))
    ((kbd "M-/") 'xml-toggle-line-comment)
    ("." (cmd (ins "</{0}>")))
    ("e" (cmd (ins "=")))
    ("2" (cmd (ins "\"{}\"")))
    ("u" (cmd (ins "<u>{}</u>")))
    ("," (cmd (ins "<{0}>{}</{0}>")))
    ("n"
     (lambda
       (mix-expr)
       (interactive "senter mix expression: ")
       (insert
        (concat "<mix:message log-level=\"INFO\">"
                (format
                 "%s is <mix:copy-of select=\"%s\"/>"
                 mix-expr mix-expr)
                "</mix:message>"))))))

 (defbuttons html-buttons xml-buttons
   (html-mode-map)
   (but
    ("\\" (cmd (ins "<br/>")))
    ("P" (cmd (ins "<p>{}</p>")))))

 (defbuttons js-buttons c-buttons
   (js-mode-map)
   (but
    ("d" (cmd (ins "function {} ( {} ){(cbd)}")))
    ("a" (cmd (ins "function({}")
              (insert "){")
              (ins "{}}")))
    ("." (cmd (ins "debugger;{(nli)}")))
    ("n" (cmd (ins "console.log( \"{}\"{} );")))
    ("T" (cmd (ins "try{(cbd)}catch(err){(cbd)}")))
    ("f" (cmd (ins "for (var {0} = 0; {0}<{}; {0}++){(cbd)}")))
    ("F" (cmd (ins "for (var {} in {}){(cbd)}")))
    ("l" (cmd (ins ".length")))
    ("r" (cmd (ins "return {};")))
    ("Z" (cmd (ins "if ( {}")
              (insert " ){ ")
              (ins "{} }")))
    ("v" (cmd (ins "var {(inm)}")))
    ("V" (cmd (ins "?{}: {}")))
    ("[" (cmd (insert "{}")))
    ("]" (cmd (ins ".format({})")))
    ("{" (cmd (insert "{")
              (ins "{(nli)}{}{(nli)}}{(idt)}")))
    (";" (cmd (ins ": ")))
    (":" (cmd (ins ": ")))
    ("_" (cmd (ins ",{(nli)}{(inm)}")))
    ("L" (cmd (insert "let { ")
              (ins "{} } = ")))
    ("G" (cmd (ins "null")))
    ("N" (cmd (ins "logger.silly( \"")
              (insert
               (format "%s-%d"
                       (f-filename
                        (buffer-file-name))
                       (random)))
              (ins "\");")))
    ("s" (cmd (ins "this.")))
    ("i" (cmd (ins "in")))
    ("p" (cmd (ins ".prototype.")))
    ("t"
     (but
      ("x" (cmd
            (let-when-compile
                ;; ((buttons-insert-rec-template-directive-regexp "{{\\(.*\\)}}"))
                ()
                  (ins
                   "
    var xhr = new XMLHttpRequest();
    xhr.open('GET{0}', {1}, true);
    xhr.setRequestHeader('Content-type', 'application/json');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4){
            if ( xhr.status != 200) {
                var err = \"{0} \"+{1}+\" failed: \"+xhr.status;
                console.log(err);
                alert(err);
            }else{
                var {} = JSON.parse(xhr.responseText);
                {}
            }
        }
    };
    xhr.send({});
"
                   ))))
      ("d" (but
            ("c" (cmd (ins "document.createElement({})")))
            ("b" (cmd (ins "document.body")))
            ("g" (cmd (ins "document.getElementById({})")))
            ("a" (cmd (ins ".appendChild({})")))
            ("t" (cmd (ins ".textContent({})")))))
      ;; node
      ("y" (cmd (ins "try {(cbd)}catch ({}){(cbd)}")))))))


 (defbuttons go-buttons c-buttons
   (go-mode-map)
   (but
    ("a" (cmd (ins "func({}")
              (insert "){")
              (ins "{}}")))
    ("s" (cmd (ins ".(*{})")))
    ("E" (cmd (ins " := ")))
    ("d" (cmd (ins "func {} ( {} ) {}{(cbd)}")))
    ("D" (cmd (ins "func Test{} ( t *testing.T ){(cbd)}")))
    ("]" (cmd (ins "[]")))
    ("#" (cmd (ins "()")))
    ("#" (cmd (ins "()")))
    ("r" (cmd (ins "return {(inm)}")))
    ("M" (cmd (ins "package main{(nli)}")))
    ("m" (cmd (ins "fmt.Sprintf( \"{}\\n\"{} )")))
    ("n" (cmd (ins "fmt.Printf( \"{}\\n\"{} )")))
    ("x" (cmd (ins "else if {}; {}{(cbd)}")))
    ("z" (cmd (ins "if {}; {}{(cbd)}")))
    (":" (cmd (ins ": ")))
    ("Z" (cmd (ins "if {}{(cbd)}")))
    ("Z" (cmd (ins "if ; DEBUG{(cbd)}")))
    ("F" (cmd (ins "for i := 0; i < {}; i++{(cbd)}")))
    ("W" (cmd (ins "switch {(cbd)}")))
    ("w" (cmd (ins "case {}:{(nli)}")))
    (";" (cmd (ins ":{(nli)}")))
    ("T" (cmd (ins "type {} struct {(cbd)}")))
    ("G" (cmd (ins "nil")))
    ("6" (cmd (ins "%v")))
    ("^" (cmd (ins "%#v")))
    ("v" (cmd (ins "var ")))
    ("e" (cmd (ins " = ")))
    ("l" (cmd (ins "len( {} )")))
    ("R" (cmd (ins "range {(inm)}")))
    ("f11" (cmd (go-run)))
    ("+" (cmd (ins " != ")))
    ("f" (cmd (ins "for {} := range {}{(cbd)}")))
    ("P" (cmd (ins "%p")))
    ("_" (cmd (ins "_, ")))
    ("{" (cmd (ins "&{}")
              (insert "{")
              (ins "{}}")))
    ("O" (cmd (insert "verbose(func(){fmt.Printf(\"VERBOSE: ")
              (ins "{}\"{})})")))))

 (defbuttons bash-buttons programming-buttons
   (sh-mode-map)
   (but
    ("1" (cmd (ins "! ")))
    ("V" (cmd (insert "\"${")
              (rec)
              (upcase-last)
              (ins "}\"")))
    ("v" (cmd (insert "${")
              (rec)
              (upcase-last)
              (ins "}")))
    ("w" (cmd (ins "while {}; do{(nli)}{}{(nli)}done")))
    ("e" (cmd (upcase-last)
              (ins "=")))
    ("E" (cmd (upcase-last)
              (insert "=${")
              (insert
               (bash-identifier-current-line))
              (ins "{}:-{}}{(nli)}")))
    ("$" (cmd (ins "$({})")))
    ("j" (cmd (ins " || ")))
    ("k" (cmd (ins " && ")))
    ("S" (cmd (ins "{(idt)}")
              (insert "case ${")
              (rec)
              (upcase-last)
              (ins "} in{(nli)}{}{(nli)}esac{(nli)}")))
    ("s" (cmd (ins "{(idt)}){(nli)}{}{(nli)};;{(nli)}")))
    ("o" (cmd (insert "${OPTARG}")))
    ("4" (cmd (ins "[ {} ]")))
    ("z" (cmd (ins "if {}; then{(nli)}{}{(nli)}fi{(idt)}{(nli)}")))
    ("x" (cmd (ins "elif {}; then{(nli)}{}{(nli)}{(idt)}")))
    ("c" (cmd (ins "else {}; then{(nli)}{}{(nli)}{(idt)}")))
    ("\\" (cmd (ins " \\{(nli)}")))
    ("|" (cmd (ins " | ")))
    ("n" (cmd (ins "echo ")))
    ("d" (cmd (ins "function {}{(cbd)}")))
    ("l" (cmd (insert " || exit ${LINENO}")))
    ("L" (cmd (ins "echo \"{}")
              (insert "\" && exit ${LINENO}")))
    ("f" (cmd (ins "for {}")
              (upcase-last)
              (ins " in {}; do{(nli)}{}{(nli)}done")))
    ("H" (cmd
          (let-when-compile
              ((buttons-insert-rec-template-directive-regexp "<\\(.*\\)>"))
            (ins "<(upcase-last)>=${1} && shift")
            (ins "<(nli)>"))))
    ("g" (cmd (ins "true")))
    ("G" (cmd (ins "false")))
    ("C" (cmd (ins "<<EOF{(nli)}{}{newline}EOF")))
    ;; ( "x" 'shell-command-of-region)
    ("0" (cmd (insert sh-getopt-template)))
    ("t" (but
          ("0" (cmd (insert "cd \"$( dirname \"${BASH_SOURCE[0]}\" )\"")))
          ("s"
           (but
            ("t" (cmd (ins "test ")))
            ("j" (cmd (ins " -o ")))
            ("k" (cmd (ins " -a ")))
            ("n" (cmd (ins " -n ")))
            ("z" (cmd (ins " -z ")))
            ("e" (cmd (ins " -e ")))))
          ("u" 'insert-unique-line)))))

 (defbuttons tex-buttons programming-buttons
   (tex-mode-map)
   (but
    ("m" (cmd (ins "${}$")))
    ("b" (cmd (insert "\\begin{")
              (ins "{0}}{}")
              (insert "\\end{")
              (ins "{0}}")))
    ("B" (cmd (insert "\\textbf{")
              (ins "{}}")))
    ("[" (cmd (insert "{")
              (ins "{}}")))
    ("i" (cmd (ins "\\in ")))
    ("I" (cmd (ins "\\item {(idt)}{}{(nli)}")))
    ("I" (cmd (ins "\\item {(idt)}{}{(nli)}")))
    ("l" (cmd (insert "\\begin{align*}")
              (ins "{(nli)}{}{(nli)}")
              (insert "\\end{align*}")))
    ("L" (cmd (insert "\\begin{tabular}{lr}")
              (ins "{(nli)}{}")
              (insert "\\end{tabular}")))
    ("_" (cmd (ins ".${}$.")))
    ("x" (cmd (ins "(.x.)")))
    ("q" (cmd (insert "\\begin{numedquestion}")
              (ins "% q#{}{(nli)}{}{(nli)}")
              (insert "\\end{numedquestion}")
              (ins "{(idt)}{(nli)}")))
    ("P" (cmd (insert "\\begin{part}")
              (ins "% ({}){(nli)}{}{(nli)}")
              (insert "\\end{part}")
              (ins "{(idt)}{(nli)}")))
    ("j" (cmd (ins " \\vee ")))
    ("k" (cmd (ins " \\wedge ")))
    ("j" (cmd (ins "\\cup ")))
    ("k" (cmd (ins "\\cap ")))
    ("1" (cmd (ins " \\sim ")))
    ("\\" (cmd (ins " \\\\{(nli)}{(inm)}")))
    ("6" (cmd (insert "^{")
              (ins "{}}")))
    ("K" (cmd (insert "{")
              (ins "{} \\choose {}} {(inm)}")))
    ("f" (cmd (insert "\\frac{")
              (rec)
              (insert "}{")
              (ins "{}}")))
    ("(" (cmd (ins "P({})")))
    ("8" (cmd (ins "P({})")))
    (")" (cmd (ins " + ")))
    ("." (cmd (ins "\\cdot ")))
    ("|" (cmd (ins "P({}|{})")))
    ("_" (cmd (backward-char)
              (upcase-last)
              (ins "_")
              (forward-char)))
    ("t" (cmd (insert "\\text{")
              (ins "{}}")))
    ("{" (cmd (insert "\\{")
              (ins "{}\\}")))
    ("-" (cmd (ins "(1-p)^{(inm)}")))
    ("_" (cmd (ins "_")))
    ("p" (cmd (ins "^{(inm)}")))
    (";" (cmd (insert "P(\\{X=")
              (ins "{}\\})")))
    ("=" (cmd (ins " + ")))
    ("E" (cmd (ins "E[{}]")))
    ("]" (cmd (ins "^")))
    ("+" (cmd (ins "+")))
    ("v" (cmd (insert "\\begin{verbatim}")
              (rec)
              (insert "\\end{verbatim}")))
    ("7" (cmd (ins " & ")))
    (";" (cmd (ins "\\;")))
    ("/" 'my-comment-out)))

 (defbuttons matlab-buttons python-buttons
   (matlab-mode-map)
   (but
    ("z" (cmd (ins "if {};{(nli)}{}{(nli)}end{(idt)}")))
    ("'" (cmd (ins "'")))
    ("f" (cmd (ins "for {}=1{}:{}{(nli)}{}end")))
    ("j" (cmd (ins " ||  ")))
    ("k" (cmd (ins " &&  ")))
    ("2" (cmd (ins "'{}'")))
    ("L" (cmd (ins "size({})")))
    ("l" (cmd (ins "length({})")))
    ("s" (cmd (ins "class({})")))
    ("+" (cmd (ins "~=")))
    ("h" (cmd (ins "help {(inm)}")))
    ("1" (cmd (ins "~")))
    ("@" (cmd (ins "@(x)")))
    ("a" (cmd (ins "arrayfun(@(x) {})")))
    (">" (cmd (ins "keyboard;{(nli)}")))
    ("Q" (cmd (ins "dbquit")
              (cmt "")))
    ("q" (cmd (ins "dbcont")
              (cmt "")))
    ("N" (cmd (ins "sprintf('{}')")))
    ("N" (cmd (ins "disp(sprintf('{}'{}))")))
    ("[" (cmd (insert "{")
              (ins "{}}")))
    ("5" (but
          ("f" (cmd (ins "%f")))))
    (";" (cmd (ins ": ")))
    ("x" (cmd (ins "elseif ")))))

 (defbuttons r-buttons programming-buttons
   (ess-mode-map)
   (but
    ("h" (cmd (ins "help.search({(inm)}{})")))
    ("e" (cmd (ins " <- ")))
    ("d" (cmd (ins " <- function({}){(cbd)}")))
    ("8" (cmd (ins "%*%")))
    ("'" (cmd (ins "t({})")))
    ("f" (cmd (ins "for({} in as.single({}:{})){(cbd)}")))
    ("-" (cmd (ins "attr({}, \"{}\" )")))
    ("N" (cmd (ins "print({})")))))

 (defbuttons octave-buttons matlab-buttons
   (octave-mode-map inferior-octave-mode-map)
   (but
    ("d" (cmd (ins "function [{}] = {}({}){(nli)}{}{(nli)}endfunction")))
    ("'" (cmd (insert "#{")
              (ins "{}#}")))
    ("2" (cmd (ins "\"{}\"")))))

 (defbuttons cpp-buttons c-buttons
   (c++-mode-map)
   (but
    ("f"
     (but
      ("a" (cmd (ins "for(auto& {}: {}){(cbd)}")))))
    ("i"
     (but
      ("m" (cmd (ins "map<{}> ") (inm)))
      ("p" (cmd (ins "pair<{}> ") (inm)))
      ("V" (cmd (ins "vector<{}> ")))
      ("s" (cmd (ins "string ")))
      ("S" (cmd (ins "string& ")))
      ("t" (but
            ("s" (cmd (ins "std::")))
            ("g" (but
                  ("a" (cmd (ins "absl::")))
                  ("f"
                   (but
                    ("i" (cmd (ins "flume::")))
                    ("p" (cmd (ins "flume::PCollection<{}>")))))
                  ("l" (cmd (ins "logs::")))))))))
    ("m" (cmd (ins "#include {}")))
    ("M" (cmd (ins "using namespace std;") (nli)
              (ins "#include <vector>") (nli)
              (ins "#include <unordered_map>") (nli)
              (ins "#include <iostream>") (nli)
              (ins "#define MAX(a, b) ((a)>(b)? (a):(b))") (nli)
              (ins "#define MIN(a, b) ((a)<(b)? (a):(b))") (nli)
              (ins "#define ABS(a) ((a)>=0? (a):-(a))") (nli)))
    ("l" (but
          ("s" (cmd (ins ".size()")))
          ("t" (cmd (ins ".length()")))))
    ("s" (cmd (ins "scanf( \"{(inm)}{}\", {} )")))
    ("s" (cmd (ins "scanf( \"%d\", &{(inm)}{} );")))
    ("S" (cmd (ins "int {0}; scanf( \"%d\", &{0} );{(nli)}")))
    (";" (cmd (ins "::")))
    ("M" (cmd (ins "#include <unordered_map>") (nli)
              (ins "#include <iostream>") (nli)
              (ins "#include <string>") (nli)
              (ins "#include <assert.h>") (nli)
              (ins "using namespace std;") (nli)))))

 (defbuttons yacc-buttons programming-buttons
   (yacc-mode-map)
   (but
    ("v" (cmd (ins "$")
              (insertchar)))
    ("D" (cmd (ins "{(nli)}:	")))
    ("d" (cmd (ins "{(nli)}|	")))))

 (defbuttons dot-buttons programming-buttons
   (dot-mode-map)
   (but
    ("l" (cmd (ins " [label=\"{}\"];")))
    ("-" (cmd (ins " -> ")))))

 (defbuttons forum-post-buttons programming-buttons
   (forum-mode-map)
   (but
    ("," (cmd (ins "[code]{}[/code]")))))

 (defbuttons org-buttons nil
   (org-mode-map)
   (but
    ("q" (cmd (ins "#+BEGIN_SRC {}{(nli)}{}#+END_SRC{(nli)}")))
    ("`" (cmd (ins "~{}~")))
    ("Q" (cmd (ins "#+begin_quote {}{(nli)}{}#+end_quote{(nli)}")))
    ((kbd "<s-tab>") 'org-indent-block)
    ("return" 'org-toggle-list-heading)
    ("i"
     (lambda nil
       (interactive)
       (if org-inline-image-overlays
           (org-remove-inline-images)
         (org-display-inline-images))))
    ("m" (cmd (ins "#+OPTIONS: ^:nil") (nli)
              (ins "#+OPTIONS: toc:nil") (nli)
              (ins "#+OPTIONS: html-postamble:nil") (nli)
              (ins "#+OPTIONS: num:nil") (nli)
              (ins "#+TITLE: {}") (nli)))
    ("R" (cmd (ins "***REMOVED***")))
    ("p" 'org-todo-promote-top)
    ("r" 'org-refile)
    ("w" 'org-refile)
    ("c" 'org-clock-in)
    ("C" 'org-resolve-clocks)))

 (defbuttons message-buttons nil
   (message-mode-map)
   (but
    ("=" (cmd (ins " => ")))
    ("<" (cmd (re-sub "^[ 	]*>?[ 	]*" "")))))

 (defbuttons ansi-term-buttons nil
   (term-raw-map)
   (but
    ("c"
     (lambda nil
       (interactive)
       "send ^C^C"
       (term-send-raw-string "")
       (term-send-raw-string "")))))

 (defbuttons conf-buttons programming-buttons
   (conf-mode-map)
   (but
    ("e" (cmd (ins "=")))))

 (defbuttons magit-buttons nil
   (magit-mode-map)
   (but
    ("p" 'magit-go-backward)
    ("n" 'magit-go-forward)))

 (defbuttons diff-buttons nil
   (diff-mode-map)
   (but
    ("-"
     (git-hunk-toggle-cmd "-"))
    ("="
     (git-hunk-toggle-cmd "+"))
    ("0"
     (git-hunk-toggle-cmd " "))))

 (defbuttons backtrace-bindings nil
   (debugger-mode-map emacs-lisp-mode-map inferior-emacs-lisp-mode-map)
   (but
    ("h"
     (but
      ("f" (cmd (describe-function-at-point)))
      ("d" (but
            ("t" (cmd (setf debug-on-error t)
                      (message "debug-on-error: %s" debug-on-error)))
            ("g" (cmd (setf debug-on-error nil)
                      (message "debug-on-error: %s" debug-on-error)))
            ("b" 'edebug-set-breakpoint)))
      ("q" (cmd (with-current-buffer "*Backtrace*" (top-level))))))))

 (defbuttons sldb-bindings nil
   (sldb-mode-map)
   (but
    ("a" 'sldb-abort)
    ("c" 'sldb-continue)
    ("q" 'sldb-quit)
    ;; (loop for i below 5 collect
    ;;       `(,(number-to-string i)
    ;;         ',(intern (format "sldb-invoke-restart-%d" i))))
    ("0" 'sldb-invoke-restart-0)
    ("1" 'sldb-invoke-restart-1)
    ("2" 'sldb-invoke-restart-2)
    ("3" 'sldb-invoke-restart-3)
    ("4" 'sldb-invoke-restart-4)
    ("5" 'sldb-invoke-restart-5)))

(let-when-compile
    ((*buttons-make-key-mapper* nil))

 (defbuttons minibuffer-quick-yes-button
   nil
   (minibuffer-local-map)
   (but
    ((kbd "s-SPC") 'quick-yes-answer-yes)
    ;; this is different from the emacs-lisp binding: no need to escape the \ itself
    ((kbd "s-\\") (cmd (ins "\\({}\\)")))
    ((kbd "s-9") (cmd (ins "[0-9]+")))))

 (defbuttons apropos-buttons
   nil
   (global-map)
   (but
    ((kbd "C-h")
     (but
      ("a"
       (but
        ;; all
        ("l" 'apropos)
        ;; functions/commands
        ("f" (cmd (let ((apropos-do-all t))
                    (call-interactively 'apropos-command))))
        ;; commands only
        ("c" (cmd (let ((apropos-do-all nil))
                    (call-interactively 'apropos-command))))
        ;; variables
        ("v" (cmd (call-interactively 'apropos-variable)))))))))
)
 (defbuttons slime-buttons
   nil
   (slime-mode-map)
   (but
    ("h"
     (but
      ("a" 'slime-apropos)
      ("s" 'slime-describe-symbol)
      ("f" 'slime-describe-function)
      ("d" 'slime-documentation-lookup)
      ("z" 'slime-apropos-all))))))
)
