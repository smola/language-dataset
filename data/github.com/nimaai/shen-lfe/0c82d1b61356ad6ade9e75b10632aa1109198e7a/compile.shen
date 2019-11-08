(set *kl-directory* "kl-sources/")
(set *lfe-directory* "lfe-sources/")

(set *kl-natives* [and cons hd if or tl])
(set *kl-primitives* [cons? error-to-string intern set simple-error value])

(define native? X -> (element? X (value *kl-natives*)))
(define primitive? X -> (element? X (value *kl-primitives*)))

(define compile-kl ->
  (do
    (output "klambda directory: ~S~%" (value *kl-directory*))
    (output "lfe directory ~S~%" (value *lfe-directory*))
    (output "~%")
    (map
      (function compile-kl-file)
      [
       \*
       "core"
       "declarations"
       "load"
       "macros"
       "prolog"
       "reader"
       "sequent"
       "sys"
       "dict"
       "t-star"
       *\
       "toplevel"
       \*
       "track"
       "types"
       "writer"
       "yacc"
       *\
       ])
    (output "~%")
    (output "compilation complete.~%")
    ()))

(define compile-kl-file
  File ->
    (let _ (output "compiling ~A~%" File)
         KlFile (make-string "~A~A.kl" (value *kl-directory*) File)
         LfeFile (make-string "~A~A.lfe" (value *lfe-directory*) File)
         KlCode (read-file KlFile)
         QFreeKlCode (map-quote-free-symbols [] KlCode)
         LfeCode (map-make-lfe-code QFreeKlCode)
         LfeString (list->string [(module File) (wrap-in-load (skip-copyright LfeCode))])
         Write (write-to-file LfeFile LfeString)
      KlFile))

(define module File -> [defmodule (intern File)])

(define wrap-in-load
  Code -> [defun load [] | Code])

(define skip-copyright
  [Copy | Rest] -> Rest where (string? Copy)
  X -> X)

(define list->string
  [] -> ""
  [X | Y] -> (@s (make-string "~R~%~%" X) (list->string Y)))

(define make-lfe-code
  [F | R] -> [F | R] where (native? F)
  [F | R] -> (let FString (str F)
                  FNew (intern (cn "klambda:" FString))
                  [FNew | (map-make-lfe-code R)])
          where (primitive? F)
  [defun N P B] -> [(intern "macros:defun") N P (make-lfe-code B)]
  [lambda P B] -> [(intern "macros:lambda") P (make-lfe-code B)]
  [freeze P B] -> [(intern "macros:freeze") P (make-lfe-code B)]
  [let X Y B] -> [(intern "macros:let") X (make-lfe-code Y) (make-lfe-code B)]
  [trap-error X F] -> [(intern "macros:trap-error") (make-lfe-code X) (make-lfe-code F)]
  [F | R] -> [(intern "macros:funcall") F | (map-make-lfe-code R)]
  Code -> Code)

(define quote-free-symbols
  P [defun X Y Z] -> [defun X Y (quote-free-symbols (union Y P) Z)]
  P [lambda X Y] -> [lambda X (quote-free-symbols (union [X] P) Y)]
  P [let X Y Z] -> [let X (quote-free-symbols P Y)
                        (quote-free-symbols (union [X] P) Z)]
  P [trap-error X F] -> [trap-error (quote-free-symbols P X) (quote-free-symbols P F)]
  P [X | Y] -> [X | (map-quote-free-symbols P Y)] where (symbol? X)
  P X -> (map-quote-free-symbols P X) where (cons? X)
  P X -> (intern (@s "'|" (str X) "|")) where (and (symbol? X) (not (element? X P)))
  P X -> X)

(define map-quote-free-symbols
  P Y -> (map (/. X (quote-free-symbols P X)) Y))

(define map-make-lfe-code
  Y -> (map (function make-lfe-code) Y))
