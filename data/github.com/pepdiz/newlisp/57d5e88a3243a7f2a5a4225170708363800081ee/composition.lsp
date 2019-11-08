; composition functions for newlisp

; only applicable to lambda functions and builtins (no special forms)
; composition requires currification, but this is not checked (relies on programmer)
; --checking this woin't be a real trouble but adds computation:
; -- (define (º f g) 
; --   (if (and (= (length (first f)) 1) (= (length (first g)) 1))
; --     (expand (lambda (x) (g (f x))) 'g 'f)
; --     (throw-error "functions must be curryfied")))

; to use just (load "https://raw.githubusercontent.com/pepdiz/newlisp/master/composition.lsp")  
; but better remove tests first!

; mathematical composition (but reversed notation)
(define (º f g) (expand (lambda (x) (g (f x))) 'g 'f))

; ((º inc dec) 4)
; (º (curry map inc) print)
; ((º (curry map inc) print) '(1 2 3))

; extending composition to several functions
; newlisp hasn't a way to explicitly define variadic functions
; but newlisp dosen't complain about matching formal arguments and actual ones
; for user defined funtions (built-in's are another song)
; so a function defined witout arguments behaves as a variadic function
; provided you access arguments throug args function
(define (->) (apply º (args) 2)) 



; --- TESTS
(define (test e v) (if (= (eval e) v) (print "test " e " ok") (print "test " e " failed")))

(test '((º inc dec) 4) 4)
(test '((º (º inc dec) inc) 4) 5)
(test '((º (curry map inc) print) '(1 2 3)) '(2 3 4))
(test '((-> inc dec inc) 4) 5)
