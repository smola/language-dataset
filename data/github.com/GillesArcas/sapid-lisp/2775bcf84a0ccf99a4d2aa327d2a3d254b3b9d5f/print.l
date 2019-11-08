; sapid lisp
; Printing
; gilles.arcas@gmail.com, https://github.com/GillesArcas/sapid-lisp
; MIT license

; This file redefines the builtin functions print, print and terpri to:
; - admit several arguments,
; - print atoms without cutting them with carriage returns,
; - print atoms using user or reader representation,
; - manage left and right margins,
; - control printing length and depth,
; - use quote character to print quoted forms.

; Save builtin definitions

;(if (eq (typefn 'prin  ) 'subr1) (synonym 'subr.prin   'prin  ))
;(if (eq (typefn 'print ) 'subr1) (synonym 'subr.print  'print ))
;(if (eq (typefn 'terpri) 'subr0) (synonym 'subr.terpri 'terpri))
(if (neq (typefn 'prin  ) 'lambda) (synonym 'subr.prin   'prin  ))
(if (neq (typefn 'print ) 'lambda) (synonym 'subr.print  'print ))
(if (neq (typefn 'terpri) 'lambda) (synonym 'subr.terpri 'terpri))

; Redefine prin and print

(de print x
    "Prints all elements in x followed by a carriage return."
    "See *print-level*, *print-length*, *print-reader*, *left-margin*,"
    "*righ-margin* for printing customization."
    (prog1
        (apply 'prin x)
        (terpri) ) )

(de prin x
    "Prints all elements in x without carriage return."
    "See *print-level*, *print-length*, *print-reader*, *left-margin*,"
    "*righ-margin* for printing customization."
    (if (atom (cdr x))
        (prin-exp (car x) 0)
        (prin-exp (car x) 0)
        (apply 'prin (cdr x)) ) )

; Redefine terpri with left margin

(de terpri ()
    "Outputs a carriage return. Returns t."
    (subr.terpri)
    (repeat *left-margin*
        (subr.prin " ") )
    (setq *outpos* *left-margin*)
    t )

; Printing customization

(describe *print-level*
    "Maximum depth of printed sub-expression. Prints ""&"" for higher depth." )

(describe *print-length*
    "Maximum length of printed lists. Prints ""..."" for higher length." )

(describe *left-margin*
    "Left margin." )

(describe *right-margin*
    "Right margin." )

(describe *print-reader*
    "Boolean. If true, printing produces a text ready to be read by the reader."
    "This concerns strings which are surrounded by "", and special symbols by |." )

; Default settings

(setq *print-level*   100)
(setq *print-length* 1000)
(setq *left-margin*     0)
(setq *right-margin*   79)
(setq *print-reader*  nil)

; Update output column

(de update-outpos (n)
    ; something of length n is going to be output. Check for place before
    ; right margin and set output pointer.
    (incr *outpos* n)
    (when (> *outpos* *right-margin*)
        (terpri)
        (incr *outpos* n) ) )

(setq *outpos* 0)

; Print one expression

(de prin-exp (e level)
    (ifn e
        (prin-atom-user "()")
        (if (atom e)
            (prin-atom e)
            (if (>= level *print-level*)
                (prin-atom-user "&")
                (if (and (eq (car  e) 'quote)(eq (cddr e) ()))
                    (progn
                        (prin-atom-user "'")
                        (prin-exp (cadr e) level) )
                    (prin-atom-user "(")
                    (prin-list e (1+ level) 0)
                    (prin-atom-user ")") ) ) ) )
    e)

; Print lists

(de prin-list (e level len)
    (if (atom e)
        (progn
            (prin-atom-user ". ")
            (prin-atom e) )
        (if (>= len *print-length*)
            (prin-atom-user "...")
            (prin-exp (car e) level)
            (when (cdr e)
                (prin-atom-user " ")
                (prin-list (cdr e) level (1+ len)) ) ) ) )

; Printing of atoms

(de prin-atom (x)
    (if *print-reader*
        (prin-atom-reader x)
        (prin-atom-user x) ) )

; User readable printing of atoms

(de prin-atom-user (x)
    (update-outpos (plength x))
    (subr.prin x) )

(de plength (x)
    (strlen (string x)) )

; Machine readable printing of atoms

(de prin-atom-reader (x)
    (if (symbolp x)
        (prin-symbol-readable x)
        (if (stringp x)
            (prin-string-readable x)
            (prin-atom-user x) ) ) )

(de prin-string-readable (x)
    (prin-elem-reader x """" (readable-string-length x)) )

(de prin-symbol-readable (x)
    (if (is-special-symbol x)
        (prin-elem-reader (string x) "|" (readable-symbol-length x))
        (prin-atom-user x) ) )

(de is-special-symbol (x)
    (let ((y (string x)) (i 0) (l (strlen (string x))))
        (while (and (< i l) (= (typech (strnth y i)) symbol-char))
            (incr i) )
        (< i l) ) )

(de prin-elem-reader (x delim)
    ; x is a string, possibly from a special symbol, delim is " or |
    (let ((n (number-of-inner-delim x delim)))
        (update-outpos (+ 2 (strlen x) n))
        (prin-atom-user delim)
        (if (= n 0)
            (prin-atom-user x)
            (let ((i 0))
                (while (< i (strlen x))
                    (if (= (strnth x i) delim)
                        (repeat 2 (prin-atom-user delim))
                        (prin-atom-user (strnth x i)) )
                (incr i) ) ) )
        (prin-atom-user delim) ) )

(de number-of-inner-delim (x delim)
    ; x is a string, possibly from a special symbol, delim is " or |
    (let ((i 0) (n 0) (l (strlen x)))
        (while (< i l)
            (if (= (strnth x i) delim)
                (incr n) )
            (incr i) )
        n ) )
