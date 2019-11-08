#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

# 08feb17jk
# (c) Jon Kleiser

# First cd picoLisp/doc/, then do
# pil pathTo/convConcat.l pathTo/output.html

# Omit the output argument if you just want analytics and tests

(setq *Fout (opt))  # output file argument

(setq *Href (chop "href"))
(setq *Name (chop "name"))
(setq *Target (chop "target"))

# idx trees used to check name value (and other property) usage, and more
(setq *NameIdx NIL)
(setq *ReuseIdx NIL)
(setq *UnquotedIdx NIL)
(setq *HrefIdx NIL)
(setq *SomePropIdx NIL)

(de filesToRead ()
  (make (link "toc.html" "ref.html")
    (for C (range 65 90)
      (link (pack "ref" (char C) ".html")) )
    (link "ref_.html")
    (chain (mapcar '((F) (pack "form/" F)) (dir "form/")))
    (link "tut.html" "app.html" "faq.html" "native.html" "select.html" "httpGate.html") ) )

(de externalUrl (Url)
  (or (pre? "irc:" Url) (pre? "http:" Url) (pre? "https:" Url) (pre? "mailto:" Url)) )

(de filenamePrefix (DropRefX Fname)
  (unless (and DropRefX (pre? "ref" Fname) (= 9 (length Fname)))
    (let Fch (head -5 (chop Fname))  # removing ".html"
      (append
        (replace Fch "/" "-")
        (list ":") ) ) ) )

(de convHref (F Val)
  #(idx '*HrefIdx (pack F "\t" Val) T)
  (if (externalUrl Val)
    Val
    # Not external
    (let (H (split Val "#")
          Hpf (head -5 (car H)) # removed possible ".html"
          Hext (pack (last (split (car H) ".")))
          Hfrag (cadr H) )
      (if (and Hext (<> Hext "html"))
        Val # some file other than .html
        (make
          (link "#")
          (let (P (split (head -5 (chop F)) "/")  # with ".html" removed
                P1of2 (when (cdr P) (car P))      # part 1 (form) when "/" in F
                Fpref
                  (if P1of2
                    (if (pre? "../" Hpf)
                      (tail -3 Hpf)  # removing "../"
                      (append P1of2 (list "-") Hpf) ) # form/Hpf
                    # P has only one part (no "/" in F)
                    (or (replace Hpf "/" "-") (car P)) )
                Feco
                  (unless (and Hfrag (pre? "ref" Fpref) (= 4 (length Fpref)))
                    Fpref ) )
            (when Feco
              (chain Feco)
              (link ":") )
            (chain Hfrag) ) ) ) ) ) )

(de convName (F Val)
  (let V (pack Val)
    (when (idx '*NameIdx V T)
      (idx '*ReuseIdx V T) ) )
  (if (filenamePrefix T F)
    (append @ Val)
    Val ) )

(de convTarget (F Val)
  (if (= Val (chop "upper"))
    (chop "_self")
    Val ) )

# Returning text between "-quotes
(de unquote (Val)
  (let L (split Val "\"")
    (if (= 3 (length L))
      (cadr L)
      # Missing quote
      (idx '*UnquotedIdx (pack Val) T)
      Val ) ) )

(de propValCons (PropVal)
  (let (Prop (car (split PropVal "="))
        Val (tail (- -1 (length Prop)) PropVal) )
    (cons Prop Val) ) )

(de propValPairs (Props)
  (let (Raw (filter car (split Props " "))
        Head (car Raw)
        Tail (cdr Raw) )
    (make
      (while Head
        (while (or (= "=" (last Head)) (pre? "=" (car Tail)))
          (setq Head (make (chain Head) (chain (car Tail))))
          (setq Tail (cdr Tail)) )
        (link (propValCons Head)) # getting another (prop . val)
        (setq Head (car Tail))
        (setq Tail (cdr Tail)) ) ) ) )

(de convLink (F Chunk)
  (use (@After @Before @Props @X)
    # First checking special case <a name=">foo">
    (if (match '(@Before < a " " n a m e = "\"" > @X "\"" > @After) Chunk)
      (cons
        (make
          (chain @Before (chop "<a name=\""))
          (let Val (convName F (unquote (make (chain (chop "\">") @X) (link "\""))))
            (chain Val (chop "\">")) ) )
        @After )
      # Other normal cases, requiring a link to have a " just before the closing ">"
      (ifn (match '(@Before < a " " @Props "\"" > @After) Chunk)
        (cons Chunk NIL)
        # Handle properties
        (cons
          (make
            (chain @Before (chop "<a"))
            (for PV (propValPairs (make (chain @Props) (link "\"")))  # adding back " at the end
              (let (Prop (car PV)  Val (unquote (cdr PV)))
                (setq Val
                  (if (= *Href Prop)
                    (convHref F Val)
                    (if (= *Name Prop)
                      (convName F Val)
                      (if (= *Target Prop)
                        (convTarget F Val)
                        # Some other property
                        (idx '*SomePropIdx (pack Prop) T)
                        Val ) ) ) )
                (link " ")
                (chain Prop (chop "=\"") Val)
                (link "\"") ) )
            (link ">") )
          @After ) ) ) ) )

(de convAllLinks (F Chunk)
  (make
    (let C (cons () Chunk)
      (while (cdr C)
        #(println ">>" @)
        (setq C (convLink F @))
        (chain (car C)) ) ) ) )

(de convFile (F BodyLinesOnly)
  (when *Fout
    (prinl "<div class=\"docunit\" id=\"" (filenamePrefix NIL F) "\">"))
  (in F
    (when BodyLinesOnly
      # Skipping header and <body> tag
      (from "<body>") )
    (make
      (let Done NIL
        (until (or Done (eof))
          (let Line (line)
            (if (and BodyLinesOnly (pre? "</body>" Line))
              (on Done)
              # Not Done, keep on processing
              (let TrimmedLine (trim Line)
                (if (= '(< a) (tail 2 TrimmedLine))
                  (chain TrimmedLine (list " "))
                  # TrimmedLine not ending with "<a"
                  (when (or TrimmedLine (sp? (pack (made))))
                    # TrimmedLine not empty, or (made) just LF or empty
                    (chain Line)
                    # Process, then clear
                    (let Res (convAllLinks F (made))
                      (when *Fout (prin Res)) )
                    (made (list "\n")) ) ) ) ) ) ) ) ) )
  (when *Fout (prinl "</div>")) )

(de printNamesAndReuse ()
  (prinl "Name values:")
  (println (sort (idx '*NameIdx)))
  (prinl "Re-used name values:")
  (println (sort (idx '*ReuseIdx)))
  (prinl "Unquoted values:")
  (println (sort (idx '*UnquotedIdx)))
  (prinl "Unhandled properties:")
  (println (sort (idx '*SomePropIdx))) )

(de printHrefIdx (F)
  (out F
    (mapcar prinl (sort (idx '*HrefIdx))) ) )

(de doTests ()
  (let (Goods 0 Bads 0)
    (mapc
      '((BoolExpr)
        (if (eval BoolExpr)
          (inc 'Goods)
          (inc 'Bads) (println 'Error: BoolExpr 'Result: (eval (caddr BoolExpr))) ) )
      (list
        '(= NIL (externalUrl (chop "abc.html")))
        '(= "http://localhost:8080" (externalUrl (chop "http://localhost:8080")))
        '(= "irc://irc.fn.net/pil" (externalUrl (chop "irc://irc.fn.net/pil")))
        '(= NIL (filenamePrefix T "ref_.html"))
        '(= '("r" "e" "f" "_" ":") (filenamePrefix NIL "ref_.html"))
        '(= '("a" "p" "p" ":") (filenamePrefix T "app.html"))
        '(= (chop "form-refB:") (filenamePrefix T "form/refB.html"))
        
        '(= '("#" "r" "e" "f" "A" ":") (convHref "ref.html" (chop "refA.html")))
        '(= '("#" "a" "r" "g") (convHref "ref.html" (chop "refA.html#arg")))
        '(= '("#" "r" "e" "f" ":") (convHref "app.html" (chop "ref.html")))
        '(= '("#" "r" "e" "f" ":" "f" "u" "n") (convHref "app.html" (chop "ref.html#fun")))
        '(= '("#" "a" "p" "p" ":" "p" "w") (convHref "app.html" (chop "#pw")))
        '(= (chop "#form-refA:") (convHref "form/form.html" (chop "refA.html")))
        '(= (chop "#form-refA:+Able") (convHref "form/refB.html" (chop "refA.html#+Able")))
        '(= (chop "#app:") (convHref "form/form.html" (chop "../app.html")))
        '(= (chop "#app:charts") (convHref "form/refC.html" (chop "../app.html#charts")))
        '(= (chop "#form-form:fun") (convHref "toc.html" (chop "form/form.html#fun")))
        
        '(= (chop "fun.l") (convHref "tut.html" (chop "fun.l")))
        '(= (chop "http://soft.de/down.html") (convHref "ref.html" (chop "http://soft.de/down.html")))
        
        '(= '("a" "b" "c") (unquote (chop "\"abc\"")))
        '(= '((("p" "1") "\"" "v" "1" "\"") (("p" "2") "\"" "v" "2" "\"") (("p" "3") "\"" "v" "3" "\""))
            (propValPairs (chop " p1 = \"v1\"  p2= \"v2\" p3 =\"v3\" ")) )
        ) )
    (prinl "Tests: " (+ Goods Bads) ", Errors: " Bads) ) )

(de convAllFiles (Flist)
  (out *Fout   # /Volumes/P3/Pico/doc/one.html
    (for F Flist (convFile F T)) )
  (unless *Fout
    (printNamesAndReuse)
    #(printHrefIdx "somePath/hrefs.txt")
    (doTests) ) )

(convAllFiles (filesToRead))

(bye)
