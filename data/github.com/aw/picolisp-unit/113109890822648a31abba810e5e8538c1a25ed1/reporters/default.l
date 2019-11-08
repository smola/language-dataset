# reporters/default.l
#
# The MIT License (MIT)
#
# Copyright (c) 2017 Alexander Williams, Unscramble <license@unscramble.jp>

(de *Colours  ("red"    . 1)
              ("green"  . 2)
              ("yellow" . 3)
              ("blue"   . 4) )


[de print-report ()
  (mapcar '((N) (prin (align 3 (cadr N)) ") ")
                (if (=T (car N))
                    (print-passed (caddr N))
                    (print-failed (caddr N)) ) )
          *Results )

  (let (Passed_results (get-results T)
        Failed_results (get-results) )

    (prinl  (colour "bold")
            "----^J"
            (colour "green")
            (length Passed_results)
            " test" (plural? Passed_results)
            " passed, "
            (colour "red")
            (length Failed_results)
            " test" (plural? Failed_results)
            " failed"
            (colour) )

    (report-failed Failed_results)
    (exit-gracefully Failed_results) ]

[de print-passed @
  (prinl  (colour "green")
          (char (hex "2713"))
          (align 2 " ")
          (colour)
          (next) ) ]

[de print-failed (Message)
  (prinl  (colour "red")
          (char (hex "2715"))
          (align 2 " ")
          Message
          (colour) ]

(de report-failed (Failed_results)
  (when Failed_results
    (prinl "^J" (colour "red") "^J  Failed test" (plural? Failed_results) ": ")

    (mapcar
      '((N) (print-error (cadr N) (caddr N))
            (print-expected (; N 4))
            (print-got (; N 5)) )
      Failed_results )

    (colour) ]

[de print-error (Number Result)
  (prinl  (colour "red")
          "^J"
          (align 4 " - ")
          Number
          ")  "
          Result
          (colour) ]

[de print-expected (Result)
  (prin (align 8 " ")
        "Expected: "
        (colour "green") )

  (println Result)
  (colour) ]

[de print-got (Result)
  (prin (align 8 " ")
        (align 10 " Actual: ")
        (colour "red") )

  (println Result)
  (colour) ]

[de exit-gracefully (Failed_results)
  (if Failed_results
      (bye 1)
      (bye) ]
