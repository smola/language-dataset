;; Read-Eval-Print Loop
;;
repl = fn(prompt, inport,
  "IScheme version 2.0\n" println
  prompt print
  loop(
    bind(
      ;; error handling function
      rescue(fn(con,
      "#{con kind}: #{con report}" println
      prompt print)),

      v = Parser parse(inport)
      val = eval(v)
      to_lisp(val) println
      prompt print
    )
  )
)

;; Turn result back into a Lisp expression
;;
to_lisp = method(x,
  cond(
    x == true,
    "#t",

    x == false,
    "#f",

    x kind?("Symbol"),
    x,

    x kind?("Text"),
    x,

    x kind?("List"),
    "(" + x map!(x, to_lisp(x)) join(" ") + ")",

    ;; default
    x asText
  )
)

;; PROBLEM: Ioke's only function for reading from the STDIN
;; stream doesn't return raw input; it interprets it as
;; Ioke code and adds parens around operands
;;
;; Example: (* 3 3) becomes (*(3 3))
;;
;; See if string contains "<operator>(" pattern, then remove it
;; Ignore if its a token like "#t" or "#f"
;;
sanitize = fn(string,
  pattern = #/([-\*\+=><%\/#]{1,2})\(/
  while(m = pattern match(string),
    op = m captures[0]
    matched = m match
    split = string split(matched)
    if(split length > 1,
      ;; remove one set of parens at a time, starting from the left
      ;;
      start = split[0] length
      ;; if op is not a math operator, don't add a space after it
      ;; regexp matches anything NOT an operator
      ;;
      sanitized = if(#/[^><=%\+-\/\*]/ match(op),
        string[start..-1] replace(matched, op) replace(")", ""),
        string[start..-1] replace(matched, op + " ") replace(")", "")
      )
      string = split[0] + sanitized,
      ;; else, just dealing with a single expression
      ;; like "(+(3 3))"and can do it in a simple way
      ;;
      string = string replace(op + "(", op + " ") replace(")", "")
    )
  )
  string
)

IO do(
  readLine = method(sanitize(self read asText))
)

Text do(
  startsWith? = method(char,
    self chars[0] == char
  )
)
