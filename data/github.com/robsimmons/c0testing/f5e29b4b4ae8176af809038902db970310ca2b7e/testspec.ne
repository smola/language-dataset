# Tests can be matched at the beginning of a file using the regular expression
# /(\/\/test.*\n)*/; the matching string then matches against this grammar.

Tests -> ("//test" (_ (Arg _):* "=>"):? _ Spec _ ("\r" | "\n" | "\r\n" | "\n\r")):*
Arg   -> "-l" [a-zA-Z_]:+
       | "-f" [a-zA-Z0-9_\.]:+
       | "-d"
       | "--no-purity-check"
       | "--standard" _ "=" _ Lang
Lang  -> "l1" | "L1" | "l2" | "L2" | "l3" | "L3" | "l4" | "L4" | "c0" | "C0" | "c1" | "C1" 
Spec  -> "return" _ ("-" | "~"):? [0-9]:+
       | "error_parse"
       | "error_typecheck"
       | "error_static"
       | "error"
       | "failure"
       | "div-by-zero"
       | "aritherror"
       | "infloop"
       | "abort"
       | "segfault"
       | "memerror"
       | "typecheck"
       | "compile"

_     -> " ":*
