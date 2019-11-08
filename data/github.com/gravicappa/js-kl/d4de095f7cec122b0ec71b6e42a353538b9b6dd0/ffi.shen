(package js [this]

(define cut-package'
  "" W -> W
  (@s "." Cs) W -> (cut-package' Cs "")
  (@s C Cs) W -> (cut-package' Cs (@s W C)))

(define cut-package
  S -> (cut-package' S "") where (string? S)
  S -> (intern (cut-package' (str S) "")))

(define item
  X _ -> (cut-package (str X)) where (symbol? X)
  X _ -> (str X) where (number? X)
  X _ -> (esc-obj X) where (string? X)
  [X | Xs] C -> (expr [X | Xs] 1 C))

(define ffi-call-args'
  [] _ R -> R
  [X | Xs] C R -> (ffi-call-args' Xs C (s [R ", " (ffi-expr X C)])))

(define ffi-call-args
  [] _ -> "()"
  [X] C -> (s ["(" (ffi-expr X C) ")"])
  [X | Xs] C -> (s ["(" (ffi-expr X C) (ffi-call-args' Xs C "") ")"]))

(define ffi-call
  F Args C -> (s ["(" (ffi-expr F C) ")" (ffi-call-args Args C)]))

(define ffi-new
  Class Args C -> (cn "new " (ffi-call Class Args C)))

(define ffi-obj-key
  X -> (str X) where (symbol? X)
  X -> (error "~A is not appropriate js object key" X))

(define ffi-obj'
  [] _ Pairs -> (s ["{" (arg-list (reverse Pairs)) "}"])
  [K V | Items] C Pairs -> (let Pair (s [(ffi-obj-key K) ": " (ffi-expr V C)])
                             (ffi-obj' Items C [Pair | Pairs])))

(define ffi-obj
  Items C -> (ffi-obj' Items C []))

(define ffi-arr
  Items C -> (s ["[" (arg-list (map (/. X (ffi-expr X C)) Items)) "]"]))

(define ffi-set
  Dst Src C -> (s ["(" (ffi-expr Dst C) " = " (ffi-expr Src C) ")"]))

(define ffi-chain-item
  X C R -> (s [R "." (item X C)]) where (symbol? X)
  X C R -> (s [R "[" (ffi-expr X C) "]"]))

(define ffi-chain
  [] _ R -> R
  [[js.call F | A] | Xs] C R -> (ffi-chain Xs C (cn (ffi-chain-item F C R)
                                                    (ffi-call-args A C)))
  [X | Xs] C R -> (ffi-chain Xs C (ffi-chain-item X C R)))

(define ffi-expr
  [js. X | Xs] C -> (ffi-chain Xs C (ffi-expr X C))
  [js.call F | Args] C -> (ffi-call F Args C)
  [js.set Dst Src] C -> (ffi-set Dst Src C)
  [js.new Class | Args] C -> (ffi-new Class Args C)
  [js.obj | Xs] C -> (ffi-obj Xs C)
  [js.arr | Xs] C -> (ffi-arr Xs C)
  X C -> (item X C)))
