(define memo-invoke
  Thunk ->
    (if (<-vector Thunk 2)
      (<-vector Thunk 3)
      (let Result (thaw (<-vector Thunk 1))
        (do
          (vector-> Thunk 2 true)
          (vector-> Thunk 3 Result)
          Result))))

(define memo
  doc "Memoizes a 0-parameter continuation."
  Continuation ->
    (let Thunk (vector 3)
      (do
        (vector-> Thunk 1 Continuation)
        (vector-> Thunk 2 false)
        (freeze
          (memo-invoke Thunk)))))

(declare memo [[--> A] --> [--> A]])
