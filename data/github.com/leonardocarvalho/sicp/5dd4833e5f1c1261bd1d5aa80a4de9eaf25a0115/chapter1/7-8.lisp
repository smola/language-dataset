(define (improve2 guess x) (/ (+ (/ x guess) guess) 2))
(define (good-enough-var? old-guess new-guess x)
  (define (rel-error x y) (abs (- (/ x y) 1)))
  (< (rel-error new-guess old-guess) 0.01)
)

(define (newton-method-step improve test previous-guess guess x)
  (if (test previous-guess guess x)
      guess
      (newton-method-step improve test guess (improve guess x) x)
  )
)

(define (newton-method improve test guess x)
  (newton-method-step improve test (- guess) guess x)
)

(define (sqrt-fixed guess x)
  (define (square x) (* x x))
  (define (good-enough-fixed? previous-guess guess x) (< (abs (- (square guess) x)) 0.001))
  (newton-method improve2 good-enough-fixed? guess x)
)

(define (sqrt-var guess x)
  (newton-method improve2 good-enough-var? guess x)
)

(define (cube-root guess x)
  (define (improve3 guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (newton-method improve3 good-enough-var? guess x)
)
