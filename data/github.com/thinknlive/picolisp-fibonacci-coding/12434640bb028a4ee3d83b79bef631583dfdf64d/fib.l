# Fibonacci codes (recursive loop solution)
(de fibn2 (N)
   (let
      _Aux '((F0 FN I N)
         (cond
            ((= I N) FN)
            (T (_Aux FN (+ F0 FN) (inc I) N)) ) )
      (_Aux 1 1 1 N) ) )

# Fibonacci encode (iterative loop solution)
(de fibn (N)
   (let (F (list 2 1 1))
      (cond
         ((= N 0) 0)
         ((= N 1) (caddr F))
         ((= N 2) (cadr F))
         (T
            (do (- N 3)
               (rot F)
               (set F (+ (cadr F) (caddr F)))
               (println F)
            )
            (car F))
         ) ) )

# Fibonacci codes for binary fibonacci encoding/decoding
# Provide B as an idx tree to cache prior looked up results

(de fibe (N B)
   (let (F (list 3 2 1) Fn NIL Fn1 NIL Fn2 NIL)
      (when B
         (setq
            Fn (lup B N)
            Fn1 (lup B (- N 1))
            Fn2 (lup B (- N 2)) ) )
      (cond
         (Fn (cadr Fn))
         ((and Fn1 Fn2)
            (setq Fn (+ (cadr Fn1) (cadr Fn2)))
            (idx 'B (list N Fn) T)
            Fn )
         (T (setq Fn
               (cond
                  ((= N 0) 0)
                  ((= N 1) (caddr F))
                  ((= N 2) (cadr F))
                  (T
                     (do (- N 3)
                        (rot F)
                        (set F (+ (cadr F) (caddr F))) )
                     (car F) ) ) )
            (when B
               (idx 'B (list N Fn) T) )
            Fn ) )
      ) )

# input : n, a positive integer
# output: Fn, encoded number by Fibonacci code of order 2 with LFn length
#  p ← 0 ;
#  while Fp ≤ n do p ←p +1;
#  p ← p − 1;
#  Fn ← 1;
#  LFn ← 1;
#  while p ≥ 0 do
#     Fn ← Fn << 1;
#     if Fp ≤ n then
#        Fn ← Fn | 1;
#        n ← n − Fp;
#     end
#     LFn ←LFn +1;
#     p ← p − 1;
#  end

(de fibEncode (N)
   (let (P 0 Fn (list 1) Fp 0 Fc NIL)
      (idx 'Fc (list 0 0) T)
      (while (<= (fibe P Fc) N)
         (inc 'P) )
      (dec 'P)
      (while (>= P 0)
         (setq
            Fn (cons 0 Fn)
            Fp (fibe P Fc) )
         (when (<= Fp N)
            (set Fn 1)
            (setq N (- N Fp)) )
         (dec 'P) )
      (cddr Fn) ) )
