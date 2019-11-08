\\
\\ AJY 2016-01-09
\\
\\ Vector and array ops for ANN software
\\

\\ ------------------------------------------------------------
\\ Aux array operations

\\ First load Ramil Farkhshatov's for macro:
\\ (load "for.shen")
\\ Then load this file:
\\ (load "array.shen")


(define outer-product
  { vector --> vector --> vector }
  Y X -> 
    (let
      XM (limit X) \\ M columns in the result vector
      YN (limit Y) \\ N rows in the result vector
      VEC (vector YN) \\ resulting output array (vector of vectors)
      (do \\ Build result array, then return it
        (for (IndxY 1 1 YN) \\ Build vert rows (vectors) one by one
          (let 
            VEC2 (vector XM) \\ One vertical row (products vector)
            (do
              (for (IndxX 1 1 XM)
                (vector-> VEC2 
                          IndxX
                          (* (<-vector Y IndxY)
                             (<-vector X IndxX)))) \\ Outer prod elems
              (vector-> VEC IndxY VEC2)))) \\ Fill row into array
         VEC) \\ Return the resulting vector of vectors
       ) \\ end (let ...)
     ) \\ end function



(define scalar-mult
  { number --> vector --> vector }
  C V -> 
  (let 
    YN (limit V) \\ Number of vertical rows
    XM (limit (<-vector V 1)) \\ Number of horizontal columns
    (do \\ First compute the scalar product, then return it
      (for (IndxY 1 1 YN)
        (let
          ROW (<-vector V IndxY)
          (do \\ Compute row, then assign it
            (for (IndxX 1 1 XM)
              (let 
                YNXM (* (<-vector ROW IndxX) \\ Compute scalar product
                        C)
                (vector-> ROW IndxX YNXM)))
            (vector-> V IndxY ROW))) \\ end (let ...)
        ) end \\ (for (IndxY 1 1 N) ...)
      V \\ return the scalar multiplied vector V
      ) \\ end (do ...)
    ) \\ end (let ...)
) \\ end function



(define output-array
  { vector --> (list A) }
  \\ Prettyprint a 2-D array, a vector of vectors
  \\ Return []
  V -> 
    (let
      Ydim (limit V) \\ # of rows
      Xdim (limit (<-vector V 1)) \\ # of columns
      _ (output "~%<")
      _ (output "~A~%" (<-vector V 1))
      _ (for (Indx 2 1 Ydim)
          (output " ~A~%" (<-vector V Indx)))
      _ (output ">~%")
      []) \\ end (let ...)
) \\ end (define output-array ...)


(define array-sum
  { vector --> vector --> vector }
  X Y ->
    (let
      XyM (limit X) \\ XyM rows in the first array
      YyM (limit Y) \\ YyM rows in the second array
      XxN (limit (<-vector X 1)) \\ XyN columns in the second array
      YxN (limit (<-vector Y 1)) \\ YyN columns in the second array
      _ (if (not (= XyM YyM)) (error "DIM error -- rows")
            skip)
      _ (if (not (= XxN YxN)) (error "DIM error -- columns")
            skip)
      VEC (vector XyM) \\ resulting output array (vector of vectors)
      (do \\ Build result array, then return it
        (for (IndxY 1 1 XyM) \\ Build vert rows (vectors) one by one
          (let 
            VEC2 (vector XxN) \\ One vertical row (products vector)
            _ (let
                XRow (<-vector X IndxY)
                YRow (<-vector Y IndxY)
                (for (IndxX 1 1 XxN) \\ Fill VEC2 with partial sums
                  (vector-> VEC2 
                            IndxX
                            (+ (<-vector XRow IndxX) \\ Sum elems
                               (<-vector YRow IndxX))))
               ) \\ end (let ...)
            (vector-> VEC IndxY VEC2) \\ Fill row into array
           ) \\ end (let ...)
        ) \\ end (for ...)
        VEC \\ Return the resulting vector of vectors
      ) \\ end (do ...)
    ) \\ end (let ...)
) \\ end function


\\ Transpose of a 2-D array

(define transpose
  { vector --> vector }
  A ->
    (let Ydim (limit A) \\ number of rows
         Xdim (limit (<-vector A 1)) \\ number of columns
         R (vector Xdim) \\ The result has Xdim rows
         _ (for (IndxX 1 1 Xdim) \\ First build the result array
             (vector-> R IndxX (vector Ydim))) \\ Fill IndxX'th row
         \\ Now the result array is there, and is full of (fail)
         \\ objects, actually.  Fill it with A's transpose
         _ (for (IndxY 1 1 Ydim) \\ For each row:
             (for (IndxX 1 1 Xdim) \\ For each column:
               \\ R(Xdim, Ydim) := A(Ydim, Xdim)
               (let
                 D (<-vector A IndxY) \\ Take IndxY'th row
                 Item (<-vector D IndxX) \\ This is the item
                 RR (<-vector R IndxX) \\ IndxX'th row in result
                 _ (vector-> RR IndxY Item) \\ Insert Item
                 (vector-> R IndxX RR) \\ Insert the new row
                ) \\ end (let ...)
             ) \\ end (for ...) 
           ) \\ end (for ...)
         R \\ Return the result from the (let ...) form
       ) \\ end (let ...)
) \\ end function



(define output-array-v \\ For Hopfield
  { vector --> (list A) }
  \\ Prettyprint a 2-D array, a vector of vectors
  \\ Return []
  V -> 
    (let
      Ydim (limit V) \\ # of rows
      Xdim (limit (<-vector V 1)) \\ # of columns
      _ (output "~%<")
      _ (output "~A~%~%" (<-vector V 1))
      _ (for (Indx 2 1 Ydim)
          (output " ~A~%~%" (<-vector V Indx)))
      _ (output ">~%")
      []) \\ end (let ...)
) \\ end (define output-array ...)

