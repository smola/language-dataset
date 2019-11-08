(\* functions-and-types.shen *\)
(\* Mac Radigan *\)
(\* adapted from Dr. Mark Tarver's examples at http://shenlanguage.org *\)
(\* see http://shenlanguage.org/learn-shen/index.html#9.2%20Functions%20and%20Types *\)

  (\* enable type checking *\)
  (tc +)

  (define member
    {A --> (list A) --> boolean}
    _ [] -> false
    X [X | _] -> true
    X [_ | Y] -> (member X Y))

  (define square
    {number --> number}
    X -> (* X X))

  (define swap
    {(A * B) --> (B * A)}
    (@p X Y) -> (@p Y X))
  
  (define unit-vector?
   {(vector A) --> boolean}
   (@v _ <>) -> true
        _ -> false)

  (define unit-string?
    {string --> boolean}
    (@s _ "") -> true
    _ -> false)

  (member 1 [1 2 3])

  (square 4)

  (swap (@p 1 2))

  (unit-vector? (@v 1 <>))

  (unit-string? "a")

(\* *EOF* \*)
