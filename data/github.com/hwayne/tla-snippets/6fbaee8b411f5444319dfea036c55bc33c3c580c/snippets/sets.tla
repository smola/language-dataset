\* Easier adding and removing elements of a set
\* 1..3 -- 1 -- 2 = {3}
\* {} ++ 1 ++ 3 == {1, 3}
set -- elem == set \ {elem}
set ++ elem == set \union {elem}

Empty(set) == set = {}

\* REDUCE
Pick(set) == CHOOSE s \in set : TRUE

RECURSIVE SetReduce(_, _, _)
SetReduce(Op(_, _), set, value) == 
  IF set = {} THEN value
  ELSE 
    LET s == Pick(set)
    IN SetReduce(Op, set \ {s}, Op(s, value)) 

\* Only defined for integers
SetSum(set) == 
  LET _op(a, b) == a + b
  IN SetReduce(_op, set, 0)

SetMax(set) == 
  CHOOSE x \in set: 
    \A y \in set: 
      y <= x

SetMin(set) == 
  CHOOSE x \in set: 
    \A y \in set: 
      y >= x

\* Useful over structures, but not determinism-safe
\* ex SetMaxBy(set_of_items, LAMBDA item: item.price)
SetMaxBy(set, Op(_)) ==
  CHOOSE x \in set:
    \A y \in set:
      Op(y) <= Op(x)

\* Useful for duplicating model values
SetOf(type, count) == {<<type, n>>: n \in 1..count}
