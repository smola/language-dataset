(datatype forall

  let C (subst (gensym &&) A B)
  X : C;
  ____________________________
  X : (mode (forall A B) -);


  (scheme A B S V);
  X : S >> P;
  _______________________________
  X : (mode (forall A B) -) >> P;

  !;
  _________________
  (scheme A A V V);

  !;
  (scheme A B D F);
  (scheme A C E F);
  _____________________________
  (scheme A (B | C) (D | E) F);

  _________________
  (scheme A B B _);
  )

(define foo
  {(forall A (A --> A)) --> (number * symbol)}
  F -> (@p (F 1) (F a)))