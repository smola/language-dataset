(datatype lns-expr

  N : number;
  ___________________
  [num N] : num-expr;

  S : string;
  ___________________
  [str S] : str-expr;

  E1 : num-expr; E2 : num-expr;
  _____________________________
  [plus E1 E2] : num-expr;

  E1 : num-expr; E2 : num-expr;
  _____________________________
  [times E1 E2] : num-expr;

  E1 : str-expr; E2 : str-expr;
  _____________________________
  [cat E1 E2] : str-expr;

  E : str-expr;
  ___________________
  [len E] : num-expr;

  E1 : T1;
  X : T1 >> E2 : T2;
  ___________________
  [let E1 X E2] : T2;)
