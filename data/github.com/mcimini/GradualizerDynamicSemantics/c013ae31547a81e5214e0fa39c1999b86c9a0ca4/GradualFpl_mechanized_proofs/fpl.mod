module fpl.

typeOf (fold E R) (mu R) :- typeOf E (R (mu R)).

typeOf (absT R2) (all R) :- (pi x\ typeOf (R2 x) (R x)).

typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).

typeOf (emptyList T) (list T).

typeOf (inr T1 E) (sum T1 T2) :- typeOf E T2.

typeOf (inl T2 E) (sum T1 T2) :- typeOf E T1.

typeOf (pair E1 E2) (times T1 T2) :- typeOf E1 T1, typeOf E2 T2.

typeOf (ff ) (bool ).

typeOf (tt ) (bool ).

typeOf (succ E) (int ) :- typeOf E (int ).

typeOf (zero ) (int ).

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ typeOf x T1 => typeOf (R x) T2).

typeOf (unfold E) (R (mu R)) :- typeOf E (mu R).

stepOriginal (unfold (fold V R)) V :- value V.

typeOf (appT E T) (R T) :- typeOf E (all R).

stepOriginal (appT (absT R2) T) (R2 T).

typeOf (isnil T E) (bool ) :- typeOf E (list T).

stepOriginal (isnil T (emptyList T')) (tt ).

stepOriginal (isnil T (cons T V1 V2)) (ff) :- value V1, value V2.

typeOf (tail T E) (list T) :- typeOf E (list T).

stepOriginal (tail T (emptyList T')) (raise (list T) (succ (zero ))).

stepOriginal (tail T (cons T V1 V2)) V2 :- value V1, value V2.

typeOf (head T E) T :- typeOf E (list T).

stepOriginal (head T (emptyList T')) (raise T (zero )).

stepOriginal (head T (cons T V1 V2)) V1 :- value V1, value V2.

typeOf (case EE R1 R2) T :- typeOf EE (sum T1 T2), (pi x\ typeOf x T1 => typeOf (R1 x) T), (pi x\ typeOf x T2 => typeOf (R2 x) T).

stepOriginal (case (inl T V) E1 E2) (E1 V) :- value V.

stepOriginal (case (inr T V) E1 E2) (E2 V) :- value V.

typeOf (snd E) T2 :- typeOf E (times T1 T2).

stepOriginal (snd (pair V1 V2)) V2 :- value V1, value V2.

typeOf (fst E) T1 :- typeOf E (times T1 T2).

stepOriginal (fst (pair V1 V2)) V1 :- value V1, value V2.

typeOf (if E1 E2 E3) T :- typeOf E1 (bool ), typeOf E2 T, typeOf E3 T.

stepOriginal (if (tt ) E1 E2) E1.

stepOriginal (if (ff ) E1 E2) E2.

typeOf (isZero E) (bool ) :- typeOf E (int ).

stepOriginal (isZero (zero )) (tt ).

stepOriginal (isZero (succ V)) (ff ) :- value V.

typeOf (pred E) (int ) :- typeOf E (int ).

stepOriginal (pred (zero )) (raise (int) (zero )).

stepOriginal (pred (succ V)) V :- value V.

typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.

stepOriginal (app (abs T R) V) (R V) :- value V.

typeOf (let E R) T2 :- typeOf E T1, (pi x\ typeOf x T1 => typeOf (R x) T2).

stepOriginal (let V R) (R V) :- value V.

typeOf (letrec T1 R1 R2) T2 :- (pi x\ typeOf x T1 => typeOf (R1 x) T1), (pi x\ typeOf x T1 => typeOf (R2 x) T2).

stepOriginal (letrec T1 R1 R2) (R2 (fix (abs T1 R1))).

typeOf (fix E) T :- typeOf E (arrow T T).

stepOriginal (fix V) (app V (fix V)) :- value V.

typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (arrow (int ) T).

stepOriginal (try E1 E2) E1 :- value E1.

stepOriginal (try (raise T V) E)  (app E V) :- value V.

value (fold E1 U2) :- value E1.

value (absT R1).

value (cons T1 E2 E3) :- value E2, value E3.

value (emptyList T1).

value (inr T V) :- value V.

value (inl T E1) :- value E1.

value (pair E1 E2) :- value E1, value E2.

value (ff ).

value (tt ).

value (succ E1) :- value E1.

value (zero ).

value (abs T1 R2).

stepOriginal (fold E1 U2) (fold E1' U2) :- stepOriginal E1 E1'.

stepOriginal (cons T1 E2 E3) (cons T1 E2' E3) :- stepOriginal E2 E2'.

stepOriginal (cons T1 E2 E3) (cons T1 E2 E3') :- stepOriginal E3 E3', value E2.

stepOriginal (inr T E1) (inr T E1') :- stepOriginal E1 E1'.

stepOriginal (inl T E1) (inl T E1') :- stepOriginal E1 E1'.

stepOriginal (pair E1 E2) (pair E1' E2) :- stepOriginal E1 E1'.

stepOriginal (pair E1 E2) (pair E1 E2') :- stepOriginal E2 E2', value E1.

stepOriginal (succ E1) (succ E1') :- stepOriginal E1 E1'.

stepOriginal (unfold E1) (unfold E1') :- stepOriginal E1 E1'.

stepOriginal (appT E1 T2) (appT E1' T2) :- stepOriginal E1 E1'.

stepOriginal (isnil T1 E2) (isnil T1 E2') :- stepOriginal E2 E2'.

stepOriginal (tail T1 E2) (tail T1 E2') :- stepOriginal E2 E2'.

stepOriginal (head T1 E2) (head T1 E2') :- stepOriginal E2 E2'.

stepOriginal (case E1 R2 R3) (case E1' R2 R3) :- stepOriginal E1 E1'.

stepOriginal (snd E1) (snd E1') :- stepOriginal E1 E1'.

stepOriginal (fst E1) (fst E1') :- stepOriginal E1 E1'.

stepOriginal (if E1 E2 E3) (if E1' E2 E3) :- stepOriginal E1 E1'.

stepOriginal (isZero E1) (isZero E1') :- stepOriginal E1 E1'.

stepOriginal (pred E1) (pred E1') :- stepOriginal E1 E1'.

stepOriginal (app E1 E2) (app E1' E2) :- stepOriginal E1 E1'.

stepOriginal (app E1 E2) (app E1 E2') :- stepOriginal E2 E2', value E1.

stepOriginal (let E1 R) (let E1' R) :- stepOriginal E1 E1'.

stepOriginal (fix E1) (fix E1') :- stepOriginal E1 E1'.

stepOriginal (try E1 E2) (try E1' E2) :- stepOriginal E1 E1'.

stepOriginal (raise T E1) (raise T E1') :- stepOriginal E1 E1'.

error (raise T E1).

typeOf (raise T E) T :- typeOf E (int ).

stepOriginal E (raise T1 E1) :- typeOf E T1, containsError E (raise T2 E1).

nstep E E.

nstep E1 E3 :- step E1 E2, nstep E2 E3.

containsError E E.
containsError (app E1 E2) E :- containsError E1 E.
containsError (app E1 E2) E :- containsError E2 E, value E1.
containsError (if E1 E2 E3) E :- containsError E1 E.
containsError (succ E1) E :- containsError E1 E.
containsError (pred E1) E :- containsError E1 E.
containsError (isZero E1) E :- containsError E1 E.
containsError (pair E1 E2) E :- containsError E1 E.
containsError (pair E1 E2) E :- containsError E2 E, value E1.
containsError (fst E1) E :- containsError E1 E.
containsError (snd E1) E :- containsError E1 E.
containsError (inr T E1) E :- containsError E1 E.
containsError (inl T E1) E :- containsError E1 E.
containsError (case E1 E2 E3) E :- containsError E1 E.
containsError (cons E1 E2 E3) E :- containsError E2 E.
containsError (cons E1 E2 E3) E :- containsError E3 E, value E2.
containsError (head E1 E2) E :- containsError E2 E.
containsError (tail E1 E2) E :- containsError E2 E.
containsError (isnil E1 E2) E :- containsError E2 E.
containsError (appT E1 E2) E :- containsError E1 E.
containsError (fold E1 E2) E :- containsError E1 E.
containsError (unfold E1) E :- containsError E1 E.
containsError (let E1 R) E :- containsError E1 E.
containsError (fix E1) E :- containsError E1 E.
containsError (raise T E1) E :- containsError E1 E.


