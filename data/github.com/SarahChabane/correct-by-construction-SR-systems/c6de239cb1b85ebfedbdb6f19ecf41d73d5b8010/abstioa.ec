(**************************************************************************)
(*                                                                        *)
(*  Correct By construction Synchronous reactive systems                  *)
(*  Copyright (C) 2018                                                    *)
(*  Sarah Chabane               &   Rabea Ameur-Boulifa                   *)
(*  Limose Laboratory-Boumerdes     LTCI-Télécom ParisTech                *)(**************************************************************************)
 
require import AllCore. 
require import Int.
require import FSet.
require import List.

type label = [io|bio|bibo|ibo].
type 'a state = 'a.
type 'a transition = 'a*label*'a.
type 'a ioa = 'a*'a fset*'a transition fset.

(* Auxiallary functions *)

op extractFirst: 'a *'b *'c  -> 'a.
op extractSecond: 'a *'b *'c  -> 'b.
op extractLast: 'a *'b *'c  -> 'c.


(* Functions manipulating automata /transition/state *)

op init_state: 'a ioa -> 'a 
             = fun(A:'a ioa) => extractFirst(A).
op trans_set: 'a ioa -> 'a transition fset 
            = fun (A:'a ioa) => extractLast(A).
op state_set:'a ioa -> 'a fset 
            =fun(A:'a ioa) => extractSecond(A).

op is_init: 'a ioa -> 'a -> bool 
          = fun(A:'a ioa, s:'a) => s=init_state(A).


op src : 'a transition -> 'a 
       = fun (t:'a transition) => extractFirst(t).
op dst: 'a transition ->'a 
      = fun (t:'a transition) => extractLast(t).
op extractlabel: 'a transition ->label 
      = fun (t: 'a transition) => extractSecond(t).

      (* Reasoning about data *)

op oldest_data: 'a state -> int. 
op size: 'a state -> int. 

(*   Predicates about SR-Model   *)

(***Definition of determinism***)
pred is_determinist(A:'a ioa) = (forall(s:'a state) (t1 t2:'a transition), (mem (state_set(A)) s  /\ mem (trans_set(A)) t1 /\  mem (trans_set(A)) t2 /\ src t1=s /\src t2=s /\ dst t1<>dst t2 => ( (extractlabel t1)<> (extractlabel t2)))).


(*** Data is accepted since there is available memory ***)

pred in_until_full(A:'a ioa,m :int) = forall (s:'a state),
 FSet.mem  (state_set(A)) s /\ ((size s)< m) =>   exists(s':'a state), FSet.mem (trans_set(A)) (s,ibo,s').

(*** An output is possible when data reach latency ***)

pred out_when_ready(A:'a ioa, l :int ) = forall (s:'a state),
 FSet.mem  (state_set(A)) s /\ (oldest_data s)= l => exists(s' s'' :'a state ), (FSet.mem (trans_set(A)) (s,io,s') /\ FSet.mem (trans_set(A)) (s,bio,s'')).

(*** Definition of receptiveness ***)

pred is_receptiv (A:'a ioa, m l :int) = out_when_ready  A l /\ in_until_full A m. 

(*** No output is possible before waiting the duration of latency ***)

pred count_before_outing(A:'a ioa, l :int ) = forall (s:'a state),
 FSet.mem  (state_set(A)) s /\ ((oldest_data s) < l) => !exists(s':'a state), FSet.mem (state_set(A)) s' /\ (FSet.mem (trans_set(A)) (s,io,s') \/ FSet.mem (trans_set(A)) (s,bio,s')).

(*** No idleness is allowed before reaching the latency ***) 

 pred no_idle(A: 'a ioa,l:int) = forall (s:'a state),  FSet.mem (state_set(A)) s /\ s <> init_state A /\ oldest_data  s < l => exists(s':'a state), (FSet.mem (trans_set(A)) (s,bibo,s') /\ s<> s' ).
 
(***  Definition of well-formed SR-Model  ***)

pred is_wellformed(A:'a ioa, m l:int) = is_determinist(A) /\ (is_receptiv A m l )  /\ (in_until_full A m) /\ (out_when_ready A l) /\ (count_before_outing A l) /\ (no_idle A l).
