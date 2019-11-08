require import Bool Option List.
require (*  *) ROM.

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

(* types *)
type stm. (* statement *)
type wit. (* witness   *)
type prf. (* proof     *) 

type g_in, g_out. (* input and output for random oracle G *)

(* operators *)
op dout: g_out distr.           (* distribution for random oracle G *)

(* random oracle *)
clone include ROM.Types with
  type from             <- g_in,
  type to               <- g_out,
  op   dsample(x: g_in) <- dout.

(* ---------------------------------------------------------------------- *)
(* Definitions *)

module type Relation ={
  proc main(s: stm, w: wit) : bool
}.

(* prover *)
module type Prover(O: ARO) ={
  proc prove(s : stm, w : wit) : prf {O.o}
}.

(* verifier *)
module type Verifier(O: ARO) ={
  proc verify(s:stm, p: prf): bool {O.o}
}.

(* proof system *)
module System(R: Relation, P:Prover, V:Verifier, O: ARO) = {
  proc relation = R.main
  proc prove  = P(O).prove
  proc verify = V(O).verify
}.

(* simulator *)
module type Simulator = {  
  proc init()          : unit
  proc o   (x : g_in)  : g_out
  proc prove (s : stm) : prf
}.

(* state of experiment  *)
module BPS = {
  var s : stm
  var w : wit
  var p : prf option
  var b : bool
  var rel: bool
}.

(* ---------------------------------------------------------------------- *)
(* Security concepts *)

(* 1. Correctness property *)
module Correctness(R: Relation, P : Prover, V : Verifier, O : Oracle) = { 

  proc main(s : stm, w : wit) : bool = { 
    var p, b;
    O.init();
    BPS.rel <@ R.main (s,w);
    b <- false;
    if (BPS.rel ) {
      p <@ P(O).prove(s, w);
      b <@ V(O).verify(s, p);
    }
    return b;
  }
}.

(* zero-knowledge adversary *)
module type AdvZK(O : ARO) = {
  proc a1()               : stm * wit {O.o}
  proc a2(p : prf option) : bool      {O.o}
}.

(* 2. Zero-Knowledge property *) 
module ZK_L(R: Relation, P : Prover, A : AdvZK, O : Oracle) = {

  proc main(): bool = {
    var p;
    O.init();
    (BPS.s, BPS.w) <@ A(O).a1();
    BPS.rel <@ R.main (BPS.s, BPS.w);
    if (BPS.rel) {
      p     <@ P(O).prove(BPS.s, BPS.w);
      BPS.p <- Some p;
    } else {
      BPS.p <- None;
    }
    BPS.b <@ A(O).a2(BPS.p);
    return BPS.b;
  }
}.

module ZK_R(R: Relation, S:Simulator, A:AdvZK) = {

  proc main(): bool = {
    var p;
    S.init();
    (BPS.s, BPS.w) <@ A(S).a1();
    BPS.rel <@ R.main (BPS.s, BPS.w);
    if (BPS.rel) {
      p     <@ S.prove(BPS.s);
      BPS.p <- Some p;
    } else {
      BPS.p <- None;
    }
    BPS.b <@ A(S).a2(BPS.p);
    return BPS.b;
  }
}.


(* ---------------------------------------------------------------------- *)
(* Typecheck advantage *)

section.
  require import Real.

  declare module O : Oracle.
  declare module R : Relation. 
  declare module P : Prover.
  declare module V : Verifier.
  declare module S : Simulator.
  declare module AZ: AdvZK.
  
  local lemma corr(s : stm, w : wit) &m:
    exists eps,
      0%r < eps /\
      Pr[Correctness(R, P, V, O).main(s,w) @ &m: res ] >= 1%r - eps by [].

  local lemma zero_knowledge &m:
    exists eps, 
      `| Pr[ZK_L(R, P, AZ, O).main() @ &m: res] -
         Pr[ZK_R(R, S, AZ).main() @ &m: res] | <= eps by []. 
end section.