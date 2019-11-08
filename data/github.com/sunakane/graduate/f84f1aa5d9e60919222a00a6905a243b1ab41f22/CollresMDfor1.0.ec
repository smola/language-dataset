require import AllCore DBool List Int IntExtra StdOrder.
require BitWord.

(* The type of plaintexts: bitstrings of length k, l *)
op k: { int | 0 < k } as gt0_k.
op l: { int | 0 < l } as gt0_l.

clone import BitWord as BitsK with
  op n <- k
proof gt0_n by exact/gt0_k
rename
  "word" as "bits_k"
  "dunifin" as "dbits_k".

clone import BitWord as BitsL with
  op n <- l
proof gt0_n by exact/gt0_l
rename
  "word" as "bits_l"
  "dunifin" as "dbits_k".

import DWord.

type block = bits_k.
type state = bits_l.
type msg = bool list.

const IV: state.
 
const z0: block.
(** axiom non_z0: forall(xs: block list), (xs <> []) => (head z0 xs) <> z0. **)

(** f: ハッシュ関数 **)
op f : (block * state) -> state.
op fstar : (block list * state) -> state.
op pad : msg -> block list.
op MD (m: msg) = fstar ((pad m), IV).

op coll(xy, xy': block * state) =
  xy <> xy' && f(fst(xy'), snd(xy')) = f((fst(xy'), snd(xy'))).
 
axiom suffix_free:
  forall (m, m': msg, bl: block list), m <> m' => pad(m) <> bl ++ pad(m').

axiom fstar_nil:
  forall (y: state), fstar([], y) = y.

axiom fstar_cons:
  forall (x: block, y: state, xs: block list), fstar((x::xs), y) = fstar(xs, f(x, y)).

op uMsg: (msg * msg) distr.

module type OR = {
  proc init(): msg * msg
}.

module Or : OR= {
  proc init(): msg * msg = {
    var m;
    m <$ uMsg;
    return m;
  }
}.

module type ADV(O: OR) = {
  proc gen() : msg * msg
}.

module (Adv : ADV) (O : OR) = {
  proc gen() : msg * msg = {
    var message;
    message <@ O.init();
    return message;
  }
}.

module CR_MD = {
  proc oracle_f(m: msg) : state = {
    var xs: block list <- pad(m);
    var y : state <- IV;
    while (xs <> []) {
      y <- f((head z0 xs), y);
      xs <- behead(xs);
    }
    return y;
  }

  proc main() : bool = {
    var m1, m2 : msg;
    var h1, h2 : state;
    (m1, m2) = $uMsg;
    h1 <@ oracle_f(m1);
    h2 <@ oracle_f(m2);
    return (m1 <> m2 && h1 = h2);
  }
}.

print module Adv.

module CR_f = {

  proc adv_b() : (block * state) * (block * state) = {
    var m1, m2 : msg;
    var y1, y2 : state;
    var xs', xs1, xs2 : block list;

    (m1, m2) <$ uMsg;
    xs1 <- pad(m1); y1 <- IV;
    xs2 <- pad(m2); y2 <- IV;
    xs' <- [];
    
    while (size xs2 < size xs1) {
      y1 <- f((head z0 xs1), y1);
      xs' <- xs' ++ [head z0 xs1];
      xs1 <- behead xs1;
    }
    while (size xs1 < size xs2) {
      y2 <- f((head z0 xs2), y2);
      xs' <- xs' ++ [head z0 xs2];
      xs2 <- behead xs2;
    }
    while (! (coll((head z0 xs1), y1) ((head z0 xs2), y2)) && xs1 <> [] ) {
      y1 <- f((head z0 xs1), y1); xs1 <- behead xs1;
      y2 <- f((head z0 xs2), y2); xs2 <- behead xs2;
    }
    return (((head z0 xs1), y1), ((head z0 xs2), y2));
  }

  proc main(): bool = {
    var xy1, xy2: block * state;
    (xy1, xy2) <@ adv_b();
    return coll xy1 xy2;
  }
}.

axiom size_behead : forall (xs: 'a list),
  0 < size xs => size(behead xs) < size xs.
  
prover ["Alt-Ergo"].
section Security.
  lemma MD_Collision_Resistance :
    equiv [CR_MD.main ~ CR_f.main : true ==> res{1} => res{2}].
  proof.
  proc. inline CR_f.adv_b. seq 1 1 : (={m1, m2}). auto.
  seq 1 0 : ((h1 = MD(m1)){1} && ={m1, m2}). inline *. wp.
  while{1} (fstar(xs, y) = MD(m1)){1} (size xs{1}). progress. auto. smt(fstar_cons). auto. progress. smt. rewrite fstar_nil in H. exact H.
  seq 1 0 : ((h1 = MD(m1) && h2 = MD(m2)){1} && ={m1, m2}). inline*. wp.
  while{1} (fstar(xs, y) = MD(m2)){1} (size xs{1}). move => &1 z. auto. smt(fstar_cons). auto. progress. smt. rewrite fstar_nil in H. exact H.
  sp.
wp.
(* m1 = m2 *)
case (m1{2} = m2{2}).
while{2} true (size xs1{2}). 
progress. wp. skip. progress. smt.
rcondf{2} 1 => //.
rcondf{2} 1 => //.
skip. progress. smt.

(* m1 <> m2 /\ size xs1 <= size xs2 *)
case (size xs1{2} <= size xs2{2}).
rcondf{2} 1. progress. skip. progress. smt.
while{2} (fstar(xs1, y1) = MD(m1) && fstar(xs2, y2) = MD(m2) && (xs1 = xs2 => y1 <> y2) && size xs1 = size xs2){2} (size xs1){2} => //.
progress. auto. progress. smt(fstar_cons). smt.  smt. smt. smt(size_behead). (** apply size_behead. apply size0_nonnil. exact H4. **)
while{2} (fstar(xs2, y2) = MD(m2) && xs' ++ xs2 = pad(m2) && size xs1 <= size xs2){2} (size xs2 - size xs1){2}.
progress. auto. progress. rewrite - fstar_cons. rewrite head_behead. smt. exact H. rewrite - H0. smt. smt. smt. skip. 
progress. rewrite subz_le0 in H4. smt. smt. smt. smt.
simplify. smt.
(* m1 <> m2 /\ size xs2 < size xs1 *)
wp.
while{2} (fstar(xs1, y1) = MD(m1) && fstar(xs2, y2) = MD(m2) && size xs1 = size xs2 && (xs1 = xs2 => y1 <> y2)){2} (size xs1){2}. auto. progress. rewrite -H. rewrite - fstar_cons. rewrite head_behead. apply H4. reflexivity. rewrite -H0. rewrite -fstar_cons. rewrite head_behead. smt. reflexivity. smt. smt. smt(size_behead).
seq{2} 0 1 : 
    ((xs2{2} = pad m2{2} /\
    y2{2} = IV /\
    xs'{2} ++ xs1{2} = pad m1{2} /\ (h1{1} = MD m1{1} && h2{1} = MD m2{1}) && ={m1, m2}) /\
   m1{2} <> m2{2} /\ size xs1{2} = size xs2{2} /\ fstar(xs1, y1){2} = MD(m1){2}).
while{2} (fstar(xs1, y1) = MD(m1) && xs' ++ xs1 = pad(m1) && size xs2 <= size xs1){2} (size xs1){2}.
progress. auto. progress. rewrite - H. rewrite - fstar_cons. rewrite head_behead. smt. reflexivity. smt. smt. smt.
skip. progress. smt. smt. smt.
rcondf{2} 1. progress. skip. progress. rewrite - H1. trivial.
skip. progress. smt. smt. simplify. smt.
qed.
end section Security.
