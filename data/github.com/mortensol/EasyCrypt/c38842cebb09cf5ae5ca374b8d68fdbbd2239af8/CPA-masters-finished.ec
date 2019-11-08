(* Part of a masters thesis written at NTNU, by Morten Solberg *)

(*** Import the theories (operators, lemmas, ...) we use throughout the proof ***)
require import AllCore DBool Distr FSet List StdOrder.
(*   *) import RealOrder. 

(*** Remove the SMT prover Alt-Ergo from the list of available provers due to a bug in this prover ***)
prover [-"Alt-Ergo"].  

(*** Introduce the types we will work with in the proof ***)
type X, Y, K, S, W.

(*** Define the sets X with a proper subset L (here named xs and ls) ***)
op xs : { X fset | is_lossless (duniform (elems xs)) /\ is_full (duniform (elems xs)) /\ is_uniform (duniform (elems xs)) } as xs_lfu. 
lemma xs_ful : is_lossless (duniform (elems xs)) /\ is_full (duniform (elems xs)) /\ is_uniform (duniform (elems xs)) by []. 

op ls : { X fset | ls < xs /\ is_lossless (duniform (elems ls)) /\ is_uniform (duniform (elems ls))} as ls_sub. 
lemma ls_ful : is_lossless (duniform (elems ls)) /\ is_uniform (duniform (elems xs)) by [].

(*** Perform a check that every element of type X is a member of xs ***)
lemma xdef : forall (x:X), x \in xs. 
proof.
move => ?. 
have : is_full (duniform (elems xs)) by smt. smt.
qed. 

(*** Some lemmas allowing us to chech that the subset ls behaves the way we want it to ***)
lemma sub1 : ls < xs by []. 
lemma sub2 : forall (x:X), x \in ls => x \in xs by smt(sub1). 
lemma sub3 : exists (x:X), x \in xs /\ !x \in ls by smt(sub1).   
lemma sub4 : xs `\` ls <> fset0 by smt. 
lemma sub5 : forall (x:X), x \in ls <=> !x \in xs `\` ls by [].
lemma sub6 : forall (x:X), !x \in ls <=> x \in xs `\` ls by []. 
lemma sub7 : ls `|` (xs `\` ls) = xs by smt unwantedlemmas=ls_ful. 
lemma sub8 : ls `&` (xs `\` ls) = fset0 by []. 

lemma mem1 : forall v, v \in (xs `\` ls) <=> (v \in xs) /\ !(v \in ls)
by smt. 

lemma mem2 : forall (x:X), x\in xs => x \in (xs `\` ls) <=> !(x \in ls).
proof. 
move => x H; split. 
smt.    
smt. 
qed.     

lemma mem3 : forall (x:X), x \in ls => !(x \in (xs `\` ls)).
proof. 
move => x; smt.  
qed. 

lemma mem4 : forall (x:X), x \in ls \/ x \in (xs `\` ls) by smt. 

(*** Equip the type K with a full, lossless, uniform distribution ***)
op dK : { K distr | is_lossless dK /\ is_full dK /\ is_uniform dK } as dK_lfu.
lemma dK_ful : is_lossless dK /\ is_full dK /\ is_uniform dK by []. 

(*** Define the set of witnesses along with operators allowing us to sample witnesses, 
     and an axiom saying that every x in L has a witness ***)
op ws : { W fset | is_lossless (duniform (elems ws)) /\ is_full (duniform (elems ws)) /\ is_uniform (duniform (elems ws)) } as ws_luf. 
lemma ws_ful : is_lossless (duniform (elems ws)) /\ is_full (duniform (elems ws)) /\ is_uniform (duniform (elems ws)) by []. 

op wit: W -> X. 
axiom wit1 (x:X) : x \in ls => exists (w:W), wit w = x. 

op iswit x = fun (w:W) => wit w = x. 

(*** Define abstract operators modeling the hash function, the projective funtion and
     alpha funtion making the projective key s ***)
op hash  : (K*X) -> Y. 
op alpha : K -> S.
op proj  : S*X*W -> Y. 

(*** Define the projective property of our hash functions ***)
axiom proj1 x k w : x \in ls => w \in (filter (iswit x) ws)  => 
   proj (alpha k,x,w) = hash(k,x). 
lemma projref x k s w : x \in ls => w \in (filter (iswit x) ws) => s = alpha k =>
    proj (s,x,w) = hash(k,x) <=> hash(k,x) = proj (s,x,w) by []. 

(*** Define a set ys consisting of all the elements of type Y ***)
op ys : { Y fset | is_lossless (duniform (elems ys)) /\ is_full (duniform (elems ys)) /\ is_uniform (duniform (elems ys))} as ys_lfu. 
lemma ys_ful : is_lossless (duniform (elems ys)) /\ is_full (duniform (elems ys)) /\ is_uniform (duniform (elems ys)) by [].  

(*** Define a module with procedures sampling elements from X, L, X\L and Y ***)
module Sampling = {
  proc fromX() : X = {
    var x;
    x <$ duniform (elems xs);
    return x;
  }

  proc fromL() : X*W = {
    var x,w;
    x <$ duniform (elems ls);
    w <$ duniform (elems (filter (iswit x) ws));
    return (x,w);
  }

  proc fromXnotL() : X = {
    var x;
    x <$ duniform (elems (xs `\` ls));
    return x;
  }

  proc fromY() : Y = {
    var y;
    y <$ duniform (elems ys);
    return y;
  }
}.

(*** Lemmas we use as a check that the sampling algorithms behave as we want them to ***)
lemma test1 &m : phoare[Sampling.fromX : true ==> mem xs res] = 1%r.
proof.  
proc; auto; smt. 
qed. 

lemma test2  &m : phoare[Sampling.fromL : true ==> mem xs res.`1] = 1%r.
proof. 
proc; auto; smt. 
qed. 

lemma test3  : phoare[Sampling.fromL : true ==> ! (mem (xs `\` ls) res.`1)] = 1%r.
proof. 
proc; auto; smt.
qed.

lemma test4  : phoare[Sampling.fromXnotL : true ==> ! (mem ls res)] = 1%r.
proof. 
proc; auto; smt.
qed.

lemma test5 : hoare[Sampling.fromL : true ==> res.`1 \in ls]. 
proof. 
proc; auto; smt. 
qed. 

(*** Define the algorithms of our hash proof system ***)
module HPS = {
  proc kg() : K = {
    var k;
    k <$ dK;
    return k;
  }

  proc seval(k) : S = {
    var s;
    s <- alpha k;
    return s;
  }

  proc priveval(k:K,x:X) : Y = {
    var y;
    y <- hash(k,x);
    return y;
  }

  proc pubeval(s,x,w) : Y = {
    var y;
    y <- proj(s,x,w);
    return y;
  }
}.

(*** Define the types we use for keys, plaintexts and ciphertexts in the encryption scheme ***)
type pkey = S.
type skey = K.
type plaintext = Y.
type ciphertext = X * Y.  

(*** Operators and axioms allowing us to add and subtract elements of type Y ***)
op toY   : int -> Y.
op toint : Y -> int.  
axiom y1 : forall(y:Y), toY (toint y) = y. 
axiom y2 : forall(y:int), toint (toY y) = y. 

(*** The encryption scheme itself ***)
module type Scheme = {
  proc keygen() : pkey * skey
  proc encrypt(pk:pkey, m:plaintext) : ciphertext
  proc decrypt(sk:skey, c:ciphertext) : plaintext
}.

module Genscheme : Scheme = {
  proc keygen() : pkey * skey = {
    var k, s, pk, sk;
    k  <- HPS.kg();
    s  <- HPS.seval(k);
    pk <- s; sk <- k;
    return (pk,sk);
  }

  proc encrypt(pk,m) : ciphertext = {
    var x, w, e, y, c;
    (x,w) <- Sampling.fromL();
    y     <- HPS.pubeval(pk,x,w);
    e     <- toY (toint m + toint y);
    c     <- (x,e);
    return c;
  }

  proc decrypt(sk,c:ciphertext) : plaintext = {
    var y, m';
    y  <- HPS.priveval(sk,c.`1);
    m' <- toY (toint c.`2 - toint y);
    return m';
  }
}.

(*** Proof of correctness for the encryption scheme ***)
module Correctness(S:Scheme) = {
  proc main(m:plaintext) : bool = {
    var pk, sk, c, m';
    (pk, sk)  <- S.keygen();
       c      <- S.encrypt(pk,m);
      m'      <- S.decrypt(sk,c);
    return (m' =  m);
  }
}.

lemma genscheme_correct &m : 
    hoare[Correctness(Genscheme).main : true ==> res].
proof.  
proc;inline*;auto; progress.
have -> : proj (alpha k0, x00, w00) = hash(k0, x00) by smt. 
smt(y1 y2). 
qed.

(*** Define an abstract CPA adversary ***)
module type CPAadversary = {
  proc choose(pk:pkey)     : plaintext * plaintext
  proc guess(c:ciphertext) : bool
}.

(*** The original CPA attack ***)
module CPA (S:Scheme, A:CPAadversary) = {
  proc main() : bool = {
    var pk, sk, c, m0, m1, b, b';
    (pk, sk) <- S.keygen();
    (m0,m1)  <- A.choose(pk);
       b     <$ {0,1};
       c     <- S.encrypt(pk,b?m1:m0);
       b'    <- A.guess(c);
    return (b=b');
  }
}.

(*** Define an abstract "smoothness adversary" ***)
module type SmoothAdversary = {
  proc guess(x:X,s:S, y:Y) : bool
}.

(*** Smoothness modules guessing either a hash value or a random value ***)
module Smooth1(A:SmoothAdversary) = {
  proc main() : bool = {
    var b, k, s, x;
    x <- Sampling.fromXnotL();
    k <- HPS.kg();
    s <- HPS.seval(k);
    b <- A.guess(x, s, hash(k,x));
    return b; 
  }
}.

module Smooth0(A:SmoothAdversary) = {
  proc main() : bool = {
    var b, k, s, x, y;
    x <- Sampling.fromXnotL();
    k <- HPS.kg();
    s <- HPS.seval(k);
    y <- Sampling.fromY();
    b <- A.guess(x,s,y);
    return b;
  }
}.

(*** Reduction from a CPA adversary to a smoothness adversary ***)
module SmoothAdv(A:CPAadversary) = {
  proc guess(x:X,s:S,y:Y) : bool = {
    var m0, m1, b, b';
    (m0,m1) <- A.choose(s);
       b    <$ {0,1};
       b'   <- A.guess(x, toY (toint (b?m1:m0) + toint y));
    return b=b';
  }
}. 

(*** SMP adversary guessing that x is from ls or from xs `\` ls ***)
module type SMPadversary = {
  proc guess(x:X) : bool
}.

module SMP1(A:SMPadversary) = {
  proc main() : bool = {
    var x, w, b;
    (x,w) <- Sampling.fromL();
      b   <- A.guess(x);
    return b;
  }
}.

module SMP0(A:SMPadversary) = {
  proc main() : bool = {
    var x, b;
    x <- Sampling.fromXnotL();
    b <- A.guess(x);
    return b;
  }
}.

(*** Reduction from a CPA adversary to a SMP adversary ***)
module SMPadv(A:CPAadversary) = {
  proc guess(x) : bool = {
    var b, b', m0, m1, pk, sk, y;
    (pk, sk) <- Genscheme.keygen();
    (m0, m1) <- A.choose(pk);
       y     <- hash(sk, x);
       b     <$ {0,1};
       b'    <- A.guess(x, toY (toint (b?m1:m0) + toint y));
    return (b = b');
  }
}.

(*** We now start the security proof, all simulation games will be defined inside this section ***)
section Security.

(*** Define an abstract adversary of type CPAadversary not working in the memory space of the CPA module  ***)
declare module A : CPAadversary{CPA}. 
(*** Make sure the procedures of the adversary terminate ***)
axiom Ag_ll : islossless A.guess. 
axiom Ac_ll : islossless A.choose. 

(*** Simulator perfectly simulating the original attack ***)
local module Game1(A:CPAadversary) = {
  proc main() : bool = {
    var x, w, pk, sk, m0, m1, b, b', y, e, c;
     (x, w)   <- Sampling.fromL();
    (pk, sk)  <- Genscheme.keygen();
    (m0, m1)  <- A.choose(pk);
       b      <$ {0,1};
       y      <- HPS.priveval(sk, x);
       e      <- toY (toint (b?m1:m0) + toint y);
       c      <- (x, e);
       b'     <- A.guess(c);
     return (b = b');
  }
}.

(*** Proof that the simulator and the original attack are equivalent ***)
local equiv CPA_Game1_equiv : CPA(Genscheme,A).main ~ Game1(A).main : ={glob A} ==> ={res}. 
proof.
proc;inline*.    
call(_:true). swap{1} [13 .. 15] - 12.  auto. call(_:true). 
auto.
move => &1 &2 A. split; first smt. move => xRsupp x xRsupp2. split; first exact/xRsupp2. 
move => xinls. split; first trivial. move => xeq. 
move => w ws. split; first smt. move => ws2. split; first trivial. move => weq. 
split. smt. move => k1R_1. split; first smt. 
move => k1R_2 k0L k0Lsupp. split; first exact/k0Lsupp. 
move => k0Lsupp2. split; first trivial. move => k0Leq pkL. 
split. smt.  move => H1 m1 m2 AR AL H2 bL bLsupp. split; first exact/bLsupp.
move => bLsupp2. split.  split. split. done. 
move => ?. have -> : proj(pkL, x,w) = hash(k0L, x) by smt. smt. 
smt. move => H3 b1 b2 A_L A_R. 
smt. 
qed. 

(*** Transform the equivalence to a probability expression ***)
local lemma CPA_Game1_pr &m : 
    Pr[CPA(Genscheme,A).main() @ &m : res] = Pr[Game1(A).main() @ &m : res]. 
proof.
by byequiv(CPA_Game1_equiv). 
qed. 

local equiv Game1_SMP1 : Game1(A).main ~ SMP1(SMPadv(A)).main : ={glob A} ==> ={res}. 
proof. 
proc. 
inline*. wp. 
call(_:true). swap{2} 14 1. 
wp. rnd. call(_:true). wp. rnd;wp;rnd;rnd;skip. auto. 
qed. 

local lemma Game1_SMP1_pr &m : Pr[Game1(A).main() @ &m : res] = Pr[SMP1(SMPadv(A)).main() @ &m : res] by byequiv(Game1_SMP1). 

(*** Make a change to the simulator: sample x from X\L instead of L ***)
local module Game2(A:CPAadversary) = {
  proc main() : bool = {
    var  pk, sk, x, y, m0, m1, b, b', e, c;
       x      <- Sampling.fromXnotL();
    (pk, sk)  <- Genscheme.keygen();
    (m0, m1)  <- A.choose(pk);
       b      <$ {0,1};
       y      <- HPS.priveval(sk, x);
       e      <- toY (toint (b?m1:m0) + toint y);
       c      <- (x, e);
       b'     <- A.guess(c);
     return (b = b');
  }
}. 

local equiv Game2_SMP0 : Game2(A).main ~ SMP0(SMPadv(A)).main : ={glob A} ==> ={res}. 
proof. 
proc. inline*. wp. 
swap{2} 13 1. 
call(_:true). wp; rnd. 
call(_:true). 
wp;rnd;wp;rnd;skip;auto. 
qed. 

local lemma Game2_SMP0_pr &m : Pr[Game2(A).main() @ &m : res] = Pr[SMP0(SMPadv(A)).main() @ &m : res] by byequiv(Game2_SMP0). 

(*** Make yet another change to the simulator: sample y from Y instead of comuting y = hash(k,x) ***)
local module Game3(A:CPAadversary) = {
  proc main() : bool = {
    var pk, sk, x, y, m0, m1, b, b', e, c;
       x      <- Sampling.fromXnotL();
    (pk, sk)  <- Genscheme.keygen();   
    (m0, m1)  <- A.choose(pk);
       b      <$ {0,1};
       y      <- Sampling.fromY();
       e      <- toY (toint (b?m1:m0) + toint y);
       c      <- (x, e);
       b'     <- A.guess(c);
     return (b = b');
  }
}.  

(* We prove that distinguishing between Game2 and Game3 is the same as 
   distinguishing between Smooth1 and Smooth0 (i.e. (x, s, hash(k,x)) and (x, s, random y)) ***)
local equiv Game2_Smooth1 : Game2(A).main ~ Smooth1(SmoothAdv(A)).main : ={glob A} ==> ={res}.
proof.  
proc. inline*. wp. 
call(_:true). wp. 
rnd. call(_:true). 
wp;rnd;wp;rnd;skip. 
move => &1 &2 H0 x0R x0Rsupp. split; first exact/x0Rsupp. 
move =>_. split; first trivial. move => x0Req k0R k0Rsupp. split; first exact/k0Rsupp. 
move => _. split; first trivial. move => k0Req. split.  smt. 
move =>H1.  
move => mL mR aL aR H2 b0R b0Rsupp. split; first exact/b0Rsupp.  move => _. 
split; first trivial. move => b0Req. 
split. smt. move => H3 rL rR AL AR H4. have -> : rL = rR by smt.  done.   
qed. 

local equiv Game3_Smooth0 : Game3(A).main ~ Smooth0(SmoothAdv(A)).main : ={glob A} ==> ={res}.
proof. 
proc.  inline*. 
wp. 
call(_:true). 
wp. swap{1} 13 -5. rnd. 
call(_:true). 
wp. rnd;wp;rnd;wp;rnd;skip. 
move => &1 &2 H0 x0R x0Rsupp. split; first exact/x0Rsupp. 
move =>_. split; first trivial. move => x0Req k0R k0Rsupp. split; first exact/k0Rsupp. 
move => _. split; first trivial. move => k0Req y0R y0Rsupp. split; first exact/y0Rsupp. 
move =>_. split; first trivial. move => y0Req. split; first smt. move => H1. 
move => mL mR aL aR H2 b0R b0Rsupp. split; first exact/b0Rsupp.  move => _. 
split; first trivial. move => b0Req. 
split. smt. move => H3 rL rR AL AR H4. have -> : rL = rR by smt.  done.  
qed. 

local lemma Game2_Smooth1_pr &m : Pr[Game2(A).main() @ &m : res] = Pr[Smooth1(SmoothAdv(A)).main() @ &m : res] by byequiv(Game2_Smooth1). 

local lemma Game3_Smooth0_pr &m : Pr[Game3(A).main() @ &m : res] = Pr[Smooth0(SmoothAdv(A)).main() @ &m : res] by byequiv(Game3_Smooth0).  

(*** Final change to the simulator: the adversary's guess is completely independent of the hidden bit b ***)
local module Game3indep(A:CPAadversary) = {
  proc main() : bool = {
    var pk, sk, x, y, m0, m1, b, b', e, c;
       x      <- Sampling.fromXnotL();
    (pk, sk)  <- Genscheme.keygen();   
    (m0, m1)  <- A.choose(pk);
       y      <- Sampling.fromY();
       e      <- toY ( toint y);
       c      <- (x, e);
       b'     <- A.guess(c);
       b      <$ {0,1};
     return (b = b');
  }
}. 


(*** Proof that the probabilty of winning in the final simulation is 1/2 ***)
local lemma Game3indep_half &m : Pr[Game3indep(A).main() @ &m : res] = 1%r/2%r. 
proof. 
byphoare => //. 
proc. rnd. call(_:true). apply Ag_ll. inline Sampling.fromY. wp; rnd. call(_:true). apply Ac_ll.
inline*. auto. 
move => &1 T. split; first smt. move => xslsll v vsupp. 
split. move => vt.  split; first smt(dK_ful). move => dkll k ksupp. 
split. move => kt.   split. move => b. smt(@DBool). smt(ys_ful). 
split. done.  
qed. 

(*** Proof that the final simulation is equivalent to Game3 ***)
local equiv Game3_Game3indep : Game3(A).main ~ Game3indep(A).main : ={glob A} ==> ={res}. 
proof. 
proc. swap{2} 8 -4.
call(_:true). wp. 
inline Sampling.fromY. 
wp. 
rnd (fun y, toY (toint y + toint (if b then m1 else m0)){2})
    (fun y, toY (toint y - toint (if b then m1 else m0)){2}). 
rnd.
call(_:true); inline*.  wp; rnd; wp; rnd; skip. 
move => &1 &2 A. split; first smt. move => xRsuppeq xR xRsupp.
split; first exact/xRsupp. move => _. split; first trivial. move => xReq.  
split; first smt. move => k0Rsuppeq. split; first smt.
move => k0Rmu k0L k0Lsupp. split; first exact/k0Lsupp.  move =>_. split; first trivial. 
move => k0Leq. split. smt. 
move => H0 bL bR AL AR H1 m1 m2 br0 br0supp.  split; first exact/br0supp. 
move => _. split; first trivial. move => br0eq. 
split. smt(y1 y2). move => H2. 
split; first smt(ys_ful). move => H3. 
move => yL yLsupp. split; first smt(ys_ful). move => H4. split; first smt(y1 y2). move => H5. 
split; first smt. move => h6. 
move => b_L b_R a_L a_R. smt. 
qed. 

local lemma Game3_Game3indep_pr &m : Pr[Game3(A).main() @ &m : res] = 
  Pr[Game3indep(A).main() @ &m : res] by byequiv (Game3_Game3indep). 

local lemma Game3half &m : Pr[Game3(A).main() @ &m : res] = 1%r/2%r. 
proof. 
by rewrite (Game3_Game3indep_pr &m) (Game3indep_half &m).
qed. 

(*** Final reduction, showing that the adversary's advantage in winning the CPA game
     is bounded by the distinguishing advantage in the SMP and the smoothness distinguishing advantage ***)
local lemma secure &m : `|Pr[CPA(Genscheme,A).main() @ &m : res] - 1%r/2%r| <=
  `|Pr[SMP1(SMPadv(A)).main() @ &m : res] - Pr[SMP0(SMPadv(A)).main() @ &m : res]| + 
  `|Pr[Smooth1(SmoothAdv(A)).main() @ &m : res] - Pr[Smooth0(SmoothAdv(A)).main() @ &m : res]|. 
proof. 
by rewrite (CPA_Game1_pr &m) -(Game1_SMP1_pr &m) -(Game2_SMP0_pr &m) (Game2_Smooth1_pr &m) -(Game3_Smooth0_pr &m) (Game3half &m) ler_dist_add.  
qed. 

print lemma secure. 

end section Security. 
