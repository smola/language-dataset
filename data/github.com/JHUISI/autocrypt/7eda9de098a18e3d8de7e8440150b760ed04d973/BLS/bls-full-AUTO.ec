prover alt-ergo, z3, cvc3.

type G_1.
type G_T.
type message.

cnst g_1_i : G_1.
cnst g_T_i : G_T.
cnst g_1 : G_1.
cnst g_T : G_T.
cnst q_1 : int.
cnst q_T : int.

cnst q : int.

cnst limit_Hash : int.
cnst limit_Sign : int.

op [*] : (G_1, G_1) -> G_1 as G_1_mul.
op [^] : (G_1, int) -> G_1 as G_1_pow.

op [*] : (G_T, G_T) -> G_T as G_T_mul.
op [^] : (G_T, int) -> G_T as G_T_pow.

op G_1_log : G_1 -> int.
op G_T_log : G_T -> int.

op e : (G_1, G_1) -> G_T as G_1_pair.

(*
   From easycrypt ElGamal:
   If we use the native modulo alt-ergo is not able
   to perform the proof.
   So we declare an operator (%%) which stand for the modulo ...
*)

op [%%] : (int,int) -> int as int_mod.

axiom limit_Hash_pos : 0 < limit_Hash.
axiom limit_Sign_pos : 0 < limit_Sign.

axiom q_1_pos : 0 < q_1.
axiom q_T_pos : 0 < q_T.

axiom q_pos : 0 < q.

(* Axioms largely pulled from ElGamal.  Note that G_1 and G_T have the same order if the order is prime. *)

axiom G_1_mult_1 : forall (x : G_1), x * g_1_i = x.
axiom G_1_exp_0 : forall (x : G_1), x ^ 0 = g_1_i.
axiom G_1_exp_S : forall (x : G_1, k : int), k > 0 => x ^ k = x * (x^(k-1)).

axiom G_T_mult_1 : forall (x : G_T), x * g_T_i = x.
axiom G_T_exp_0 : forall (x : G_T), x ^ 0 = g_T_i.
axiom G_T_exp_S : forall (x : G_T, k : int), k > 0 => x ^ k = x * (x^(k-1)).

axiom bilinearity : forall (x : G_1, y : G_1, a : int, b : int), e(x ^ a, y ^ b) = e(x, y) ^ (a * b).
(* axiom non_degenerate : !(e(g_1, g_1) = g_T_i). *)

axiom G_1_pow_add_1 :
 forall (x, y:int), g_1 ^ (x + y) = g_1 ^ x * g_1 ^ y.

axiom G_T_pow_add :
 forall (x, y:int), g_T ^ (x + y) = g_T ^ x * g_T ^ y.

axiom G_1_pow_mult_1 :
 forall (x, y:int),  (g_1 ^ x) ^ y = g_1 ^ (x * y).

axiom G_T_pow_mult :
 forall (x, y:int),  (g_T ^ x) ^ y = g_T ^ (x * y).

axiom G_1_log_pow_1 :
 forall (g_1': G_1), g_1 ^ G_1_log(g_1') = g_1'.

axiom G_T_log_pow :
 forall (g_T':G_T), g_T ^ G_T_log(g_T') = g_T'.

axiom G_1_pow_mod_1 :
 forall (z:int), g_1 ^ (z%%q_1) = g_1 ^ z.

axiom G_T_pow_mod :
 forall (z:int), g_T ^ (z%%q_T) = g_T ^ z.

axiom mod_add :
 forall (x,y:int), (x%%q + y)%%q = (x + y)%%q.

axiom mod_small :
 forall (x:int), 0 <= x => x < q => x%%q = x.

axiom mod_sub :
 forall (x, y:int), (x%%q - y)%%q = (x - y)%%q.

axiom mod_bound :
 forall (x:int), 0 <= x%%q && x%%q < q.

pop Rand_exp : () -> (int).
pop Rand_G_1 : () -> (G_1).

(* axiom Rand_G_1_exp_def() : x = Rand_G_1_exp() ~ y = [0..q-1] : true ==> x = y. *)
axiom Rand_G_1_def() : x = Rand_G_1() ~ y = Rand_exp() : true ==> x = g_1 ^ y.

adversary Adv (adv_public_key_1 : G_1) : (message * G_1) {message -> G_1; (message) -> G_1}.

game blsfull_EF = {
  var sk : int
  var pk : G_1
  var queried : message list
  var count_Hash : int
  var count_Sign : int
  var count_Verify : int
  var rand_oracle : (message, G_1) map

  fun Hash(m : message) : G_1 = {
    count_Hash = count_Hash + 1;
    if(!in_dom(m, rand_oracle)) {
      rand_oracle[m] = Rand_G_1();
    }
    return rand_oracle[m];
  }

  fun Sign(M : message) : G_1 = {
    var sig : G_1;
    var preSig : G_1;
    var output : G_1;
    count_Sign = count_Sign + 1;
    preSig = Hash(M);
    sig = (preSig ^ sk);
    output = sig;
    queried = M :: queried;
    return output;
  }

  abs A = Adv{Hash, Sign}

  fun Verify(M : message, sig : G_1) : bool = {
    var output : bool;
    var h : G_1;
    count_Verify = count_Verify + 1;
    h = Hash(M);
    output = (e(h, pk) = e(sig, g_1));
    return output;
  }

  fun Init() : bool = {
    var x : int;
    count_Hash = 0;
    count_Sign = 0;
    count_Verify = 0;
    x = Rand_exp();
    pk = (g_1 ^ x);
    sk = x;
    rand_oracle = empty_map;
    queried = [];
    return true;
  }

  fun Main() : bool = {
    var M : message;
    var sig : G_1;
    var v : bool;
    var dummy : bool;

    dummy = Init();
    (M, sig) = A(pk);

    v = Verify(M, sig);
    return v && !mem(M, queried);
  }
}.
