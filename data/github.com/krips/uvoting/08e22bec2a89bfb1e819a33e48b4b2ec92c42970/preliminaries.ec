
require import Int.
require import Real.
require import FSet.
require import NewMap.

require import ABitstring.
require import PrimeField.
import FDistr.

require (*--*) DiffieHellman.
require (*--*) PKE.
require PKS.

(** Assumption: set DDH *)
require export ABitstring.
export FDistr.
clone export DiffieHellman as DH.
import DDH.


type pkey       = group.
type skey       = t.
type plaintext  = group.
type ciphertext = group * group.
type message    = bitstring.
type signature.


op pTextToMsg: plaintext -> message.
op msgToPText: message -> plaintext.
axiom msgFormat: forall (p:plaintext), msgToPText(pTextToMsg(p)) = p.

op bitstringToF: bitstring -> F.t.
op toBitstring: F.t -> bitstring.
axiom bitEncode: forall (b:bitstring), toBitstring(bitstringToF(b)) = b.


(** Construction: ElGamal based PKE **)
theory ElGamal_PKE.
  clone import PKE as PKE_ with
    type pkey <- pkey,
    type skey <- skey,
    type plaintext <- plaintext,
    type ciphertext <- ciphertext.

  (* multiplication of ElGamal ciphertexts *)
  op mult_ciphertext (a: group * group) (b: group * group): group * group =
    (fst a * fst b, snd a * snd b).
    
  module ElGamal= {
    proc kg(): pkey * skey = {
      var sk;
      sk = $FDistr.dt;
      return (g ^ sk, sk);
    }

    proc enc(pk:pkey, randomElement:t, m:plaintext): ciphertext = {
      return (g ^ randomElement, m * pk ^ randomElement);
    }

    proc dec(sk:skey, c:ciphertext): plaintext option = {
      var gy, gm;
      (gy, gm) = c;
      return Some (gm * gy^(-sk));
    }
  }.
end ElGamal_PKE.



(** Construction: abstract PKS **)
theory Abstract_PKS.
  clone import PKS as PKS_ with
    type pkey <- pkey,
    type skey <- skey,
    type message <- message,
    type signature <- signature.

  op sign: (skey * message) -> signature.
  op verify: (pkey * message * signature) -> bool.

  axiom pks_correctness (pk:pkey, sk:skey):
    forall (m:message), verify(pk, m, sign(sk, m)) = true.
  
  
  module APKS: Scheme = {
    proc init(): unit = {}

    proc keygen(): (pkey * skey) = {
      var pk: pkey;
      var sk: skey;      
      sk = $FDistr.dt;
      pk = g^sk;
      return (pk, sk);      
    }

    proc sign(sk:skey, m:message): signature = {
      var s: signature;
      s = sign(sk, m);
      return s;
    }

    proc verify(pk:pkey, m:message, s:signature): bool = {
      var isValid: bool;
      isValid = verify(pk, m, s);
      return isValid;
    }
  }.
end Abstract_PKS.
