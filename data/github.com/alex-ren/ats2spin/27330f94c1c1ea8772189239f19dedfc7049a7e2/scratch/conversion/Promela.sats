(* ****** ****** *)
//
// For embedding
// Promela into ATS
//
(* ****** ****** *)
//
// Author: Hongwei Xi
// Authoremail: gmhwxiATgmailDOTcom
// Start time: December, 2015
//
(* ****** ****** *)
//
//
typedef
pid(i:int) = int i
//
typedef pid = [i:nat] pid(i)
//
(* ****** ****** *)
//
//
(* ****** ****** *)
//

// todo: In reality curpid is not a constant function.
// How to incorporate it into the type system?
// One thing good about curpid is that it is not dependent
// on time.
stacst curpid: () -> int

fun
Promela$mypid(): pid (curpid ())
//
fun
Promela$assert{b:bool}(bool(b)): [b==true] void
//
(* ****** ****** *)

sortdef gname = int

fun
Promela$wait_until(() -> bool): void
fun
Promela$wait_unless(() -> bool): void

fun
Promela$atomic {a:viewt@ype} (() -> a): a

fun
Promela$run (() -> void): void

// abstype process_end
// fun Promela$run2 (process_end): void

// fun Promela$process_end (): process_end

// prfun
// Promela$set_tag (tag: string): void

(* ****** ****** *)

// absview atomic_view
// viewdef atom_v = atomic_view
// 
// prfun Promela$begin_atomic (): (atomic_view | void)
// prfun Promela$end_atomic(atomic_view): void

// abstype garray (a:t@ype, g: gname, n: int)
// 
// fun array_create {a:t@ype}{g:gname}{n:nat} (n: int n, init: a):
//   garray (a, g, n)
// 
// fun array_get {a:t@ype}{g:gname}{i,n:nat | i < n} (
//   arr: garray (a, g, n), i: int i): a
// 
// fun array_set {a:t@ype}{g:gname}{i,n:nat | i < n} (
//   arr: garray (a, g, n), i: int i, v: a): void

(* ****** ****** *)

(* end of [Promela.sats] *)


