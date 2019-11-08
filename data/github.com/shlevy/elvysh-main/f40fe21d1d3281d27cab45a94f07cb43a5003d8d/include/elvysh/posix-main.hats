staload "elvysh/maybe-consumed.sats"
staload "elvysh/filedes.sats"
staload "elvysh/errno.sats"

(* Define a POSIX entry point
 *
 * When this header is #included, the user is expected to implement posix_main
 * instead of main. In addition to argc and argv, posix_main's interface
 * takes views corresponding to initial global program state in the POSIX
 * environment.
 *
 * posix_main takes an errno_v ( free ) proof as well as filedes proofs for 0,
 * 1, and 2 (corresponding to stdio). The errno_v must be unconsumed and free
 * at the end of the function, but the filedes proofs may be consumed (see
 * elvysh/maybe-consumed.sats for details).
 *
 * stdin (filedes 0) is readable.
 *
 * It is expected to include this file directly into the file which implements
 * posix_main
 *)

(* posix_main interface *)
extern fun posix_main { n:int | n >= 1 } ( ev: !errno_v ( free )
                                         , stdin: !readable_filedes ( 0 ) >>
                                             maybe_consumed (
                                               any_filedes ( 0 )
                                             )
                                         , stdout: !any_filedes ( 1 ) >>
                                             maybe_consumed (
                                               any_filedes ( 1 )
                                             )
                                         , stderr: !any_filedes ( 2 ) >>
                                             maybe_consumed (
                                               any_filedes ( 2 )
                                             )
                                         | argc: int n
                                         , argv: !argv ( n )
                                         ): int = "sta#elvysh_main_posix_main"

(* main impl *)
implement main ( argc, argv ) = let
  extern praxi { v: view } __assert_view (): v
  prval ev = __assert_view< errno_v ( free ) > ()
  prval stdin = __assert_view< readable_filedes ( 0 ) > ()
  prval stdout = __assert_view< any_filedes ( 1 ) > ()
  prval stderr = __assert_view< any_filedes ( 2 ) > ()
  val res = posix_main ( ev, stdin, stdout, stderr | argc, argv )
  extern praxi { v: view } __unassert_view ( x: v ): ()
  prval _ = __unassert_view ( ev )
  prval _ = __unassert_view ( stdin )
  prval _ = __unassert_view ( stdout )
  prval _ = __unassert_view ( stderr )
in res end
