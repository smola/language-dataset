staload "SATS/recursive.sats"
staload "SATS/recursive_prf.sats"

#include "share/atspre_staload.hats"
#include "$PATSHOMELOCS/specats-0.4.0/mylibies.hats"
#include "DATS/recursive.dats"
#include "DATS/recursive_list0.dats"
#include "DATS/recursive_prf.dats"
#include "DATS/recursive_list_prf.dats"

fun sum(is : list0(int)) : int =
  let
    fn go(i : list0f(int, int)) : int =
      case+ i of
        | list0_consf (x, xs) => x + xs
        | list0_nilf() => 0
  in
    cata(lam x0 =<cloref1> go(x0), is)
  end

implement main0 () =
  {
    val folded: int = let
      var list = list0_cons(2, list0_cons(1, list0_nil()))
    in
      sum(list)
    end
    var folded_check = eq_g0int_int(folded, 3)
    var n0 = @{ test_name = "cata", test_result = folded_check }
    var xs = n0 :: nil
    var total = list_vt_length(xs)
    val g = @{ group = "Recursion schemes", leaves = xs } : test_tree
    val _ = iterate_list(g, 0, total)
  }
