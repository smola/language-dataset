#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

fn should_add(n: int): bool =
  n % 3 = 0 || n % 5 = 0

fn sum(n: int): int =
  let
    fun go(n: int, acc: int, lim: int): int =
      if n >= lim
        then acc
        else if should_add(n)
             then go(n + 1, acc + n, lim)
             else go(n + 1, acc, lim)
  in
    go(1, 0, n)
  end

implement main0 () = () where {
  val n = g0string2int(fileref_get_line_string(stdin_ref))
  val res = sum(n)
  val _ = println!(res)
}