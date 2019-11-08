(* ****** ****** *)

#include "./mylib.dats"

(* ****** ****** *)

extern
fun
print_term(t0: term): void
and
prerr_term(t0: term): void

extern
fun
fprint_term(out: FILEref, t0: term): void

(* ****** ****** *)

overload print with print_term
overload prerr with prerr_term
overload fprint with fprint_term

(* ****** ****** *)

implement
print_term(t0) =
fprint_term(stdout_ref, t0)
implement
prerr_term(t0) =
fprint_term(stderr_ref, t0)

(* ****** ****** *)

implement
fprint_val<term> = fprint_term

(* ****** ****** *)

implement
fprint_term(out, t0) =
(
case+ t0 of

| TMint(i) => (fprint!(out, "TMint(", i, ")"))
| TMstr(s) => (fprint!(out, "TMstr(", s, ")"))
| TMvar(x) =>
(  fprint!(out, "TMvar(", x, ")"))

| TMlam(x, t) =>
(  fprint!(out, "TMlam(", x, "; ", t, ")"))
| TMapp(t1, t2) =>
(  fprint!(out, "TMapp(", t1, "; ", t2, ")"))

| TMopr(opr, ts) =>
(  fprint!(out, "TMopr(", opr, "; ", ts))

| TMfix(f, x, t) =>
(  fprint!(out, "TMfix(", f, ", ", x, "; ", t, ")"))

| TMifnz(t1, t2, t3) =>
(  fprint!(out, "TMifnz(", t1, "; ", t2, "; ", t3, ")"))

| TMtup(xs) => (fprint!(out, "TMtup()"); list0_foreach(xs, lam(x) => println!(x)))
| TMproj(t1, i1) => (fprint!(out, "TMproj(", t1, "|", i1, ")"))
| TMseq _ => (fprint!(out, "TMseq()")
))


extern 
fun
print_value(t0: value): void

extern
fun
fprint_value(out: FILEref, t0: value): void

overload print with print_value
overload fprint with fprint_value


implement
fprint_val<value> = fprint_value


implement
print_value
(t0) = fprint_value(stdout_ref, t0)


implement
fprint_value(out, t0) =
(
  case+ t0 of
  | VALint(i) => fprint!(out, "VALint(", i, ")")
  | VALstr(s) => fprint!(out, "VALstr(", s, ")")
  | VALtup(vs) => fprint!(out, "VALtup(", vs, ")")
  | VALlam(_, _) => fprint!(out, "VALlam(_, _)")
  | VALfix(_, _) => fprint!(out, "VALlam(_, _)")
  | VALunit() => fprint!(out, "VALunit()")
)


extern
fun
interp2 : (term, envir) -> value


implement
interp1(src) =
interp2(src, list0_nil())

overload interp with interp1
overload interp with interp2

(* ****** ****** *)


extern
fun
envir_find :
(envir, string) -> value
implement
envir_find(env, x0) =
(
  case- env of
  | list0_cons(xv, env) =>
    if x0 = xv.0 then xv.1 
    else envir_find(env, x0)
)


(* ****** ****** *)


fun
interp2_list
(ts: termlst, env: envir): valuelst = 
list0_map<term><value>(ts, lam(t) => interp2(t, env))

implement
interp2(t0, env) =
(
case t0 of
| TMint(i) => (VALint(i))
| TMstr(s) => (VALstr(s))
| TMvar(x) => (envir_find(env, x))
| TMlam _ => (VALlam(t0, env))
| TMfix _ => (VALfix(t0, env))
| TMtup(ts) => (VALtup(interp2_list(ts, env)))
| TMproj(t_tup, i) => 
  (
    let
      val v_tup = interp2(t_tup, env)
    in
      case- v_tup of | VALtup(vs) => vs[i]
    end
  )
| TMapp(t1, t2) => 
(
  let
    val v1 = interp2(t1, env)
  in
    case- v1 of
    | VALlam(t_lam, env_lam) => 
      (
        let
          val v2 = interp2(t2, env)
          val-TMlam(x, t_body) = t_lam
          val env_lam = list0_cons($tup(x, v2), env_lam)
        in
          interp2(t_body, env_lam)
        end
      )
    | VALfix(t_fix, env_fix) => 
      (
        let
          val v2 = interp2(t2, env)
          val-TMfix(f, x, t_body) = t_fix
          val env_fix = list0_cons($tup(x, v2), env_fix)
          val env_fix = list0_cons($tup(f, v1), env_fix)
        in
          interp2(t_body, env_fix)
        end
      )
  end
  )
| TMopr _ => (interp2_opr(t0, env))
| TMifnz(t1, t2, t3) => 
  (
    let
      val v1 = interp2(t1, env)
    in
      case- v1 of
      | VALint(i) => interp2(if i != 0 then t2 else t3, env)
    end
  )
| TMseq(t1, t2) => 
  (
    let 
      val _ = interp2(t1, env) 
    in 
      interp2(t2, env) 
    end
  )
) where
{
fun
interp2_opr
(t0: term, env: envir): value = let

#define :: list0_cons
#define nil list0_nil

val-TMopr(opr, ts) = t0

val vs 
  = list0_map<term><value>(ts, lam(t) => interp2(t, env))

in
case- opr of
| "+" => 
  (
    case- vs of | VALint(i1)::VALint(i2)::nil() => VALint(i1+i2)
  )
| "-" =>
  (
    case- vs of | VALint(i1)::VALint(i2)::nil() => VALint(i1-i2)
  )
| "*" =>
  (
    case- vs of | VALint(i1)::VALint(i2)::nil() => VALint(i1*i2)
  )
| "/" =>
  (
    case- vs of | VALint(i1)::VALint(i2)::nil() => VALint(i1/i2)
  )
| "~" => 
  (
    case- vs of VALint(i1)::nil() => VALint(~i1)
  )
| "abs" => 
  (
    case- vs of VALint(i1)::nil() => VALint(abs(i1))
  )
| "<" => 
  (
    case- vs of 
    | VALint(i1)::VALint(i2)::nil() => VALint(bool2int(i1 < i2))
  )
| ">" => 
  (
    case- vs of 
    | VALint(i1)::VALint(i2)::nil() => VALint(bool2int(i1 > i2))
  )
| ">=" => 
  (
    case- vs of 
    | VALint(i1)::VALint(i2)::nil() => VALint(bool2int(i1 >= i2))
  )
| "isneq" => 
  (
    case- vs of 
    | VALint(i1)::VALint(i2)::nil() => VALint(bool2int(i1 != i2))
  )
| "iseq" => 
  (
    case- vs of 
    | VALint(i1)::VALint(i2)::nil() => VALint(bool2int(i1 = i2))
  )
| "print" =>
  (
    case- vs of v0::nil() =>
    (
      let 
      val () =
      ( 
        case+ v0 of
        | VALint(i) => print!(i)
        | VALstr(s) => print!(s)
        | VALtup(vs) => print!("VALtup(...)")
        | VALlam(_, _) => print!("VALlam(...)")
        | VALfix(_, _) => print!("VALfix(...)")
        | VALunit() => print!("VALunit()")
      )
      in
        VALunit()
      end
    )
  )
| "println" =>
  (
    case- vs of v0::nil() =>
    (
      let 
      val () = 
        (
          case+ v0 of
          | VALint(i) => println!(i)
          | VALstr(s) => println!(s)
          | VALtup(vs) => println!("VALtup(...)")
          | VALlam(_, _) => println!("VALlam(...)")
          | VALfix(_, _) => println!("VALfix(...)")
          | VALunit() => println!("VALunit()")
        )
      in
        VALunit()
      end
    )
  )

end
} (* end of [interp2] *)

(* ****** ****** *)

fun sub_term_int(t1: term, i2: int): term 
  = TMopr( "-", list0_tuple(t1, TMint(i2)))
fun add_term_int(t1: term, i2: int): term 
  = TMopr( "+", list0_tuple(t1, TMint(i2)))
fun div_term_int(t1: term, i2: int): term 
  = TMopr( "/", list0_tuple(t1, TMint(i2)))
fun mod_term_int(t1: term, i2: int): term 
  = TMopr( "%", list0_tuple(t1, TMint(i2)))
fun gt_term_int(t1: term, i2: int): term 
  = TMopr(">" , list0_tuple(t1, TMint(i2)))
fun gte_term_int(t1: term, i2: int): term 
  = TMopr(">=" , list0_tuple(t1, TMint(i2)))
fun lte_term_int(t1: term, i2: int): term 
  = TMopr("<=" , list0_tuple(t1, TMint(i2)))
fun lt_term_int(t1: term, i2: int): term 
  = TMopr("<", list0_tuple(t1, TMint(i2)))
fun add_term_term(t1: term, t2: term): term 
  = TMopr("+", list0_tuple(t1, t2))
fun sub_term_term(t1: term, t2: term): term 
  = TMopr("-", list0_tuple(t1, t2))
fun mul_term_term(t1: term, t2:term): term 
  = TMopr("*", list0_tuple(t1, t2))
fun div_term_term(t1: term, t2:term): term 
  = TMopr("/", list0_tuple(t1, t2))
fun mod_term_term(t1: term, t2:term): term 
  = TMopr("%", list0_tuple(t1, t2))
fun lt_term_term(t1: term, t2: term): term 
  = TMopr("<", list0_tuple(t1, t2))
fun lte_term_term(t1: term, t2: term): term 
  = TMopr("<=", list0_tuple(t1, t2))
fun isneq_term_term(t1: term, t2: term): term 
  = TMopr("isneq", list0_tuple(t1, t2))
fun iseq_term_int(t1: term, i2: int): term 
  = TMopr("iseq", list0_tuple(t1, TMint(i2)))
fun abs_term(t1: term): term 
  = TMopr("abs", list0_tuple(t1))


val loop = TMvar("loop")
val TMtrue = TMint(1)
val TMfalse = TMint(0)
val i = TMvar("i")
val n = TMvar("n")
val res = TMvar("res")
val f = TMvar("f")
val x = TMvar("x")
val res = TMvar("res")
val xyz = TMvar("xyz")
val xyzw = TMvar("xyzw")

overload abs with abs_term
overload - with sub_term_int
overload + with add_term_int
overload / with div_term_int
overload % with mod_term_int

overload - with sub_term_term
overload + with add_term_term
overload * with mul_term_term
overload / with div_term_term
overload % with mod_term_term

overload < with lt_term_int
overload > with gt_term_int
overload >= with gte_term_int
overload <= with lte_term_int

overload < with lt_term_term
overload <= with lte_term_term
overload isneq with isneq_term_term

overload iseq with iseq_term_int

(* ****** ****** *)

#define N 8

(* ****** ****** *)

fun p0 (t0: term): term = TMproj(t0, 0)
fun p1 (t0: term): term = TMproj(t0, 1)
fun p2 (t0: term): term = TMproj(t0, 2)
fun p3 (t0: term): term = TMproj(t0, 3)
fun p4 (t0: term): term = TMproj(t0, 4)
fun p5 (t0: term): term = TMproj(t0, 5)
fun p6 (t0: term): term = TMproj(t0, 6)
fun p7 (t0: term): term = TMproj(t0, 7)

(* ****** ****** *)

val xyzw = TMvar("xyzw")
val qw = TMvar("qw")
val j = TMvar("j")

fun 
quad
(t0: term, t1: term, t2: term, t3:term): term 
  = TMtup(cons0(t0, cons0(t1, cons0(t2, cons0(t3, nil0())))))


(* ****** ****** *)

fun brd
( 
  t0: term, t1: term, t2: term, t3: term
, t4: term, t5: term, t6: term, t7: term
): term 
  =
  TMtup
  ( 
    cons0(t0, cons0(t1, cons0(t2, cons0(t3, 
      cons0(t4, cons0(t5, cons0(t6, cons0(t7, nil0()))))))))
  )

(* ****** ****** *)

#define :: list0_cons
#define nil list0_nil

(* ****** ****** *)

fun 
print_dots
(i: term): term = 
let
val print_dot = 
TMfix
(
  "f", "x", 
  TMseq(TMopr("print", list0_tuple(TMstr(". "))), 
  TMifnz(x, TMapp(f, x-1), 
  TMopr("print", list0_tuple(TMstr("")))))
)
in
  TMifnz
  (
    i > 0, 
    TMapp(print_dot, i-1), 
    TMstr(". ")
  )
end

(* ****** ****** *)


fun 
print_row
(i: term): term = 
(
TMseq
  (
    TMseq
    (print_dots(i), TMopr("print", list0_tuple(TMstr("Q ")))), 
    TMseq(print_dots(abs(i-N) - 1), TMopr("print", list0_tuple(TMstr("\n"))))
    )
)


(* ****** ****** *)


fun 
print_board
(bd: term): term =
(
  TMseq(
    TMseq(TMseq(  print_row((p0(bd))), print_row((p1(bd))))
        , TMseq(  print_row((p2(bd))), print_row((p3(bd)))))
  , TMseq(TMseq(  print_row((p4(bd))), print_row((p5(bd))))
        , TMseq(  print_row((p6(bd))), print_row((p7(bd))))))
  (* print!("\n") *)
)


(* ****** ****** *)


val gg = 
brd(TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0))


(* ****** ****** *)


fun 
board_get
(bd: term, i: term): term 
  =
  (
    TMifnz
    (
      i, 
      TMifnz(i - 1, TMifnz(i - 2 , TMifnz(i - 3,
      TMifnz(i - 4, TMifnz(i - 5, TMifnz(i - 6, p7(bd), 
      p6(bd)), p5(bd)), p4(bd)), p3(bd)), 
      p2(bd)), p1(bd)), p0(bd)
    )
  )


(* ****** ****** *)


fun
board_set
(bd: term, i: term, j:term): term =
TMifnz(i, TMifnz(i - 1, TMifnz(i -2, TMifnz(i - 3
, TMifnz(i - 4, TMifnz(i - 5, TMifnz(i - 6, 
brd(p0(bd), p1(bd), p2(bd), p3(bd), p4(bd), p5(bd), p6(bd), j), 
brd(p0(bd), p1(bd), p2(bd), p3(bd), p4(bd), p5(bd), j, p7(bd))),
brd(p0(bd), p1(bd), p2(bd), p3(bd), p4(bd), j, p6(bd), p7(bd))),
brd(p0(bd), p1(bd), p2(bd), p3(bd), j, p5(bd), p6(bd), p7(bd))),
brd(p0(bd), p1(bd), p2(bd), j, p4(bd), p5(bd), p6(bd), p7(bd))),
brd(p0(bd), p1(bd), j, p3(bd), p4(bd), p5(bd), p6(bd), p7(bd))),
brd(p0(bd), j, p2(bd), p3(bd), p4(bd), p5(bd), p6(bd), p7(bd))),
brd(j, p1(bd), p2(bd), p3(bd), p4(bd), p5(bd), p6(bd), p7(bd)))


(* ****** ****** *)


val zx = TMvar("zx")


fun
safety_test1
(i0: term, j0: term, i1: term, j1: term): term 
  = TMapp(loop, quad(i0, j0, i1, j1)) where
  {
    val loop = 
    TMfix
    (
      "f", "zx",
      TMifnz(isneq(p1(zx),p3(zx)),  
      isneq(abs(p0(zx) - p2(zx)),abs(p1(zx) - p3(zx))),
     (* false *) TMfalse)
    )
  }


(* ****** ****** *)

val sf = TMvar("sf")

fun
safety_test2
(i1: term, j1: term, bd0: term, i3: term): term = 
TMapp(loop, quad(i1, j1, bd0, i3)) where
{
  val loop =
  TMfix
  ( 
    "f", "sf",
    TMifnz
    ( 
      p3(sf) >= 0,
      TMifnz
      ( 
        safety_test1(p0(sf), p1(sf), p3(sf), board_get(p2(sf), p3(sf))),
        TMapp(f, quad(p0(sf), p1(sf), p2(sf), p3(sf) - 1)),
        TMfalse (* false *) 
      ),
      TMtrue
    )
  )
}

(* ****** ****** *)

val N = 8

fun
search
(bd0: term, i: term, j: term, res: term): term =
TMapp(loop, quad(bd0, i, j, res)) where
{
val loop =
TMfix
("f", "x",
  TMifnz
  (
    p2(x) < N,
    TMifnz
    ( 
      safety_test2(p1(x), p2(x), p0(x), p1(x) - 1),
      TMifnz
      ( 
        iseq(p1(x) + 1, N),
        TMseq
        ( 
          TMseq
          ( 
            TMseq
            (
              TMopr("print", list0_tuple(TMstr("Solution #"))),
              TMopr("println", list0_tuple(p3(x)+1))
            ),
            print_board(board_set(p0(x), p1(x), p2(x)))
          ),
          TMapp(f, quad(p0(x), p1(x), p2(x)+1, p3(x)+1))
        ),
        TMapp
        (f, quad(board_set(p0(x), p1(x), p2(x)), p1(x)+1, TMint(0), p3(x)))
      ),
      TMapp( f, quad(p0(x), p1(x), p2(x)+1, p3(x)))
    ),
    TMifnz
    ( 
      p1(x) > 0 ,
      TMapp( f, quad(p0(x), p1(x)-1, board_get(p0(x), p1(x)-1) + 1, p3(x))),
      p3(x)
    )
  )
) // end fix
}// end search



(* ****** ****** *)
implement
main0() = () where
{
val test 
  = brd(TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0),TMint(0))

val searched 
  = search(test, TMint(0), TMint(0), TMint(0))

val () = println!("Solution: \n", interp(searched))
}
(* ****** ****** *)

(* end of [assign04_sol.dats] *)
