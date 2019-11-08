//
// Title:
// Concepts of Programming Languages
// Number: CAS CS 320
// Semester: Spring 2016
// Class Time: TR 12:30-2:00
// Instructor: Hongwei Xi (hwxiATbuDOTedu)
// Teaching Fellow: Hanwen Wu (hwwuATbuDOTedu)
//
(*
//
// Assignment #3
// Due Wednesday, February 10, 2016 at 11:59pm
//
*)
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
// HX:
// It is implemented
//
extern
fun
show_disk
(
  radius: int, x: int
) : void
//
(* ****** ****** *)
//
fun
nshow(n: int, c: char) =
  if n > 0 then (print(c); nshow(n-1, c)) else ()
//
(* ****** ****** *)

implement
show_disk(radius, x) = let
//
val () = assertloc(x >= 0)
val () = assertloc(radius >= x)
//
in
  nshow(radius-x, ' '); nshow(2*x, 'O'); nshow(radius-x, ' ');
end // end of [show_disk]

(* ****** ****** *)
//
// HX: 20 points
//
// Please think of a way to print 3 poles
// side-by-side. Note that 'radius' is the
// pole radius; 'height' is the pole height
//
// There is no strict specification as to what
// you need to do. Any "reasonable" output will
// earn your full credit (that is, 20 points).
//
extern
fun
show_poles
(
  radius: int, height: int
, xs1: list0(int), xs2: list0(int), xs3: list0(int)
) : void // end of [show_poles]
//
(* ****** ****** *)

(* end of [assign3.dats] *)
