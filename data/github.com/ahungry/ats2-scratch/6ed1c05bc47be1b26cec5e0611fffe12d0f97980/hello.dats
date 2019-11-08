// https://ats-lang.github.io/DOCUMENT/ATS2TUTORIAL/HTML/ATS2TUTORIAL-BOOK-onechunk.html
// https://github.com/ats-lang/ats-lang.github.io/blob/master/DOCUMENT/INT2PROGINATS/CODE/CHAP_FUNCTION/)
// my comment

(*
**
** A template for single-file ATS programs
**
*)
(* ****** ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
//
(* ****** ****** *)
//
// please write you program in this section
//
val _ = print "Hello, world!\n"
val _ = print 3
val xyz = ('A', 1, 2.0)
val (x, y, z) = xyz
val _ = print x
val _ = print xyz.0

// boxed tuple (undef ref here, hmm)
// val abc = '( 'A', 1, 2.0 )
// val a = abc.0 and b = abc.1 and c = abc.2

// val _ = print x

// record types (p11)
typedef
point2D = @{ x = double, y = double }
val theOrigin = @{x=0.0,y=0.5} : point2D
val y = theOrigin.y
val _ = print y
val @{x=theOrigin_x, y=theOrigin_y} = theOrigin
val _ = print y
val @{x=theOrigin_x, ...} = theOrigin
val _ = print theOrigin_x

fn square (x: int): int = x * x
val square2 = lam (x: int): int => x * x

val _ = print(square(square2(3)))

// defining a unary fn
typedef int2 = (int, int)
fn sqrsum2
  (xy: int2): int =
  let val x = xy.0 and y = xy.1 in x * x + y * y end
// end of [sqrsum2]

// page 25
val _ = print(sqrsum2((3,4)))
val _ = print(sqrsum2@(3,4))

// Need some working generics but its at end of book blah!
// fun {a:t@ype} out (x: a): void
// implement {a} out (x) = print_string "?"
// fn {a:t@ype} out (x: a): void = print(x)

// This will have to do for now
fn oi (x: int): void = begin print(x); print("\n"); end
fn od (x: double): void = begin print(x); print("\n"); end
fn os (x: string): void = begin print(x); print("\n"); end
fn oc (x: char): void = begin print(x); print("\n"); end

val _ = os("fin\n")

(* ****** ****** *)
implement main0 () = () // a dummy implementation for [main]

// The following is an 'until end of file' comment, 4 slashes, nice
////
begin
  print 'H'; print 'i'
end
