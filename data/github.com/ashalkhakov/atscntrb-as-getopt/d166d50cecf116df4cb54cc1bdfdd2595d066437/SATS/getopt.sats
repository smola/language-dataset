
(* ****** ****** *)

datatype errcode =
  | ERRmissing_param
  | ERRunknown_argument
  | ERRunexpected_param

(* ****** ****** *)

datatype
optarity =
| OAnull of () (*no argument should be present*)
| OAoptional of () (*an argument is optional*)
| OArequired of () (*an argument is required*)

// option information
typedef optinfo = @{
  lname= string(*long name*)
, sname= char(*short name*)
, arity= optarity
, help= string(*help text*)
}

(* ****** ****** *)

// called when an argument passed from command-line
// matches one of the options passed to [getopt]
fun{env:vt@ype}
arg$match (
  env: &(env) >> _
, optind: size_t(*index into [opts]*)
, &optinfo >> _(*reference to option info*)
, key: string(*argument*)
, value: string(*parameter*)
): void
// called when an error is encountered
fun{env:vt@ype}
arg$error (
  env: &(env) >> _
, arg: string, index: size_t, errcode
): void

datatype
getopt_ret = // return codes
| GOnext of () // try iterating further
| GOstop of () // stop (at end of argument list)
| GOerr of () // error encountered

// the main function
fun{env:vt@ype}
getopt_env {n,n1,m:int | n1 <= n} (
  env: &(env) >> _
, argc: size_t(n)
, argv: &(@[string][n])
, cur: &size_t(n1) >> size_t(n2)(*current option*)
, opts: &(@[optinfo][m])(*option information*)
, optsz: size_t(m)
): #[n2:int | n2 <= n] getopt_ret

fun{env:vt@ype}
getopt_all$rest {n:int} (env: &(env) >> _, size_t(n), &(@[string][n])): void

fun{env:vt@ype}
getopt_all_env {n,n1,m:int | n1 <= n} (
  env: &(env) >> _
, argc: size_t(n)
, argv: &(@[string][n])
, cur: size_t(n1)(*current option*)
, opts: &(@[optinfo][m])(*option information*)
, optsz: size_t(m)
): getopt_ret

fun{}
getopt_help {m:int} (arg0: string, &(@[optinfo][m]), size_t(m)): void

fun{}
unit_test (): void
