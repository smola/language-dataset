// vim: filetype=ragel

%%{

  machine pack;

  include "pack_actions.rl";

  count = digit >start_digit digit* @count;

  platform      = [_!];
  little_endian = '<';
  big_endian    = '>';

  count_modifier        = '*' %rest | count?;
  modifier              = count_modifier | platform @non_native_error;
  platform_le_modifier  = (platform little_endian) | (little_endian platform);
  platform_be_modifier  = (platform big_endian) | (big_endian platform);
  le_modifier           = little_endian | platform_le_modifier;
  be_modifier           = big_endian | platform_be_modifier;

  # Integers
  short = ('S' | 's');
  S     = (short platform? count_modifier) %check_size %S;
  Sl    = (short le_modifier count_modifier) %check_size %Sl;
  Sb    = (short be_modifier count_modifier) %check_size %Sb;

  int   = ('I' | 'i');
  I     = (int platform? count_modifier) %check_size %I;
  Il    = (int le_modifier count_modifier) %check_size %Il;
  Ib    = (int be_modifier count_modifier) %check_size %Ib;

  long  = ('L' | 'l');
  L     = (long count_modifier) %check_size %L;
  Lp    = (long platform count_modifier) %check_size %Lp;
  Ll    = (long little_endian count_modifier) %check_size %Ll;
  Lb    = (long big_endian count_modifier) %check_size %Lb;
  Lpl   = (long platform_le_modifier count_modifier) %check_size %Lpl;
  Lpb   = (long platform_be_modifier count_modifier) %check_size %Lpb;

  int64 = ('Q' | 'q');
  Q     = (int64 modifier) %check_size %Q;
  Ql    = (int64 little_endian modifier) %check_size %Ql;
  Qb    = (int64 big_endian modifier) %check_size %Qb;

  C = (('C' | 'c') modifier) %check_size %C;
  n = ('n'         modifier) %check_size %n;
  N = ('N'         modifier) %check_size %N;
  v = ('v'         modifier) %check_size %v;
  V = ('V'         modifier) %check_size %V;

  # Floats
  D = (('D' | 'd') modifier) %check_size %D;
  E = (('E'      ) modifier) %check_size %E;
  e = (('e'      ) modifier) %check_size %e;
  F = (('F' | 'f') modifier) %check_size %F;
  G = (('G'      ) modifier) %check_size %G;
  g = (('g'      ) modifier) %check_size %g;

  # Moves
  X  = ('X' modifier) %X;
  x  = ('x' modifier) %x;
  at = ('@' modifier) %at;

  # Strings
  A = ('A' modifier) %string_check_size %to_str_nil %string_append %A;
  a = ('a' modifier) %string_check_size %to_str_nil %string_append %a;
  Z = ('Z' modifier) %string_check_size %to_str_nil %string_append %Z;

  # Others
  P = (('P' | 'p') modifier) %check_size %P;

  # Encodings
  B = ('B' modifier) %string_check_size %to_str_nil %B;
  b = ('b' modifier) %string_check_size %to_str_nil %b;
  H = ('H' modifier) %string_check_size %to_str_nil %H;
  h = ('h' modifier) %string_check_size %to_str_nil %h;
  M = ('M' modifier) %string_check_size %to_s %M;
  m = ('m' modifier) %string_check_size %b64_uu_size %to_str %m;
  U = ('U' modifier) %check_size %U;
  u = ('u' modifier) %string_check_size %b64_uu_size %to_str %u;
  w = ('w' modifier) %check_size %w;

  Ss = S | Sl | Sb;
  Is = I | Il | Ib;
  Ls = L | Lp | Ll | Lb | Lpl | Lpb;
  Qs = Q | Ql | Qb;

  integers  = C | Ss | Is | Ls | n | N | v | V | Qs;
  floats    = D | E | e | F | G | g;
  encodings = B | b | H | h | M | m | U | u | w;
  strings   = A | a | Z;
  moves     = X | x | at;
  others    = P;

  directives = integers | strings | encodings | moves | floats | others;

  main := (directives >start)* %done;

  write data nofinal noerror noprefix;
  write init;
  write exec;

}%%
