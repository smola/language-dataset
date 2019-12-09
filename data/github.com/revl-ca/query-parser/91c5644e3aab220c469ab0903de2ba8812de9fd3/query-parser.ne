@builtin "number.ne"
@builtin "whitespace.ne"

@{%
  const reserved = require('./reserved.json')

  const opExpr = operator => ([left, _, __, ___, right]) => ({
    type: 'operator',
    operator,
    left,
    right
  })

  const exprParser = ([left, _, operator, __, right]) => ({
    type: 'operator',
    operator,
    left,
    right
  })

  const betweenParser = ([_, __, ___, ____, begin_expr, _____, ______, _______, end_expr]) => ({
    type: 'between',
    begin_expr,
    end_expr
  })

  const chkReserved = (d, _, reject) => {
    const expression = d[0] + d[1].join('')

    if (reserved.indexOf(expression.toUpperCase()) > -1) return reject
    return { type: 'column', expression }
  }

  const flatten = d => d[0][0][0]
%}

main -> (statement):+ {% flatten %}

statement ->
    "(" _ expr _ ")" {% d => ({ type: 'where', condition: d[2] }) %}
  | _ expr _         {% d => ({ type: 'where', condition: d[1] }) %}

expr ->
    statement __ AND __ statement  {% opExpr('and') %}
  | statement __ OR __ statement   {% opExpr('or') %}
  | statement __ XOR __ statement  {% opExpr('xor') %}
  | boolean                        {% d => d[0] %}

boolean ->
    identifier __ comparison_type __ literal               {% exprParser %}
  | identifier __ BETWEEN __ primitive __ AND __ primitive {% betweenParser %}

comparison_type ->
    "="       {% d => d[0] %}
  | "<>"      {% d => d[0] %}
  | "<"       {% d => d[0] %}
  | "<="      {% d => d[0] %}
  | ">"       {% d => d[0] %}
  | ">="      {% d => d[0] %}
  | "!="      {% d => d[0] %}

literal ->
    primitive  {% d => d[0] %}
  | NULLX      {% d => ({ type: 'null' }) %}
  | TRUE       {% d => ({ type: 'boolean', value: true }) %}
  | FALSE      {% d => ({ type: 'boolean', value: false }) %}

primitive ->
    string  {% d => d[0] %}
  | decimal {% d => ({ type: 'decimal', value: d[0]} ) %}

identifier ->
    btstring                 {% d => ({ value: d[0] }) %}
  | [a-zA-Z_] [a-zA-Z0-9_]:* {% chkReserved %}

string ->
    dqstring {% d => ({ type: 'string', value: d[0] }) %}
  | sqstring {% d => ({ type: 'string', value: d[0] }) %}
  | btstring {% d => ({ type: 'string', value: d[0] }) %}

dqstring -> "\"" dstrchar:* "\"" {% d => d[1].join('') %}
sqstring -> "'"  sstrchar:* "'"  {% d => d[1].join('') %}
btstring -> "`"  [^`]:*    "`"   {% d => d[1].join('') %}

dstrchar ->
    [^\\"\n]       {% id %}
  | "\\" strescape {% d => JSON.parse('"' + d.join('') + '"') %}

sstrchar ->
    [^\\'\n]       {% id %}
  | "\\" strescape {% d => JSON.parse('"' + d.join("") + '"') %}
  | "\\'"          {% d => "'" %}

strescape ->
    ["\\/bfnrt]                                         {% id %}
  | "u" [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] {% d => d.join('') %}

AND -> [Aa] [Nn] [Dd] | "&&"
OR -> [Oo] [Rr] | "||"
XOR -> [Xx] [Oo] [Rr] | "^"
BETWEEN -> [Bb] [Ee] [Tt] [Ww] [Ee] [Ee] [Nn]
IS -> [Ii] [Ss]
NOT -> [Nn] [Oo] [Tt]
IN -> [Ii] [Nn]
LIKE -> [Ll] [Ii] [Kk] [Ee]
TRUE -> [Tt] [Rr] [Uu] [Ee]
FALSE -> [Ff] [Aa] [Ll] [Ss] [Ee]
NULLX -> [Nn] [Uu] [Ll] [Ll]

A -> "A" | "a"
B -> "B" | "b"
C -> "C" | "c"
D -> "D" | "d"
E -> "E" | "e"
F -> "F" | "f"
G -> "G" | "g"
H -> "H" | "h"
I -> "I" | "i"
J -> "J" | "j"
K -> "K" | "k"
L -> "L" | "l"
M -> "M" | "m"
N -> "N" | "n"
O -> "O" | "o"
P -> "P" | "p"
Q -> "Q" | "q"
R -> "R" | "r"
S -> "S" | "s"
T -> "T" | "t"
U -> "U" | "u"
V -> "V" | "v"
W -> "W" | "w"
X -> "X" | "x"
Y -> "Y" | "y"
Z -> "Z" | "z"
