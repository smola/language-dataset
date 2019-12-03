@{%
    const merge = d => d.join('');
    const nth = n => d => d[n];
%}
@builtin "whitespace.ne"

file -> line:*              {% nth(0) %}

line -> op _ comment        {% nth(0) %}
      | label _ comment     {% nth(0) %}
comment -> null             {% d => null %}
         | ";" [a-zA-Z]:*   {% d => null %}

op -> "HALT"                {% d => ({op: d[0]}) %}
	| "DISPLAY" __ reg      {% d => ({op: d[0], reg: d[2]}) %}
	| "PRINT_STACK" __ number{% d => ({op: d[0], value: d[2]}) %}
	| "PUSH" __ reg         {% d => ({op: d[0], reg: d[2]}) %}
	| "POP" __ reg          {% d => ({op: d[0], reg: d[2]}) %}
	| "MOV" __ reg __ number{% d => ({op: d[0], reg: d[2], value: d[4]}) %}
	| "CALL" __ ident       {% d => ({op: d[0], label: d[2]}) %}
	| "RET"                 {% d => ({op: d[0]}) %}
	| "JMP" __ ident        {% d => ({op: d[0], label: d[2]}) %}
	| "JZ" __ ident         {% d => ({op: d[0], label: d[2]}) %}
	| "JPOS" __ ident       {% d => ({op: d[0], label: d[2]}) %}
	| "JNEG" __ ident       {% d => ({op: d[0], label: d[2]}) %}
	| "ADD" __ reg __ reg   {% d => ({op: d[0], regA: d[2], regB: d[4]}) %}
	| "SUB" __ reg __ reg   {% d => ({op: d[0], regA: d[2], regB: d[4]}) %}
	| "MUL" __ reg __ reg   {% d => ({op: d[0], regA: d[2], regB: d[4]}) %}
	| "DIV" __ reg __ reg   {% d => ({op: d[0], regA: d[2], regB: d[4]}) %}

label -> ident ":"          {% d => ({op: "LABEL", name: d[0]}) %}

number -> int                          {% d => parseInt(d[0]) %}
reg -> "R" int                         {% d => parseInt(d[1]) %}
ident -> [a-z,A-Z] [a-z,A-Z,0-9]:*     {% d => d[0] + merge(d[1]) %}

int -> digit                           {% id %}
     | digit digits                    {% merge %}
     | "-" digit                       {% merge %}
digits -> digit                        {% id %}
        | digit digits                 {% merge %}
digit -> [0-9]                         {% id %}
