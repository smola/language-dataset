@{%
const ast = require('./ast.js');
const moo = require("moo");

const NEWLINE     = '\\n'
const SPACE       = ' ';
const WS          = `${SPACE}+`;
const DIGIT       = '[0-9]';
const NZERO_DIGIT = '[1-9]';
const IDENT_CHAR  = '[a-zA-Z_]';
const NAME        = `${IDENT_CHAR}(?:${IDENT_CHAR}|${DIGIT})*`;
const STR_CHAR    = '[^\\n\'\"]';
const STR_ESCAPE  = '\\\\.';
const STR_ITEM    = `(?:${STR_CHAR}|${STR_ESCAPE})`;
const STR         = `'${STR_ITEM}*'|"${STR_ITEM}*"`;
const INT         = `(?:0|${NZERO_DIGIT}${DIGIT}*)`;
const FLOAT       = `${INT}\\.${DIGIT}*`;
const KEYWORDS    = ['False', 'await', 'else', 'import', 'pass', 'None',
                     'finally', 'is', 'return', 'and', 'continue', 'for',
                     'break', 'except', 'in', 'raise', 'True', 'class',
                     'lambda', 'try', 'as', 'def', 'from', 'nonlocal', 
                     'while', 'assert', 'del', 'global', 'not', 'with',
                     'async', 'elif', 'if', 'or', 'yield'];
const OPERATORS   = ['+', '-', '*', '**', '/', '//', '%', 
                     '+=', '-=', '*=', '**=', '/=', '//=', '%=', 
                     '<', '>', '<=', '>=', '==', '!='];
const DELIMS      = ['(', ')', '[', ']', '{', '}', ',', ':', '.', ';', '=', '->'];

const lexer = moo.compile({
  newline: { match: new RegExp(NEWLINE), lineBreaks: true },
  ws: new RegExp(WS),
  name: {
    match: new RegExp(NAME), 
    type: moo.keywords({ keyword: KEYWORDS }),
  },
  op: OPERATORS,
  delim: DELIMS,
  str: new RegExp(STR), 
  float: new RegExp(FLOAT), 
  int: new RegExp(INT), 
});
%}

@lexer lexer

expr -> term (_ ("+"|"-") _ term):* {%
  ([term, rest]) => {
    const terms = [term];
    for(let [, [op], , term2] of rest) {
      terms.push(new ast.Op(op.text), term2);
    }
    
    return new ast.Expr(terms);
  }
%}
term -> factor (_ ("*"|"/"|"//"|"%") _ factor):* {%
  ([factor, rest]) => {
    const factors = [factor];
    for(let [, [op], , factor2] of rest) {
      factors.push(new ast.Op(op.text), factor2);
    }
    
    return new ast.Term(factors);
  }
%}
factor -> ("+"|"-") factor {% ([[op], factor]) => new ast.Factor(op.text, factor) %}
        | power {% ([power]) => power %}
power -> atom_expr (_ "**" _ factor):? {%
  ([atom_expr, rest]) => {
    if(rest === null) {
      return new ast.Power(atom_expr);  
    }

    let [, op, , factor] = rest;
    return new ast.Power(atom_expr, factor);
  }
%}
atom_expr -> atom trailer:* {% 
  ([atom, trailers]) => new ast.AtomExpr(atom, trailers)
%}
trailer -> "." %name {% ([, name]) => name %}
atom -> %name   {% ([tkn]) => new ast.Atom(tkn, tkn.text) %}
      | %str    {% ([tkn]) => new ast.Atom(tkn, tkn.text) %}
      | %float  {% ([tkn]) => new ast.Atom(tkn, parseFloat(tkn.text)) %} 
      | %int    {% ([tkn]) => new ast.Atom(tkn, parseInt(tkn.text)) %} 
      | bool    {% ([[tkn]]) => new ast.Atom(tkn, tkn.text === 'True') %}    
bool -> "True" | "False"
_ -> %ws:?
__ -> %ws