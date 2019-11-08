/*
 * NOTICE: This specification is a work-in-progress. Bugs and other
 * inconsistencies are to be expected. If you'd like to help improve
 * this specification, please submit a pull request to:
 * https://github.com/jeml-lang/jeml
 *
 * JEML (Just Enough Markup Language) v1.0.0 Specification
 * Initial Publishing Date: Tuesday Jul 31, 2019
 *
 * To learn more about the syntax of this specification,
 * visit: https://www.antlr.org/
*/

grammar jeml ;

/*
 * Parser Rules
 * ------------
*/

document
  : j_map+
  ;

j_map
  : j_key '{' ( j_map_body ) '}'
  | j_key '{' '}'
  ;

j_map_body
  : ( j_pair | j_map ) ( j_pair | j_map )*
  ;

j_pair
  : j_key j_value
  ;

j_key
  : KEY
  ;

j_value
  : j_bool
  | j_list
  | j_string
  | j_number
  ;

j_list
  : '[' j_value ( j_value )* ']'
  | '[' ']'
  ;

j_string  : STRING | MULTILINE_STRING ;
j_bool    : BOOLEAN ;
j_number  : ( j_octal | j_binary | j_hex | j_decimal | j_integer );

// Numerical types
j_octal   : OCTAL_NUMBER ;
j_binary  : BINARY_NUMBER ;
j_hex     : HEXIDECIMAL_NUMBER ;
j_decimal : DECIMAL ;
j_integer : INTEGER ;
j_complex : j_binary | j_octal | j_hex ;


/*
 * Lexer Rules
 * -----------
*/

BOOLEAN
  : 'true'
  | 'false'
  ;

STRING
  : '"' ( ESCAPE_SEQUENCE | SAFE_CODEPOINT )* '"'
  ;

INTEGER
  : '-'? DIGIT_09 ( DIGIT_09+ )?
  ;

DECIMAL
  : '-'? DIGIT_09 ( '.' DIGIT_09+ )? EXPONENT?
  | DIGIT_09 ( '_' DIGIT_09 )*
  | '.' DIGIT_09 ( '_' DIGIT_09 )*
  ;

HEXIDECIMAL_NUMBER
  : '0x' HEXIDECIMAL ( HEXIDECIMAL | '_' HEXIDECIMAL )*
  ;

BINARY_NUMBER
  : '0b' DIGIT_01 ( DIGIT_01 | '_' DIGIT_01 )*
  ;

OCTAL_NUMBER
  : '0o' DIGIT_07 ( DIGIT_07 | '_' DIGIT_07 )*
  ;

KEY
  : ( ALPHA | DIGIT_09 | '-' | '_' | '?' | '!' )+
  ;

MULTILINE_STRING
  : '"""' ( . )*? '"""'
  ;

EXPONENT
  : [Ee] [+\-]? DIGIT_09
  ;

fragment ESCAPE_SEQUENCE : '\\' ( ["\\/bfnrt] ) ;
fragment SAFE_CODEPOINT  : ~["\\\u0000-\u001F] ;

fragment ALPHA           : [A-Za-z] ;
fragment DIGIT_09        : [0-9]+ ;
fragment DIGIT_07        : [0-7]+ ;
fragment DIGIT_01        : [0-1]+ ;
fragment HEXIDECIMAL     : [A-Fa-f] | DIGIT_09 ;

COMMENT           : '#' ( ~[\n] )* -> skip ;
MULTILINE_COMMENT : '###' ( . )*? '###' -> skip ;
WS                : [ \t]+ -> skip ;
NL                : [\n\r]+ -> skip ;
