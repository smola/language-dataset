grammar Component;

packageDeclaration
  :  'package' Identifier ' {'  componentDeclaration*  '}'
  ;

componentDeclaration
  : 'component' Identifier componentBody
  ;

componentBody
  : '{' description? componentBodyDeclaration* '}'
  ;

componentBodyDeclaration
  : propDeclaration
  | slotDeclaration
  | eventDeclaration
  ;

propDeclaration
  : 'prop' Identifier
    ';'
  ;

slotDeclaration
  : 'slot' Identifier slotBody
  ;

slotBody
  : '{' description? slotBodyDeclaration* '}'
  ;

slotBodyDeclaration
  : packageName componentName
  ;

packageName : 'packageName' Identifier ';';
componentName: 'componentName' Identifier ';';

eventDeclaration
  : 'event' Identifier eventBody
  ;

eventBody
  : '{' description? eventBodyDeclaration* '}'
  ;

eventBodyDeclaration
  : type Identifier description? ';'
  ;


type
  : STRING
  | NUMBER
  | BOOLEAN
  | FUNCTION
  | OBJECT
  | ARRAY
  ;

description : DESCRIPTION;

PACKAGE : 'package';
COMPONENT : 'component';
PROP : 'prop';
EVENT : 'event';
SLOT : 'slot';


DOT : '.';
STRING : 'String';
NUMBER : 'Number';
BOOLEAN : 'Boolean';
FUNCTION : 'Function';
OBJECT : 'Object';
ARRAY : 'Array';

OpenBracket                : '[';
CloseBracket               : ']';
OpenParen                  : '(';
CloseParen                 : ')';
OpenBrace                  : '{';
CloseBrace                 : '}';
SemiColon                  : ';';
Identifier : [a-z][a-zA-Z0-9_$]*;
DESCRIPTION : '"'(~[\"])*?'"';
WS :  [ \t\r\n\u000C]+ -> skip ;
COMMENT :   '/*' .*? '*/' -> skip ;
LINE_COMMENT :   '//' ~[\r\n]* -> skip ;
