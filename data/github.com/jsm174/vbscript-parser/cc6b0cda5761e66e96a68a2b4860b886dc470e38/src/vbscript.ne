#===============================
# VB Script grammar.
#
# Ported VBS BNF to nearly from
# https://rosettacode.org/wiki/BNF_Grammar
#===============================

#===============================
# Rules
#===============================
			   		
Program              -> NLOpt GlobalStmtList
			   
#===============================
# Rules : Declarations
#===============================

ClassDecl            -> "Class" ExtendedID NL MemberDeclList "End" "Class" NL

MemberDeclList       -> MemberDecl MemberDeclList
                         | null

MemberDecl           -> FieldDecl
                         | VarDecl
                         | ConstDecl
                         | SubDecl
                         | FunctionDecl
                         | PropertyDecl

FieldDecl            -> "Private" FieldName OtherVarsOpt NL
                         | "Public" FieldName OtherVarsOpt NL

FieldName            -> FieldID "(" ArrayRankList ")"
                         | FieldID

FieldID              -> ID
                         | "Default"
                         | "Erase"
                         | "Error"
                         | "Explicit"
                         | "Step"

VarDecl              -> "Dim" Whitespace VarName OtherVarsOpt NL

VarName              -> ExtendedID "(" ArrayRankList ")"
                         | ExtendedID

OtherVarsOpt         -> "," Whitespace VarName OtherVarsOpt
                         | null

ArrayRankList        -> IntLiteral2 "," ArrayRankList
                         | IntLiteral2
                         | null

ConstDecl            -> AccessModifierOpt "Const" Whitespace ConstList NL

ConstList            -> ExtendedID Whitespace "=" ConstExprDef Whitespace "," Whitespace ConstList
                         | ExtendedID Whitespace "=" Whitespace ConstExprDef

ConstExprDef         -> "(" ConstExprDef ")"
                         | "-" ConstExprDef
                         | "+" ConstExprDef 
                         | ConstExpr 

SubDecl              -> MethodAccessOpt "Sub" ExtendedID MethodArgList NL MethodStmtList "End" "Sub" NL
                         | MethodAccessOpt "Sub" ExtendedID MethodArgList InlineStmt "End" "Sub" NL

FunctionDecl         -> MethodAccessOpt "Function" ExtendedID MethodArgList NL MethodStmtList "End" "Function" NL
                         | MethodAccessOpt "Function" ExtendedID MethodArgList InlineStmt "End" "Function" NL

MethodAccessOpt      -> "Public" "Default"
                         | AccessModifierOpt

AccessModifierOpt    -> "Public"
                         | "Private"
                         | null

MethodArgList        -> "(" ArgList ")"
                         | "(" ")"
                         | null

ArgList              -> Arg "," ArgList
                         | Arg

Arg                  -> ArgModifierOpt ExtendedID "(" ")"
                         | ArgModifierOpt ExtendedID

ArgModifierOpt       -> "ByVal"
                         | "ByRef"
                         | null

PropertyDecl         -> MethodAccessOpt "Property" PropertyAccessType ExtendedID MethodArgList NL MethodStmtList "End" "Property" NL                        

PropertyAccessType   -> "Get"
                         | "Let"
                         | "Set"

#===============================
# Rules : Statements
#===============================
                                                                           
GlobalStmt           -> OptionExplicit
                         | ClassDecl
                         | FieldDecl
                         | ConstDecl
                         | SubDecl
                         | FunctionDecl
                         | BlockStmt

MethodStmt           -> ConstDecl
                         | BlockStmt

BlockStmt            -> VarDecl
                         | RedimStmt
                         | IfStmt
                         | WithStmt
                         | SelectStmt
                         | LoopStmt
                         | ForStmt
                         | InlineStmt NL

InlineStmt           -> AssignStmt
                         | CallStmt
                         | SubCallStmt
                         | ErrorStmt
                         | ExitStmt
                         | "Erase" ExtendedID

GlobalStmtList       -> GlobalStmt GlobalStmtList
                         | null

MethodStmtList       -> MethodStmt MethodStmtList
                         | null
                        
BlockStmtList        -> BlockStmt BlockStmtList
                         | null
                        
OptionExplicit       -> "Option" Whitespace "Explicit" NL

ErrorStmt            -> "On" "Error" "Resume" "Next"
                         | "On" "Error" "GoTo" IntLiteral  # must be 0

ExitStmt             -> "Exit" "Do"
                         | "Exit" "For"
                         | "Exit" "Function"
                         | "Exit" "Property"
                         | "Exit" "Sub"

AssignStmt           -> LeftExpr "=" Expr
                         | "Set" LeftExpr "=" Expr
                         | "Set" LeftExpr "=" "New" LeftExpr
               
# Hack: VB Script allows to have construct a = b = c, which means a = (b = c)
# In this grammar we do not allow it in order to prevent complications with
# interpretation of a(1) = 2, which may be considered as array element assignment
# or a subroutine call: a ((1) = 2).
# Note: VBScript allows to have missed parameters: a ,,2,3,
# VM: If somebody knows a better way to do it, please let me know
               
SubCallStmt          -> QualifiedID SubSafeExprOpt CommaExprList
                         | QualifiedID SubSafeExprOpt
                         | QualifiedID "(" Expr ")" CommaExprList
                         | QualifiedID "(" Expr ")"
                         | QualifiedID "(" ")"
                         | QualifiedID IndexOrParamsList "." LeftExprTail SubSafeExprOpt CommaExprList
                         | QualifiedID IndexOrParamsListDot LeftExprTail SubSafeExprOpt CommaExprList
                         | QualifiedID IndexOrParamsList "." LeftExprTail SubSafeExprOpt
                         | QualifiedID IndexOrParamsListDot LeftExprTail SubSafeExprOpt

# This very simplified case - the problem is that we cannot use parenthesis in aaa(bbb).ccc (ddd)
SubSafeExprOpt       -> SubSafeExpr
                         | null
                         
CallStmt             -> "Call" LeftExpr

LeftExpr             -> QualifiedID IndexOrParamsList "." LeftExprTail
                         | QualifiedID IndexOrParamsListDot LeftExprTail
                         | QualifiedID IndexOrParamsList
                         | QualifiedID
                         | SafeKeywordID

LeftExprTail         -> QualifiedIDTail IndexOrParamsList "." LeftExprTail
                         | QualifiedIDTail IndexOrParamsListDot LeftExprTail
                         | QualifiedIDTail IndexOrParamsList
                         | QualifiedIDTail

# VB Script does not allow to have space between Identifier and dot:
# a . b - Error ; a. b or a.b - OK
QualifiedID          -> IDDot QualifiedIDTail
                         | DotIDDot QualifiedIDTail
                         | ID
                         | DotID

QualifiedIDTail      -> IDDot QualifiedIDTail
                         | ID
                         | KeywordID

KeywordID            -> SafeKeywordID
                         | "And"
                         | "ByRef"
                         | "ByVal"
                         | "Call"
                         | "Case"
                         | "Class"
                         | "Const"
                         | "Dim"
                         | "Do"
                         | "Each"
                         | "Else"
                         | "ElseIf"
                         | "Empty"
                         | "End"
                         | "Eqv"
                         | "Exit"
                         | "False"
                         | "For"
                         | "Function"
                         | "Get"
                         | "GoTo"
                         | "If"
                         | "Imp"
                         | "In"
                         | "Is"
                         | "Let"
                         | "Loop"
                         | "Mod"
                         | "New"
                         | "Next"
                         | "Not"
                         | "Nothing"
                         | "Null"
                         | "On"
                         | "Option"
                         | "Or"
                         | "Preserve"
                         | "Private"
                         | "Public"
                         | "Redim"
                         | "Resume"
                         | "Select"
                         | "Set"
                         | "Sub"
                         | "Then"
                         | "To"
                         | "True"
                         | "Until"
                         | "WEnd"
                         | "While"
                         | "With"
                         | "Xor"

SafeKeywordID        -> "Default"
                         | "Erase"
                         | "Error"
                         | "Explicit"
                         | "Property"
                         | "Step"  
                                        
ExtendedID           -> SafeKeywordID
                         | ID               

IndexOrParamsList    -> IndexOrParams IndexOrParamsList
                         | IndexOrParams

IndexOrParams        -> "(" Expr CommaExprList ")"
                         | "(" CommaExprList ")"
                         | "(" Expr ")"
                         | "(" ")"

IndexOrParamsListDot -> IndexOrParams IndexOrParamsListDot
                         | IndexOrParamsDot

IndexOrParamsDot     -> "(" Expr CommaExprList ")."
                         | "(" CommaExprList ")."
                         | "(" Expr ")."
                         | "(" ")."

CommaExprList        -> "," Expr CommaExprList
                         | "," CommaExprList
                         | "," Expr
                         | ","

#========= Redim Statement

RedimStmt            -> "Redim" RedimDeclList NL
                         | "Redim" "Preserve" RedimDeclList NL

RedimDeclList        -> RedimDecl "," RedimDeclList
                         | RedimDecl

RedimDecl            -> ExtendedID "(" ExprList ")"

#========= If Statement

IfStmt               -> "If" Expr "Then" NL BlockStmtList ElseStmtList "End" "If" NL
                         | "If" Expr "Then" InlineStmt ElseOpt EndIfOpt NL

ElseStmtList         -> "ElseIf" Expr "Then" NL BlockStmtList ElseStmtList
                         | "ElseIf" Expr "Then" InlineStmt NL ElseStmtList
                         | "Else" InlineStmt NL
                         | "Else" NL BlockStmtList
                         | null

ElseOpt              -> "Else" InlineStmt
                         | null

EndIfOpt             -> "End" "If"
                         | null

#========= With Statement

WithStmt             -> "With" Expr NL BlockStmtList "End" "With" NL

#========= Loop Statement

LoopStmt             -> "Do" LoopType Expr NL BlockStmtList "Loop" NL
                         | "Do" NL BlockStmtList "Loop" LoopType Expr NL
                         | "Do" NL BlockStmtList "Loop" NL
                         | "While" Expr NL BlockStmtList "WEnd" NL

LoopType             -> "While"
                         | "Until"

#========= For Statement

ForStmt              -> "For" ExtendedID "=" Expr "To" Expr StepOpt NL BlockStmtList "Next" NL
                         | "For" "Each" ExtendedID "In" Expr NL BlockStmtList "Next" NL

StepOpt              -> "Step" Expr
                         | null

#========= Select Statement

SelectStmt           -> "Select" "Case" Expr NL CaseStmtList "End" "Select" NL

CaseStmtList         -> "Case" ExprList NLOpt BlockStmtList CaseStmtList
                         | "Case" "Else" NLOpt BlockStmtList
                         | null

NLOpt                -> NL
                         | null

ExprList             -> Expr "," ExprList
                         | Expr

#===============================
# Rules : Expressions
#===============================
            
SubSafeExpr          -> SubSafeImpExpr

SubSafeImpExpr       -> SubSafeImpExpr "Imp" EqvExpr
                         | SubSafeEqvExpr

SubSafeEqvExpr       -> SubSafeEqvExpr "Eqv" XorExpr
                         | SubSafeXorExpr

SubSafeXorExpr       -> SubSafeXorExpr "Xor" OrExpr
                         | SubSafeOrExpr

SubSafeOrExpr        -> SubSafeOrExpr "Or" AndExpr
                         | SubSafeAndExpr

SubSafeAndExpr       -> SubSafeAndExpr "And" NotExpr
                         | SubSafeNotExpr

SubSafeNotExpr       -> "Not" NotExpr
                         | SubSafeCompareExpr

SubSafeCompareExpr   -> SubSafeCompareExpr "Is" ConcatExpr
                         | SubSafeCompareExpr "Is" "Not" ConcatExpr
                         | SubSafeCompareExpr ">=" ConcatExpr
                         | SubSafeCompareExpr "=>" ConcatExpr
                         | SubSafeCompareExpr "<=" ConcatExpr
                         | SubSafeCompareExpr "=<" ConcatExpr
                         | SubSafeCompareExpr ">"  ConcatExpr
                         | SubSafeCompareExpr "<"  ConcatExpr
                         | SubSafeCompareExpr "<>" ConcatExpr
                         | SubSafeCompareExpr "="  ConcatExpr
                         | SubSafeConcatExpr

SubSafeConcatExpr    -> SubSafeConcatExpr "&" AddExpr
                         | SubSafeAddExpr

SubSafeAddExpr       -> SubSafeAddExpr "+" ModExpr
                         | SubSafeAddExpr "-" ModExpr
                         | SubSafeModExpr

SubSafeModExpr       -> SubSafeModExpr "Mod" IntDivExpr
                         | SubSafeIntDivExpr

SubSafeIntDivExpr    -> SubSafeIntDivExpr "\\" MultExpr
                         | SubSafeMultExpr

SubSafeMultExpr      -> SubSafeMultExpr "*" UnaryExpr
                         | SubSafeMultExpr "/" UnaryExpr
                         | SubSafeUnaryExpr

SubSafeUnaryExpr     -> "-" UnaryExpr
                         | "+" UnaryExpr
                         | SubSafeExpExpr

SubSafeExpExpr       -> SubSafeValue "^" ExpExpr
                         | SubSafeValue

SubSafeValue         -> ConstExpr
                         | LeftExpr
#                         | "(" Expr ")"

Expr                 -> ImpExpr

ImpExpr              -> ImpExpr "Imp" EqvExpr
                         | EqvExpr

EqvExpr              -> EqvExpr "Eqv" XorExpr
                         | XorExpr

XorExpr              -> XorExpr "Xor" OrExpr
                         | OrExpr

OrExpr               -> OrExpr "Or" AndExpr
                         | AndExpr

AndExpr              -> AndExpr "And" NotExpr
                         | NotExpr

NotExpr              -> "Not" NotExpr
                         | CompareExpr

CompareExpr          -> CompareExpr "Is" ConcatExpr
                         | CompareExpr "Is" "Not" ConcatExpr
                         | CompareExpr ">=" ConcatExpr
                         | CompareExpr "=>" ConcatExpr
                         | CompareExpr "<=" ConcatExpr
                         | CompareExpr "=<" ConcatExpr
                         | CompareExpr ">"  ConcatExpr
                         | CompareExpr "<"  ConcatExpr
                         | CompareExpr "<>" ConcatExpr
                         | CompareExpr "="  ConcatExpr
                         | ConcatExpr

ConcatExpr           -> ConcatExpr "&" AddExpr
                         | AddExpr

AddExpr              -> AddExpr "+" ModExpr
                         | AddExpr "-" ModExpr
                         | ModExpr

ModExpr              -> ModExpr "Mod" IntDivExpr
                         | IntDivExpr

IntDivExpr           -> IntDivExpr "\\" MultExpr
                         | MultExpr

MultExpr             -> MultExpr "*" UnaryExpr
                         | MultExpr "/" UnaryExpr
                         | UnaryExpr

UnaryExpr            -> "-" UnaryExpr
                         | "+" UnaryExpr
                         | ExpExpr

ExpExpr              -> Value "^" ExpExpr
                         | Value

Value                -> ConstExpr
                         | LeftExpr
                         | "(" Expr ")"

ConstExpr            -> BoolLiteral
                         | IntLiteral2
                         | FloatLiteral
                         | StringLiteral
                         | DateLiteral
                         | Nothing
                         
BoolLiteral          -> "True"
                         | "False"

IntLiteral2           -> IntLiteral
                         | HexLiteral
                         | OctLiteral

Nothing              -> "Nothing"
                         | "Null"
                         | "Empty"    
                                                                                                 
#===============================
# Terminals
#===============================

NewLine        -> CR LF
               | CR
               | LF
               | ":"

# Special white space definition. Whitespace is either space or tab, which
# can be followed by continuation symbol '_' followed by new line character
Whitespace     -> WS:+
               | "_" WS:* CR:? LF:?

# Special comment definition
CommentLine    -> "'"
               | "Rem"

# Literals               
StringLiteral  -> "\"" ( STRING_CHAR | "\"\"" ):* "\""
IntLiteral     -> DIGIT:+
HexLiteral     -> "&H" HEX_DIGIT:+ "&":?
OctLiteral     -> "&" OCT_DIGIT:+ "&":?
FloatLiteral   -> DIGIT:* "." DIGIT:+ ( "E" [+-]:? DIGIT:+ ):? 
               | DIGIT:+ "E" [+-]:? DIGIT:+
DateLiteral    -> "#" DATE_CHAR:+ "#"

# Identifier is either starts with letter and followed by letter,
# number or underscore, or it can be escaped sequence of any printable
# characters ([] and [_$% :-) @] are valid identifiers)
ID          -> LETTER ID_TAIL:* 
               | "[" ID_NAME_CHAR:* "]"

# White space is not allowed to be before dot, but allowed to be after it.
IDDot          -> LETTER ID_TAIL:* "."
               | "[" ID_NAME_CHAR:* "]" "."
               | "And."
               | "ByRef."
               | "ByVal."
               | "Call."
               | "Case."
               | "Class."
               | "Const."
               | "Default."
               | "Dim."
               | "Do."
               | "Each."
               | "Else."
               | "ElseIf."
               | "Empty."
               | "End."
               | "Eqv."
               | "Erase."
               | "Error."
               | "Exit."
               | "Explicit."
               | "False."
               | "For."
               | "Function."
               | "Get."
               | "GoTo."
               | "If."
               | "Imp."
               | "In."
               | "Is."
               | "Let."
               | "Loop."
               | "Mod."
               | "New."
               | "Next."
               | "Not."
               | "Nothing."
               | "Null."
               | "On."
               | "Option."
               | "Or."
               | "Preserve."
               | "Private."
               | "Property."
               | "Public."
               | "Redim."
               | "Rem."
               | "Resume."
               | "Select."
               | "Set."
               | "Step."
               | "Sub."
               | "Then."
               | "To."
               | "True."
               | "Until."
               | "WEnd."
               | "While."
               | "With."
			   | "Xor."
			 
# The following identifiers should only be used in With statement.
# This rule must be checked by contextual analyzer.			   
DotID          -> "." LETTER ID_TAIL:*
               | "." "[" ID_NAME_CHAR:* "]"
               | ".And"
               | ".ByRef"
               | ".ByVal"
               | ".Call"
               | ".Case"
               | ".Class"
               | ".Const"
               | ".Default"
               | ".Dim"
               | ".Do"
               | ".Each"
               | ".Else"
               | ".ElseIf"
               | ".Empty"
               | ".End"
               | ".Eqv"
               | ".Erase"
               | ".Error"
               | ".Exit"
               | ".Explicit"
               | ".False"
               | ".For"
               | ".Function"
               | ".Get"
               | ".GoTo"
               | ".If"
               | ".Imp"
               | ".In"
               | ".Is"
               | ".Let"
               | ".Loop"
               | ".Mod"
               | ".New"
               | ".Next"
               | ".Not"
               | ".Nothing"
               | ".Null"
               | ".On"
               | ".Option"
               | ".Or"
               | ".Preserve"
               | ".Private"
               | ".Property"
               | ".Public"
               | ".Redim"
               | ".Rem" 
               | ".Resume"
               | ".Select"
               | ".Set"
               | ".Step"
               | ".Sub"
               | ".Then"
               | ".To"
               | ".True"
               | ".Until"
               | ".WEnd"
               | ".While"
               | ".With"
			   | ".Xor"
			   
DotIDDot     -> "." LETTER ID_TAIL:* "."
               | "." "[" ID_NAME_CHAR:* "]" "."
               | ".And."
               | ".ByRef."
               | ".ByVal."
               | ".Call."
               | ".Case."
               | ".Class."
               | ".Const."
               | ".Default."
               | ".Dim."
               | ".Do."
               | ".Each."
               | ".Else."
               | ".ElseIf."
               | ".Empty."
               | ".End."
               | ".Eqv."
               | ".Erase."
               | ".Error."
               | ".Exit."
               | ".Explicit."
               | ".False."
               | ".For."
               | ".Function."
               | ".Get."
               | ".GoTo."
               | ".If."
               | ".Imp."
               | ".In."
               | ".Is."
               | ".Let."
               | ".Loop."
               | ".Mod."
               | ".New."
               | ".Next."
               | ".Not."
               | ".Nothing."
               | ".Null."
               | ".On."
               | ".Option."
               | ".Or."
               | ".Preserve."
               | ".Private."
               | ".Property."
               | ".Public."
               | ".Redim."
               | ".Rem."
               | ".Resume."
               | ".Select."
               | ".Set."
               | ".Step."
               | ".Sub."
               | ".Then."
               | ".To."
               | ".True."
               | ".Until."
               | ".WEnd."
               | ".While."
               | ".With."
			   | ".Xor."
			   
NL                   -> NewLine NL
                         | NewLine
                     
                     
#===============================
# Character sets
#===============================                    
                         
LF -> [\x0A]
CR -> [\x0D]
LETTER -> [\x41-\x5A] | [\x61-\x7A]	
STRING_CHAR -> [\x01-\x21] | [\x23-\xD7FF] | [\xE000-\xFFEF]
DATE_CHAR -> [\x20-\x22] | [\x24-\x7E] | [\xA0]
ID_NAME_CHAR -> [\x20-\x5A] | [\x5C] | [\x5E-\x7E] | [\xA0]
DIGIT -> [0-9]
HEX_DIGIT -> [0-9a-f]
OCT_DIGIT -> [0-7]
WS -> [\x09] | [\x0B] | [\x0C] | [\x20] | [\xA0]
ID_TAIL -> [\x30-\x39] | [\x41-\x5A] | [\x61-\x7A] | "_"                     
