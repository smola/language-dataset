#pragma -comment

comments
    { }
end comments    

keys
    'program 'end 'begin 'Write 'Readln 'uses 'crt 'ClrScr
    'TextBackground 'TextColor 'Sound 'Delay 'NoSound
    'Window 'GoToXY 'Byte 'Word 'ShortInt 'Integer 'LongInt
    'Real 'Char 'String 'Boolean 'mod 'div 'ReadKey
end keys    

compounds
    ':= '>= '<= '<>
end compounds    

define program
    [code]
end define

define code
    [opt comment]  [NL]  %optional comment to describe prog
    [programTitle] [NL][NL]
    [opt preStatements] [NL][NL]
    [repeat statement] [NL]
end define

define preStatements
    [repeat preStatement] [NL][NL]
end define    

define preStatement
    [comment]
    | [constant]
    | [variableDeclar]
    | [usesStatement]
end define

define statement
    [comment] [NL] %a comment is anything between {}
    | [windowStatements] [NL]
    | [conditionExpression] [NL]
    | [actionStatement] [NL]
    | [startStatement] [NL]
    | [writeStatement] [NL]
    | [readStatement] [NL]
    | [colorStatements] [NL]
    | [soundStatements] [NL]
    | [assignExpr] [NL]
    | [endStatement] [NL]
end define

define programTitle
    'program [id] ';
end define    

define endLine  %the '; is optional in condition statments
    ';
end define    
    
define assignExpr
    [id] [assignOp] [expression][opt endLine]
    | [id] [assignOp] [charExpression][opt endLine]
end define    

define assignOp
    ':=
end define

define charExpression
    [charlit]
    | [charExpression] '+ [charExpression]
end define    

define expression
    [ID_or_Number]
    | [expression] [op] [expression]
    | [expression] [conditionOp] [expression] %boolean values can be assign
end define

define op
    '+ | '- | '/ | '* | 'mod | 'div
end define

define conditionExpression
    [ifStatement] | [ifStatement] [elseStatement]
end define 

define ifStatement
    if [ifCondition] then
        [NL][IN] [statement] [EX]
    | if '([ifCondition]') [and_or] '([ifCondition]') then
         [NL][IN] [statement] [EX]
end define

define ifCondition
    [id] [conditionOp] [ID_or_Number]
end define    

define and_or
    'and | 'or
end define    

define elseStatement
    else [NL][IN] [statement] [EX]
end define    

define conditionOp
    '> | '< | '>= | '<= | '= | '<>
end define    

define ID_or_Number
    [id]
    | [number]
    | '-[number]
end define    

define variableDeclar
    var [NL][IN] [repeat variable] [EX]
end define

define variable
    [varName]: [type] [opt endLine] [NL]
end define    

define varName
    [id]
    | [id], [varName]
end define    

define constant
    const [NL][IN] [varName]: [type] '= [number]'; [NL][EX]
end define    

define type
    'Byte | 'Word | 'ShortInt | 'Integer | 'LongInt | 'Real
    | 'Char | 'String | 'Boolean
end define    

define startStatement
   begin [IN]
end define        

define endStatement
    [EX] 'end.
end define    

define usesStatement
    uses [NL][IN] [useTypes] [NL][EX]
end define    

define colorStatements
    TextBackground '( [color] ')[opt endLine]
    | TextColor '( [color] ')[opt endLine]
end define

define windowStatements
    Window '( [number], [number], [number], [number] ')[opt endLine]
    | GoToXY '( [number], [number] ')[opt endLine]
end define    

define soundStatements
    Sound '( [number] ')[opt endLine]
    | Delay '( [number] ')[opt endLine]
    | NoSound[opt endLine]
end define

define color
    [id]
end define    

define actionStatement
    ClrScr[opt endLine]
end define

define useTypes
    crt;
end define

define writeStatement
    Write '( [charlit] ')[opt endLine]
    | Writeln '( [charlit] ')[opt endLine]
    | Writeln '( [id] ')[opt endLine]
    | Writeln'([id], [charlit] ')[opt endLine]
end define

define readStatement
    Readln[opt endLine]
    | Readln([id])[opt endLine]
    | [id] [assignOp] 'ReadKey[opt endLine]
end define    

function main
    replace [program]
        P [program]
    by
        P
end function    
    
