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

define assignExpr
    [id] [assignOp] [expression]';
    | [id] [assignOp] [charExpression]';
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
end define

define op
    '+ | '- | '/ | '* | 'mod | 'div
end define

define conditionExpression
    if [id] [conditionOp] [ID_or_Number] then
    [NL][IN] [statement] [NL][EX]
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
    [varName]: [type] '; [NL]
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
    TextBackground '( [color] ');
    | TextColor '( [color] ');
end define

define windowStatements
    Window '( [number], [number], [number], [number] ');
    | GoToXY '( [number], [number] ');
end define    

define soundStatements
    Sound '( [number] ');
    | Delay '( [number] ');
    | NoSound;
end define

define color
    [id]
end define    

define actionStatement
    ClrScr;
end define

define useTypes
    crt;
end define

define writeStatement
    Write '( [charlit] ');
    | Writeln '( [charlit] ');
    | Writeln '( [id] ');
    | Writeln'([id], [charlit] ');
end define

define readStatement
    Readln;
    | Readln([id]);
    | [id] [assignOp] 'ReadKey;
end define    

function main
    replace [program]
        P [program]
    by
        P
end function    
    
