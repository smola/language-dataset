%***********************************************************
% ASM_lineCV[-2] (Add Sync around a Method from ConTest) Mutator for ARC
% (Automatic Repair of Concurrency Bugs)
% With variable targeting information (_V)
%
% Copyright David Kelk and Kevin Jalbert, 2012
%           David Kelk, 2013
%
% Based on ASK by J.S. Bradbury
%
% Typical invocation:
% txl ManageAccount.java ASM_RND.Txl - -outfile ManageAccount_1.java
% -outdir ASM/ -syncvar gbk
%
% Notes:
% - Java files must be named _1.java, _2.java due to the fact
%   that the second invocation of mutant generation will
%   overwrite the first.
% - Output directory must already exist.
%***********************************************************

%***********************************************************
% ASK (Add Synchronized Keyword To a Method) Mutator
% Jeremy S. Bradbury

% Copyright 2006 J.S. Bradbury

% Description: This mutation operator adds the synchronized
% keyword to any method that is not synchronized. We do
% not consider constructors because a constructor method
% can not be synchronized.

% NOTE: This operator generates a number of equivalent
% mutants. In the future we plan to restrict the ASK mutator
% to add a synchronized modifier ONLY to methods that
% have synchronized blocks (with the objective to cause
% a deadlock bug)
%***********************************************************

% Base grammar, and grammar override files
include "Java.Grm"
include "JavaCommentOverrides.Grm"

% Before and after for the method (Where the mutant comments go)
redefine method_or_constructor_declaration
    [attr labelM] [method_declaration]
  |
    [attr labelM] /* 'MUTANT: [stringlit] */
    [method_declaration]
    /* 'MUTANT: [stringlit] */ [NL]
  |
    [attr labelM] [constructor_declaration]
end redefine

define labelM
  'MUTATED
end define

redefine method_declaration
  [NL] [repeat modifier] [type_specifier] [rest_of_method_declaration]
end redefine

define rest_of_method_declaration
  [method_declarator] [opt throws] [method_body]
end define

% Before and after for the block (Where the synchronization goes)

redefine block
    '{                                        [NL][IN]
        [repeat declaration_or_statement]     [EX]
    '}
   |
    '{ [NL][IN]
        'synchronized '( [id] ') [NL]
        '{ [NL][IN]
          [repeat declaration_or_statement]     [EX]
        '}
    '}
end redefine

%------------------------------------------------------------
% Main rule, followed by other rules in topological order
%------------------------------------------------------------
function main
  % initialize and export a mutant count that will be used in
  % file name / folder name generation
      construct MutantCount [number]
    0
      export MutantCount

  % initialize and export the name of the current mutant which
  % will be used for folder name generation
  construct MutantOpName [stringlit]
    "ASM_V"
  export MutantOpName

  % Read in name of class and variable from ConTest
  import TXLargs [repeat stringlit]
    deconstruct * TXLargs
      "-syncvar" syncNm [stringlit] MoreOptions [repeat stringlit]

  construct syncvarName [id]
    _ [unquote syncNm]
  export syncvarName

  replace [program]
    P [program]
  by
    P [ProcessSynchronizeStatementMutations]
end function

%------------------------------------------------------------
% Start the mutation process at the global level.
% - IdentifyMutationClass limits the scope to the required
%   class (className)
% - The other classes are skipped over
% - Print/WriteToFile has to be at this level so that ALL
%   classes appear in the mutated file
%------------------------------------------------------------
rule ProcessSynchronizeStatementMutations
  replace [program]
    P [program]
  where
    P [?IdentifyMutationClass]
  by
    P [doSynchronizedInsertMutation]
      %[IdentifyMutationClass]
      %[print]
      [writeToFile]
      % Undo mutation and return to original program
      [undoSynchronizedInsertMutation]
end rule

%------------------------------------------------------------
% Start by matching the class name
% - Deconstruct a class declaration to get the [class_name]
% - Efficiency: Don't import a variable [className] until it
%   is needed
% - Efficiency: Don't call [?IdentifyMutationMethod]
%   until the proper class has been identified
%------------------------------------------------------------
function IdentifyMutationClass
  replace * [class_declaration]
    AClass [class_declaration]

  where
    AClass [?doSynchronizedInsertMutation]

  by
    AClass [doSynchronizedInsertMutation]
end function

%---------------------
% Match the class name
%---------------------

rule isSameClass ClsName [id]
  match * [id]
    ClsName
end rule

%------------------------------------------------------------
%
%------------------------------------------------------------
function doSynchronizedInsertMutation

  replace * [method_or_constructor_declaration]
    GenDecl [method_or_constructor_declaration]

  deconstruct GenDecl
    MethodDecl [method_declaration]

  % export the old declaration and reinsert it
  % once the mutant has been created and saved
  export GenDecl

  deconstruct MethodDecl
    MethodModifiers [repeat modifier]
    TypeSpec [type_specifier]
    RemainingDeclaration [rest_of_method_declaration]

  deconstruct RemainingDeclaration
    MD [method_declarator]
    OT [opt throws]
    MB [method_body]

  deconstruct MB
    BL [block]

  deconstruct BL
    '{
        DOS [repeat declaration_or_statement]
    '}

  import syncvarName [id]

  % create new block
  construct SynchBlock [block]
    '{
      'synchronized '( syncvarName ')
      '{
        DOS
      '}
    '}

  construct NewBody [method_body]
    SynchBlock

  construct NewRemDecl [rest_of_method_declaration]
    MD OT NewBody

  construct NewMethodDecl [method_declaration]
    MethodModifiers TypeSpec NewRemDecl

  construct NewGenDecl [method_declaration]
    NewMethodDecl

  % create mutant comment to appear in source
  construct MutantString [stringlit]
    "ASM (Add Sync around a Method from ConTest)"

  by
    % generate hidden attribute and comment
    'MUTATED /* 'MUTANT: MutantString */
    NewMethodDecl
    /* 'MUTANT: MutantString */
end function

%------------------------------------------------------------
%
%------------------------------------------------------------
function undoSynchronizedInsertMutation
  replace * [method_or_constructor_declaration]
    CurrGenDecl [method_or_constructor_declaration]

  % Import the old declaration
  import GenDecl [method_or_constructor_declaration]

  % extract method declaration
  deconstruct GenDecl
    SavedMethodDecl [method_declaration]

  deconstruct CurrGenDecl
    'MUTATED /* 'MUTANT: MutStr1 [stringlit] */
    CurMethDecl [method_declaration]
    /* 'MUTANT: MutStr2 [stringlit] */

  by
    % replace mutated declaration with original
    % but leave hidden attribute
    'MUTATED SavedMethodDecl

end function

%Include rules for writing mutants to files
include "WriteMutants.Txl"
