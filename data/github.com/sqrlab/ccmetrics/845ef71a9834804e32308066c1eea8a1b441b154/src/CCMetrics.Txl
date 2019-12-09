%***********************************************************
% Static Metric Generator for Concurrent Java
% Jeremy S. Bradbury, Oct. 27, 2006

% Copyright 2006 J.S. Bradbury

% Revision history:

% Description: This program generates the following static
% metrics for concurrent Java (J2SE5.0) programs:
%	- # of classes
%	- # of methods
%	- # of statements
%		- # of critical regions
%		- # of sychronized blocks
%		- # of synchronized methods
%		- # of statements inside synchronized methods
%		- # of statements inside synchronized blocks
%	- # of semaphores
%	- # of locks
%	- # of latches
%	- # of cyclicbarriers
%	- # of exchangers

% Note: Program based on ccount.Txl program 
% written by J. Cordy (1993)

%***********************************************************

% Base grammar, and grammar override files
include "Java.Grm"
include "JavaCommentOverrides.Grm"

% Local grammar overrides to add optibutes for counting

redefine class_header
    [opt classlabel] [repeat modifier] 'class [class_name] [opt extends_clause] [opt implements_clause]
end redefine

define classlabel
	'CLASS
end define

redefine class_or_interface_name
	    [opt semLabel] 'Semaphore
	|	[opt lockLabel] 'Lock
	|	[opt latchLabel] 'CountDownLatch
	|	[opt barrierLabel] 'CyclicBarrier
	| 	[opt exchangerLabel] 'Exchanger
	|	[qualified_name]
end redefine

define semLabel
	'SEMAPHORE
end define

define lockLabel
	'LOCK
end define

define latchLabel
	'LATCH
end define

define barrierLabel
	'BARRIER
end define

define exchangerLabel
	'EXCHANGER
end define

redefine statement
        [opt statementlabel] [label_statement]        
    |   [opt statementlabel] [empty_statement]
    |   [opt statementlabel] [expression_statement]
    |   [opt statementlabel] [if_statement]
    |   [opt statementlabel] [switch_statement]
    |   [opt statementlabel] [while_statement]
    |   [opt statementlabel] [do_statement]
    |   [opt statementlabel] [for_statement]
    |   [opt statementlabel] [break_statement]
    |   [opt statementlabel] [continue_statement]
    |   [opt statementlabel] [return_statement]
    |   [opt statementlabel] [throw_statement]
    |   [opt statementlabel] [opt synchstmtlabel] [synchronized_statement]
    |   [opt statementlabel] [try_statement]
%    |   [opt statementlabel] [block]
    |   [block] %an empty block should not be counted
|	[comment_NL]
end redefine

define statementlabel
	'STATEMENT
end define

define synchstmtlabel
	'SYNCHSTMT
end define

redefine synchronized_statement
    'synchronized '( [expression] ')
        [synch_block_body]                  [NL]
end redefine

redefine constructor_declaration
    [opt mthdlabel] [NL] [repeat modifier] [constructor_declarator] [opt throws] [constructor_body] [NL][NL]
end redefine

redefine method_declaration
		[opt mthdlabel] [other_method_declaration]
	|	[opt mthdlabel] [opt synchmthdlabel] [synch_method_declaration]
end redefine

define other_method_declaration
     [repeat mdmodifier] [type_specifier] [method_declarator] [opt throws] [method_body]
end define

define synch_method_declaration
 [NL] [repeat modifier] [type_specifier] [method_declarator] [opt throws] [synch_method_body]
end define

redefine synch_block_body
	    [opt synchBodyLabel] [statement]        [NL][NL]
    |   ';             [NL][NL]
end redefine

redefine synch_method_body
        [opt synchBodyLabel] [block]        [NL][NL]
    |   ';             [NL][NL]
end redefine


define synchBodyLabel
	'SYNCHBODY
end define

%list of modifiers with synchronized removed
define mdmodifier
        'abstract
    |   'final
    |   'public
    |   'protected
    |   'private
    |   'static
    |   'transient
    |   'volatile
    |   'native
    |   'strictfp		% Added strictfp XG July 23, 2002
end define

define mthdlabel
	'MTHD
end define


define synchmthdlabel
	'SYNCHMTHD
end define

define id_or_number
        [id] | [number]
end define



%------------------------------------------------------------
% Main function that markups the program for the different 
% metric counts. The function then generates the header 
% information for metric output as well as the actual 
% metrics.
%------------------------------------------------------------
function main
	replace [program]
		UnmarkedProgram [program]

	%--------------------------------------------------
	% Markup program
	%--------------------------------------------------
	construct P [program]
		UnmarkedProgram [markup] [print]

	%--------------------------------------------------
	% Generate header information for metric output
	%--------------------------------------------------
	construct HeaderString1 [stringlit]
		'"--------------------------"
	construct HeaderString2 [stringlit]
		'" Java Concurrency Metrics "
	construct HeaderString3 [stringlit]
		'"--------------------------"
	construct Header1 [id]
		_ [unquote HeaderString1] [print]
	construct Header2 [id]
		_ [unquote HeaderString2] [print]
	construct Header3 [id]
		_ [unquote HeaderString3] [print]

	%--------------------------------------------------
	% Generate class count
	%--------------------------------------------------
	construct ClassLabels [repeat classlabel]
		_ [^ P]
	construct ClassLabelCount [number]
		_ [length ClassLabels]	[printMetric '"# of Classes = "] 


	%--------------------------------------------------
	% Generate statement count
	%--------------------------------------------------
    construct StatementCounts [program]
        P [getStatementCounts]

	%--------------------------------------------------
	% Generate counts for number of Semaphores, Locks,
	% Latches, Barriers, and Exchangers
	%--------------------------------------------------
	construct Sem [repeat semLabel]
		_ [^ P]
	construct SemCount [number]
		_ [length Sem] 	[printMetric '"# of Semaphores = "] 
	construct Lock [repeat lockLabel]
		_ [^ P]
	construct LockCount [number]
		_ [length Lock] 	[printMetric '"# of Locks = "] 
	construct Latch [repeat latchLabel]
		_ [^ P]
	construct LatchCount [number]
		_ [length Latch] 	[printMetric '"# of Latches  "] 
	construct Barrier [repeat barrierLabel]
		_ [^ P]
	construct BarrierCount [number]
		_ [length Barrier] 	[printMetric '"# of CyclicBarrier = "] 
	construct Exchanger [repeat exchangerLabel]
		_ [^ P]
	construct ExchangerCount [number]
		_ [length Exchanger] 	[printMetric '"# of Exchangers = "] 

	by
		%This program does not replace the program it just counts metrics
end function


%------------------------------------------------------------
% Function to collect method & statement counts, synchronized 
% block and synchronized method counts.
%------------------------------------------------------------
function getStatementCounts
    match [program]
        P [program]

	%--------------------------------------------------
	% Generate total method count
	%--------------------------------------------------
	construct Methods [repeat mthdlabel]
		_ [^ P]
	construct MethodCount [number]
		_ [length Methods] 	[printMetric '"# of Methods = "] 


	%--------------------------------------------------
	% Generate total statement count
	%--------------------------------------------------
	construct Statements [repeat statementlabel]
		_ [^ P]
	construct StatementCount [number]
		_ [length Statements]	[printMetric '"# of Statements = "] 


	%--------------------------------------------------
	% Generate synchronized block count
	%--------------------------------------------------
	construct SynchStatements [repeat synchstmtlabel]
		_ [^ P]
	construct SynchStatementCount [number]
		_ [length SynchStatements]	
		[printMetric '"# of Synchronized Statement Blocks = "]

	%--------------------------------------------------
	% Generate synchronized block statement count and
	% percentage of total number of statements
	%--------------------------------------------------
	construct SynchBlockBodys [repeat synch_block_body]
		_ [^ P] [makeEmbeddedSynchsEmpty]
	construct SynchBlockStmts [repeat statementlabel]
		_ [^ SynchBlockBodys]
	construct SynchBlockStmtsCount [number]
		_ [length SynchBlockStmts]	
	construct SynchBlockStmtsPercentage [number]
		SynchBlockStmtsCount [/ StatementCount] [* 100]	
		[printMetric2 
			'"  # of Statements inside Synchronized Blocks = "  
			SynchBlockStmtsCount '"of Total Statements"] 	



	%--------------------------------------------------
	% Generate synchronized method count and percentage
	% of total number of methods
	%--------------------------------------------------
	construct SynchMethodLabels [repeat synchmthdlabel]
		_ [^ P]
	construct SynchMethodCount [number]
		_ [length SynchMethodLabels]
	construct SynchMethodPercentage [number]
		SynchMethodCount [/ MethodCount] [* 100]	
		[printMetric2 
			'"# of Methods Synchronized = " 
			SynchMethodCount '"of Total Methods"] 

	%--------------------------------------------------
	% Generate synchronized method statement count and
	% percentage of total number of statements
	%--------------------------------------------------
	construct SynchMethods [repeat synch_method_declaration]
		_ [^ P]
	construct SynchMethodStmts [repeat statementlabel]
		_ [^ SynchMethods]
	construct SynchMethodStmtsCount [number]
		_ [length SynchMethodStmts]
	construct SynchMethodStmtsPercentage [number]
		SynchMethodStmtsCount [/ StatementCount] [* 100]	
		[printMetric2 
			'"  # of Statements inside Synchronized Methods = "  
			SynchMethodStmtsCount '"of Total Statements"] 


	%--------------------------------------------------
	% Generate total number of critical regions (i.e.,
	% synchronized blocks + synchronized methods)
	%--------------------------------------------------
	construct CriticalRegionCount [number]
		SynchMethodCount [+ SynchStatementCount] 
		[printMetric '"# of Critical Regions (i.e., Synchronized Blocks & Methods) = "] 


	%--------------------------------------------------
	% Generate a count of synch statements in synch
	% methods excluding synchronized blocks (this will
	% be needed later for total synch statement count
	%--------------------------------------------------
	construct SynchBodys [repeat synch_method_body]
		_ [^ P] [makeEmbeddedSynchsEmpty]
	construct SynchMthdEmbeddedStmts [repeat statementlabel]
		_ [^ SynchBodys]
	construct SynchMthdEmbeddedStmtsCount [number]
		_ [length SynchMthdEmbeddedStmts]	
	%	[printMetric 
	%		'"  # of Statements in Synchronized Method (excluding Synch statement bodies)  "] 

	%--------------------------------------------------
	% Generate count of total number of statements 
	% inside synchronized methods and blocks as well
	% as the percentage of the total number of 
	% statements
	%--------------------------------------------------
	construct TotalSynchStmtsCount [number]
		SynchMthdEmbeddedStmtsCount [+ SynchBlockStmtsCount] 
	construct TotalSynchStmtsPercentage [number]
		TotalSynchStmtsCount [/ StatementCount] [* 100]	
		[printMetric2 
			'"  # of Critical Region Statements = "  
			TotalSynchStmtsCount '"of Total Statements"] 	

end function

%------------------------------------------------------------
% This rule replaces embedded synchronized blocks by empty
% blocks so they won't be counted multiple times.
%------------------------------------------------------------
rule makeEmbeddedSynchsEmpty
	replace [statement]
		S [statement]
	deconstruct S
		'STATEMENT 'SYNCHSTMT ST [synchronized_statement]
	construct E [empty_statement]
		';
	by
		'STATEMENT E
end rule
	
%------------------------------------------------------------
% This function prints out metric count information.
%------------------------------------------------------------
function printMetric Metric [stringlit]
	match [number]
		N [number]
	construct MetricLabel [id]
		_ [unquote Metric]
	construct Output [repeat id_or_number]
		MetricLabel N
	construct Print [repeat id_or_number]
		Output [print]
end function

%------------------------------------------------------------
% This function prints out both metric count information as
% well as percentage information.
%------------------------------------------------------------
function printMetric2 MetricLabelString1 [stringlit] 
						metricValue [number] 
						MetricLabelString4 [stringlit]
	match [number]
		N [number]
	construct MetricLabel1 [id]
		_ [unquote MetricLabelString1] 
	construct MetricLabelString2 [stringlit]
		'"("
	construct MetricLabel2 [id]
		_ [unquote MetricLabelString2]
	construct MetricLabelString3 [stringlit]
		'"% "
	construct MetricLabel3 [id]
		_ [unquote MetricLabelString3]
	construct MetricLabel4 [id]
		_ [unquote MetricLabelString4] 
	construct MetricLabelString5 [stringlit]
		'")"
	construct MetricLabel5 [id]
		_ [unquote MetricLabelString5]
	
	construct Output [repeat id_or_number]
		MetricLabel1 metricValue MetricLabel2  N MetricLabel3 MetricLabel4 MetricLabel5
	construct Print [repeat id_or_number]
		Output [print]
end function

%------------------------------------------------------------
% This function marks up the program to enable metric counts.
%------------------------------------------------------------
function markup
    construct NClass [opt classlabel]
        % no optibute
    construct Class [opt classlabel]
        'CLASS

    construct NStmt [opt statementlabel]
        % no optibute
    construct Stmt [opt statementlabel]
        'STATEMENT
    construct NSynchStmt [opt synchstmtlabel]
        % no optibute
    construct SynchStmt [opt synchstmtlabel]
        'SYNCHSTMT

    construct NMthd [opt mthdlabel]
        % no optibute
    construct Mthd [opt mthdlabel]
        'MTHD
    construct NSynchMthd [opt synchmthdlabel]
        % no optibute
    construct SynchMthd [opt synchmthdlabel]
        'SYNCHMTHD

	construct NSynchBody [opt synchBodyLabel]
        % no optibute
    construct SynchBody [opt synchBodyLabel]
        'SYNCHBODY

     construct NSem [opt semLabel]
        % no optibute
    construct Sem [opt semLabel]
        'SEMAPHORE

    construct NLock [opt lockLabel]
        % no optibute
    construct Lock [opt lockLabel]
        'LOCK

    construct NLatch [opt latchLabel]
        % no optibute
    construct Latch [opt latchLabel]
        'LATCH

    construct NBarrier [opt barrierLabel]
        % no optibute
    construct Barrier [opt barrierLabel]
        'BARRIER

	construct NExchanger [opt exchangerLabel]
        % no optibute
    construct Exchanger [opt exchangerLabel]
        'EXCHANGER

    replace [program]
        P [program]
    by
        P 	[$ NClass Class]
			[$ NStmt Stmt]
        	[$ NSynchStmt SynchStmt]
			[$ NMthd Mthd]
			[$ NSynchMthd SynchMthd]
			[$ NSynchBody SynchBody]
			[$ NSem Sem]
			[$ NLock Lock]
			[$ NLatch Latch]
			[$ NBarrier Barrier]
			[$ NExchanger Exchanger]
end function
