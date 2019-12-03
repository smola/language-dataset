% TXL 7.7a4 Translation, Turing to C
% Andy Maloney, Queen's University, January 1995
% [part of CISC 499 undergraduate thesis project]

% The base C grammar
include "C.Grm"

% Comment out this line to disallow Gnu gcc extensions
include "CGnuOverrides.Grm"

% Comment out this line to disallow parsing of comments
include "CCommentOverrides.Grm"

% Translation rule sets
include "TranslateDecls.Rul"

% Main translation rule set
%function main
%	replace [program]
%		ExtDefs [repeat C_compilation_unit]
%	by
%		_ [changeExternalDef each ExtDefs]
%end function
%
%function changeExternalDef ExtDef [C_compilation_unit]
%	replace [repeat C_compilation_unit]
%		SoFar [repeat C_compilation_unit]
%	construct NewExtDef [C_compilation_unit]
%		ExtDef [translateCConst] [translateCType] [translateCVar]
%	by
%		SoFar [. NewExtDef]
%end function

function main
	replace [program]
		EntireInput [program]
	by
		EntireInput 
		[dropTQ]
		[changeType]
		[changeStruct]
		           
		 
end function