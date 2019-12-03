%  BSD 3-Clause License
%  
%  Copyright (c) 2017, Ali ElShakankiry
%  All rights reserved.
%  
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%  
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%  
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%  
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%  
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include "../GRM/c.grm"
include "../GRM/ASNOne.Grm"

redefine type_rule_definition
	% scl5
	...
	% C
	| [function_definition_or_declaration]
end redefine

redefine construction_assignment_statement
	% scl5
	[decl] '::= [type_decision] [opt scl_additions]				[NL]
	% C
	| [function_definition_or_declaration]
	| [repeat preprocessor]
end redefine

redefine program
	...
	| [repeat preprocessor]
		[repeat rule_definition]
		[opt preprocessor]
end redefine

redefine member_declaration
	...
	| [decl_specifiers] [list init_declarator] [semi]
end redefine

redefine constant
	...
	| 'NULL
end redefine

redefine alternative_decision
	...
	| [repeat preprocessor]
end redefine

redefine preprocessor
	...
	| [repeat alternative_decision]
end redefine

redefine element_type
   [named_type] [opt position_value]
   | [named_type] 'OPTIONAL [opt position_value]
   |	 [named_type] 'DEFAULT [value] 
   | 	 [id]'COMPONENTS 'OF [id] 
end redefine

define position_value
  'POS
end define

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main program

function main
	replace [program]
		P [program]
	construct AuxStructs [repeat rule_definition]
	export AuxStructs 
	construct AuxFunctionHeaders [repeat rule_definition]
	export AuxFunctionHeaders
	construct RULES [repeat rule_definition]
		_ [^ P]
	construct DECS [repeat construction_assignment_statement]
		_ [^ P]
	construct DTIDIncludes [repeat preprocessor]
	export DTIDIncludes
	by
		P %[checkBuiltinTypes RULES]
			[createSequenceStructs]
			[createTypeDecisionStructs]
			[structsOnly DECS]
end function

%rule checkBuiltinTypes RULES [repeat rule_definition]
%	replace $ [rule_definition]
%		RD [rule_definition]
%	deconstruct * [rule_definition] RD
%
%	by
%		
%end rule

rule createSequenceStructs
	replace $ [type_rule_definition]
		LONG [id] '^ SHORT [id] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct body [repeat member_declaration]
		_ [createStructBody each LE]
	construct checkCallback [opt scl_additions]
		OP [checkForSequenceCallback LONG]
	by
		'typedef 'struct {
			body
		} LONG ;
end rule

function checkForSequenceCallback LONG [id]
	replace [opt scl_additions]
		GR [opt encoding_grammar_indicator] SZ [opt size_markers_block] 
		TR [opt transfer_rules_block]
    	CB [opt constraints_block] 
    deconstruct * [transfer_statement] TR
    	'Callback
    construct callName [id]
    	_ [+ LONG] [+ "_callback"]
    construct header [rule_definition]
    	void callName '( LONG * LONG[tolower], PDU *thePDU ');
    import AuxFunctionHeaders [repeat rule_definition]
    export AuxFunctionHeaders
    	AuxFunctionHeaders [. header]
    by
    	GR SZ TR CB
end function

rule createTypeDecisionStructs
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '::= '( TR [type_reference] RTR [repeat alternative_decision] ') OPT [opt scl_additions]
	construct body [repeat member_declaration]
		_ [addTypeDecisionBody TR RTR] [addTypeDecisionBodyDotID TR RTR]
	import AuxStructs [repeat rule_definition]
	construct auxName [id]
		LONG [+ "List"]
	construct checkCallback [opt scl_additions]
		OPT [checkForTDCallbacks TR RTR]
	construct auxStruct [rule_definition]
		'typedef 'struct '{
			LONG '* 'data';
			'struct auxName '* 'next';
		'} auxName;
	export AuxStructs
		AuxStructs [. auxStruct]
	by
		'typedef 'struct {
			'uint32_t 'type';
			'union '{
				body
			'} 'ptr;
		} LONG ;
end rule

function checkForTDCallbacks TPR [type_reference] RTR [repeat alternative_decision]
	replace [opt scl_additions]
		GR [opt encoding_grammar_indicator] SZ [opt size_markers_block] 
		TR [opt transfer_rules_block]
    	CB [opt constraints_block] 
    deconstruct * [transfer_statement] TR
    	'Callback
    deconstruct TPR
    	LONG [id] OPT [opt dotID]
   construct callName [id]
    	_ [+ LONG] [+ "_callback"] 	
    construct header [rule_definition]
    	void callName '( LONG * LONG[tolower], PDU *thePDU ');
    construct restHeaders [repeat rule_definition]
    	_ [constructFunctionHeaders each RTR]
    import AuxFunctionHeaders [repeat rule_definition]
    export AuxFunctionHeaders
    	AuxFunctionHeaders [. header] [. restHeaders]
    by
    	GR SZ TR CB
end function

function constructFunctionHeaders TR [alternative_decision]
	replace [repeat rule_definition]
		RL [repeat rule_definition]
	deconstruct TR
		'| LONG [id] OPT [opt dotID]
   construct callName [id]
    	_ [+ LONG] [+ "_callback"] 			
	construct header [rule_definition]
    	void callName '( LONG * LONG[tolower], PDU *thePDU ');
	by
		RL [. header]
end function

%DONE NOTE: not dealing with opt dotID type_reference (imported type references)
function addTypeDecisionBody typeRef [type_reference] RaltDec [repeat alternative_decision]
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	deconstruct typeRef
		ID [id]
	construct decl [member_declaration]
		ID ID[tolower];% = NULL;
		%ID '* ID [tolower] '= 'NULL ';
	by
		MD [. decl] [addAltDecision each RaltDec] [addAltDecisionDotID each RaltDec]
end function

function addTypeDecisionBodyDotID typeRef [type_reference] RaltDec [repeat alternative_decision]
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	deconstruct typeRef
		ID [id] OP [opt dotID]
	deconstruct OP
		'. ID2 [id]
	construct INCLUDE [stringlit]
		_ [+ "#include \""] [+ ID] [+ "_Generated.h\""]
	construct INCLUDEPRE [opt preprocessor]
		_ [parse INCLUDE]
	deconstruct INCLUDEPRE
		INCLUDEFINAL [preprocessor]
	import DTIDIncludes [repeat preprocessor]
	export DTIDIncludes
		DTIDIncludes [. INCLUDEFINAL]
	construct decl [member_declaration]
		ID2 ID2[tolower];% = NULL;
		%ID '* ID [tolower] '= 'NULL ';
	by
		MD [. decl][addAltDecision each RaltDec] [addAltDecisionDotID each RaltDec]
end function

function addAltDecision altDec [alternative_decision]
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	deconstruct altDec
		'| ID [id] 
	construct decl [member_declaration]
		ID ID[tolower];
		%ID '* ID [tolower] '= 'NULL ';
	by
		MD [. decl]
end function

function addAltDecisionDotID altDec [alternative_decision]
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	deconstruct altDec
		'| ID [id] OP [opt dotID]
	deconstruct OP
		'. ID2 [id]
	construct INCLUDE [stringlit]
		_ [+ "#include \""] [+ ID] [+ "_Generated.h\""]
	construct INCLUDEPRE [opt preprocessor]
		_ [parse INCLUDE]
	deconstruct INCLUDEPRE
		INCLUDEFINAL [preprocessor]
	import DTIDIncludes [repeat preprocessor]
	export DTIDIncludes
		DTIDIncludes [. INCLUDEFINAL]
	construct decl [member_declaration]
		ID2 ID2[tolower];
		%ID '* ID [tolower] '= 'NULL ';
	by
		MD [. decl]
end function

function createStructBody LE [element_type]
	replace  [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [addSizeBasedType LE]
			[addSizeBasedOptionalType LE]
			[addSetOfType LE]
			[addInteger LE]
			[addReal4 LE]
			[addReal8 LE]
			[addDynamicOctetString LE]
			[addStaticOctetString LE]
			[addStaticOctetStringLarge LE]
end function

% this is a 'size_based_type'
function addSizeBasedType LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] TYPE [id] '('SIZE 'DEFINED') OP [opt endian] OPSL [opt slack] POS [opt position_value]
	construct decl [member_declaration]
		TYPE SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

% this is a 'size_based_type'
function addSizeBasedOptionalType LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] TYPE [id] '('SIZE 'DEFINED') OP [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	construct decl [member_declaration]
		TYPE * SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function addSetOfType LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '('SIZE 'CONSTRAINED') POS [opt position_value]
	construct decl [repeat member_declaration]
		'unsigned 'long SHORT[+ "length"][tolower]';
		'unsigned 'long SHORT[+ "Count"][tolower]';
		TYPE '* SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

% this is an 'integer_type' of builtin_type'
function addInteger LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'INTEGER '( 'SIZE NUM [number] 'BYTES ') OP [opt endian] POS [opt position_value]	
	construct Eight [number]
		'8
	where
		NUM [<= 8]
	construct finalNum [number]
		NUM [checkgt4] [check3]
	construct NumBits [number]
		Eight [* finalNum]
	construct IntType [id]
	   _ [+ 'uint] [+ NumBits] [+ '_t]
	construct decl [member_declaration]
		IntType SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function addReal4 LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'REAL '( 'SIZE NUM [number] 'BYTES ') OP [opt endian]
	where
		NUM [= 4]
	construct type [id]
		_ [+ 'float]
	construct decl [member_declaration]
		type SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function addReal8 LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'REAL '( 'SIZE NUM [number] 'BYTES ') OP [opt endian]
	where
		NUM [= 8]	
	construct type [id]
		_ [+ 'double]
	construct decl [member_declaration]
		type SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function checkgt4
	replace [number]
		NUM [number]
	where
		NUM [> 4]
	where 
		NUM [< 8]
	by
		8
end function

function check3
	replace [number]
		NUM [number]
	where
		NUM [= 3]
	by
		4
end function

function addDynamicOctetString LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '( 'SIZE 'CONSTRAINED ') OP [opt endian] OPSL [opt slack] POS [opt position_value]
	construct decl [repeat member_declaration]
		unsigned 'long SHORT[+ "_length"][tolower]';
		'unsigned 'char '* SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function addStaticOctetString LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '( 'SIZE NUM [number] 'BYTES ') OP [opt endian] OPSL [opt slack] POS [opt position_value]
	where
		NUM [<= 8]
	construct Eight [number]
		'8
	construct finalNum [number]
		NUM [checkgt4] [check3]
	construct NumBits [number]
		Eight [* finalNum]
	construct IntType [id]
	   _ [+ 'uint] [+ NumBits] [+ '_t]
	construct decl [member_declaration]
		IntType SHORT[tolower]';
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function addStaticOctetStringLarge LE [element_type]
	deconstruct LE
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '( 'SIZE NUM [number] 'BYTES ') OP [opt endian] OPSL [opt slack] POS [opt position_value]
	where
		NUM [> 8]
	construct decl [member_declaration]
		'unsigned 'char SHORT[tolower]'[NUM'];
	replace [repeat member_declaration]
		MD [repeat member_declaration]
	by
		MD [. decl]
end function

function structsOnly DECS [repeat construction_assignment_statement]
	replace [program]
		ID [id] 'DEFINITIONS TAG [opt tag_default] ::= 'BEGIN
		EX [opt export_block]
		IM [opt import_block]
		BODY [repeat rule_definition]
		'END
	import AuxStructs [repeat rule_definition]
	import AuxFunctionHeaders [repeat rule_definition]
	construct IFNDEF [stringlit]
		_ [+ "#ifndef "] [+ ID] [+ "_H"]
	construct IFNDEFPRE [opt preprocessor]
		_ [parse IFNDEF]
	deconstruct IFNDEFPRE
		IFNDEFFINAL [preprocessor]
	construct DEFINE [stringlit]
		_ [+ "#define "] [+ ID] [+ "_H"]
	construct DEFINEPRE [opt preprocessor]
		_ [parse DEFINE]
	deconstruct DEFINEPRE
		DEFINEFINAL [preprocessor]
	import DTIDIncludes [repeat preprocessor]
	construct INCLUDES [repeat preprocessor]
		IFNDEFFINAL
		DEFINEFINAL
		'#include <stdio.h>
		'#include <stdint.h>
		'#include <stdlib.h>
		'#include <string.h>
		'#include <inttypes.h>
		'#include "packet.h"
		'#include "globals.h"
	construct currentVal [number]
		0
	export currentVal 
	%construct DEFINESLIST [repeat preprocessor]
	%construct DEFINES [repeat construction_assignment_statement]
	%	DECS [genDefines]
	construct structs [repeat struct_or_union_specifier]
		_ [^ BODY]
	construct AuxDefines [repeat id]
		_ [genIDS each structs]
	construct finalDefines [repeat preprocessor]
		_ [genDefines each AuxDefines]
	%construct DEFINESFINAL [repeat preprocessor]
	%	_ [^ DEFINES]
	construct mainParse [id]
		_ [+ "parse"] [+ ID] %[+ "Packet"]
	construct mainDS [id]
		_ [+ "PDU_"] [+ ID]
	construct freeFunc [id]
		_ [+ "freePDU_"] [+ ID]
	construct mainParseFunction [rule_definition]
		bool mainParse(mainDS * mainDS[tolower], PDU * thePDU, char * name, uint8_t endianness); %struct HeaderInfo * headerinfo, 
	construct mainFreeFunction [rule_definition]
		void freeFunc(mainDS * 'mainpdu);
	construct FULLBODY [repeat rule_definition]
	by
		INCLUDES [. DTIDIncludes]
		finalDefines
		%AuxFunctionHeaders [. BODY] %[reverseBody] %[. AuxStructs]
		FULLBODY [. BODY] 
			[. AuxFunctionHeaders]
			[. mainParseFunction]
			[. mainFreeFunction]
		'#endif
end function

%function reverseBody 
%    replace [repeat rule_definition]
%        FD [rule_definition] MoreFDs [repeat rule_definition]
%    by
%        MoreFDs [reverseBody] [. FD]
%end function


function genDefines ID [id]
	replace [repeat preprocessor]
		DEFINES [repeat preprocessor]
	import currentVal [number]
	construct nextVal [number]
		currentVal [+ 1]
	export currentVal
		nextVal
	construct IDVAL [id]
		ID [+ "_VAL"]
	construct DEFINE [stringlit]
		_ [+ "#define "] [+ IDVAL] [+ " ("] [+ nextVal] [+ ")"]
	construct DEFINEPRE [opt preprocessor]
		_ [parse DEFINE]
	deconstruct DEFINEPRE
		DEFINEFINAL [preprocessor]
	by
		DEFINES [. DEFINEFINAL]
end function

function genIDS fullStruct [struct_or_union_specifier]
	replace [repeat id]
		IDS [repeat id]
	%'typedef 
	deconstruct fullStruct
		'struct {
			'uint32_t 'type;
			'union body [struct_or_union_body] 'ptr;
		} %LONG [reference_id]
	construct bodyIDS [repeat member_declaration]
		_ [^ body]
	construct bodyNames [repeat id]
		_ [getEachID each bodyIDS]
	construct newIDS [repeat id]
		_ [checkBody IDS each bodyNames]
	by
		IDS [. newIDS]
end function

function getEachID bodyName [member_declaration]
	replace [repeat id]
		IDS [repeat id]
	deconstruct bodyName
		ID1 [id] ID2[id] ;
	by
		IDS [. ID1]
end function

function checkBody IDLIST [repeat id] bodyName [id]
	replace [repeat id]
		IDS [repeat id]
	deconstruct not * [id] IDLIST
		bodyName
	by
		IDS [. bodyName]
end function
