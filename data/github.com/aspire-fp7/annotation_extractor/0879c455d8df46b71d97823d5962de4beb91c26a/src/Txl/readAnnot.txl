% Copyright (c) 2016 Fondazione Bruno Kessler www.fbk.eu
% Author Marino Ceccato

% Permission is hereby granted, free of charge, to any person obtaining a copy 
% of this software and associated documentation files (the "Software"), to 
% deal in the Software without restriction, including without limitation the 
% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
% sell copies of the Software, and to permit persons to whom the Software is 
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in 
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
% IN THE SOFTWARE.



% This program reads a prerpocessed C file and emit a json fime containing the annotations
% it assumes the program to be normalized

include "c.grm"
include "annotations_grammar.grm"
include "block_ann.grm"
include "json.grm"


% Grammar redefinitions to make the program work

redefine json_object
	'{  [IN] [list json_pair] [EX] [NL]'} [attr number] [NL]
end define

redefine attribute_spec
    [aspire_attribute_prefix] [stringlit] [aspire_attribute_suffix]
    | '__attribute__ '( '( [list attribute_content] ') ') 
end redefine

redefine program
	[compilation_unit]
  |	[json_program]
end redefine

redefine aspire_pragma_begin
	[parsing_annotation] '_ASPIRE_BLOCK_BEGIN '( [stringlit_or_protection_annotation] ') [NL]
end redefine

redefine aspire_pragma_end
	[parsing_annotation] '_ASPIRE_BLOCK_END '( ') [NL]
end redefine

redefine aspire_attribute_suffix
	') ')') [parsing_annotation]
end define


define parsing_annotation
	[srcfilename] [srclinenumber]
end define




% Main program that fires the algorithm.
% The global ANNOTATIONS is used to accumulate annotatuions in json format as they are found in the code

function main
	replace [program]
		P [program]
	export ANNOTATIONS [list json_value] _
	by
		P [check_normalized]
		  [extract_data_annotations]
		  [extract_code_annotations]
		  [replace_program_with_result]
		  [sort]
end function


% check preconditions on the normalized code

rule check_normalized
	replace $ [declaration] 
		a_declaration [declaration]
	% it is an annotatated variable
	deconstruct * [aspire_attribute_prefix] a_declaration
		_ [aspire_attribute_prefix]
	deconstruct a_declaration
		_ [decl_specifiers] a_list [list init_declarator] _ [semi]
	construct length [number]
		_ [length a_list]
	where 
		length [> '1]
	construct _ [stringlit]
		_ [+ "ERROR: detected wrong pattern with annotated variables in the preprocessed source file "] [print]
	construct _ [stringlit]
		_ [+ "code: ["] [unparse a_declaration] [+ "]"] [print] [quit '1]
	by
		a_declaration
end rule




% 1. extract data annotations
% The algorthm first apply to global variables (i.e. outside funtion declarations) and then local function variables

function extract_data_annotations
	replace [program]
		p [program]
	by
		p [extract_data_annotations_in_declaration ""]
		  [extract_data_annotations_in_functions]
end function

rule extract_data_annotations_in_functions
	replace $ [function_definition]
		the_function [function_definition]
	deconstruct the_function
		header [function_header]
    	KR [opt KR_parameter_decls]
    	code [compound_statement]
    deconstruct header
    	_ [opt decl_specifiers] the_declarator [declarator] _ [function_declarator_extension] 
    deconstruct the_declarator
    	_ [repeat ptr_operator] the_base_declarator [base_declarator] _ [repeat declarator_extension] 
    deconstruct * [reference_id] the_base_declarator
		function_name [reference_id]
	construct function_string [stringlit]
		_ [unparse function_name]
	construct new_code [compound_statement]
		code [extract_data_annotations_in_declaration function_string]
	by
		header KR new_code
end rule


rule extract_data_annotations_in_declaration scope [stringlit]
	skipping [function_definition]
	replace $ [declaration]
		declSpecs [decl_specifiers] initDecls [list init_declarator]  semi [semi]
	construct newDeclSpecs [decl_specifiers]
		declSpecs [extract_and_remove_data_annotations1 scope]
	construct newInitDecls [list init_declarator]
		initDecls [extract_and_remove_data_annotations2 scope]
	by
		newDeclSpecs newInitDecls semi
end rule


% duplication required by a bug in the parsing.
% eaiser to duplicate the function that to post-process the AST to fix the bug.
rule extract_and_remove_data_annotations1 scope [stringlit]
	replace $ [repeat decl_qualifier_or_type_specifier]
		first [type_specifier] second [decl_qualifier]
	deconstruct * [reference_id] first
		RefId [reference_id] 
	deconstruct second
		_ [aspire_attribute_prefix] ProtectionStr [stringlit] suffix [aspire_attribute_suffix]
	deconstruct suffix
		') ')') file_name [srcfilename] line_number [srclinenumber]
	%construct file_name_s [stringlit]
	%	_ [unparse file_name]
	construct line_number_n [number]
		_ [parse line_number]
	construct variable_name [stringlit]
		_ [unparse RefId]
	construct annotation_fact [list json_pair]
		%"file_name" ': file_name_s ',
		"line_number" ': line_number_n ',
		"annotation_type" ': "data" ',
		%"function_name" ': scope ',
		"variable_name" ': variable_name ',
		"annotation_content" ': ProtectionStr
	construct complete_list [list json_pair]
		annotation_fact [add_scope_if_not_empy scope] [add_file_name file_name]
	construct fact_entry [json_value]
		'{ complete_list '}
	import ANNOTATIONS [list json_value]
	export ANNOTATIONS
		ANNOTATIONS [, fact_entry]
	by
		first %remove annotation
end rule

rule extract_and_remove_data_annotations2 scope [stringlit]
	replace $ [init_declarator]
		RPtrOp [repeat ptr_operator] RefId [reference_id] ArrayPart [repeat declarator_extension] AttrSpec [attribute_spec] OptInit [opt initialization]
	deconstruct AttrSpec
		_ [aspire_attribute_prefix] ProtectionStr [stringlit] suffix [aspire_attribute_suffix]
	deconstruct suffix
		') ')') file_name [srcfilename] line_number [srclinenumber]
	%construct file_name_s [stringlit]
	%	_ [unparse file_name]
	construct line_number_n [number]
		_ [parse line_number]
	construct variable_name [stringlit]
		_ [unparse RefId]
	%construct _ [stringlit]
	%	scope [print]
	%construct _ [reference_id]
	%	RefId [print]
	%construct _ [stringlit]
	%	ProtectionStr [print]
	%construct _ [srcfilename]
	%	file_name [print]
	%construct _ [srclinenumber]
	%	line_number [print]
	construct annotation_fact [list json_pair]
		%"file_name" ': file_name_s ',
		"line_number" ': line_number_n ',
		"annotation_type" ': "data" ',
		%"function_name" ': scope ',
		"variable_name" ': variable_name ',
		"annotation_content" ': ProtectionStr
	construct complete_list [list json_pair]
		annotation_fact [add_scope_if_not_empy scope] [add_file_name file_name]
	construct fact_entry [json_value]
		'{ complete_list '}
	import ANNOTATIONS [list json_value]
	export ANNOTATIONS
		ANNOTATIONS [, fact_entry]
	by
		% remove annotation when estracted
		RPtrOp RefId ArrayPart OptInit 
end rule


function add_scope_if_not_empy scope [stringlit]
	replace [list json_pair]
		old_list [list json_pair]
	deconstruct not scope
		""
	construct new_pair [json_pair]
		"function_name" ': scope 
	by
		old_list [, new_pair]
end function




% 2. Extract code annotations. The search is restricted to function bodies only

rule extract_code_annotations
	replace $ [function_definition]
		header [function_header]
    	kr [opt KR_parameter_decls]
    	code [compound_statement]
    deconstruct header
    	_ [opt decl_specifiers] the_declarator [declarator] _ [function_declarator_extension] 
    deconstruct the_declarator
    	_ [repeat ptr_operator] the_base_declarator [base_declarator] _ [repeat declarator_extension] 
    deconstruct * [reference_id] the_base_declarator
		function_name [reference_id]
	construct function_string [stringlit]
		_ [unparse function_name]
	construct new_code [compound_statement]
		code [extract_and_remove_code_annotations function_string]
	by
		header kr new_code
end rule


rule extract_and_remove_code_annotations scope [stringlit]
	replace $ [declaration_or_statement]
		a_block_annotation [aspire_block]
	deconstruct a_block_annotation
		annot_begin [aspire_pragma_begin]_ [attr aspire_block_status]
			code [repeat declaration_or_statement]
		annot_end [aspire_pragma_end]
	deconstruct annot_begin
		file_name [srcfilename] loc_being [srclinenumber]
		'_ASPIRE_BLOCK_BEGIN '( content [stringlit] ') 
	deconstruct annot_end
		_ [srcfilename] loc_end [srclinenumber] 
		'_ASPIRE_BLOCK_END '( ') 
	%construct file_name_s [stringlit]
	%	_ [unparse file_name]
	construct line_number_begin [number]
		_ [parse loc_being]
	construct line_number_end [number]
		_ [parse loc_end] 
	construct annotation_fact [list json_pair]
		%"file_name" ': file_name_s ',
		"line_number" ': '[ line_number_begin ', line_number_end  '],
		"annotation_type" ': "code" ',
		"function_name" ': scope ',
		"annotation_content" ': content
	construct new_entry [json_value]
		'{ annotation_fact [add_file_name file_name] '}
	import ANNOTATIONS [list json_value]
	export ANNOTATIONS
		ANNOTATIONS [, new_entry]
	construct new_code [compound_statement]
		'{ code '} %annotation removed
	by
		new_code
end rule







% 3. Check for missing annoations and replace program with result.
% Since annotations are removed before conrsion to Json, in case an annotation is not convernted
% (i.e., because not matched by the patterns) it remains in the code. This condition is detected are
% made evident with a runtime error.

function replace_program_with_result
	replace [program]
		p [program]
	by
		p [check_for_missed_data_annotation]
		  [check_for_missed_code_annotation]
		  [replace_result]
end function

rule check_for_missed_data_annotation
	replace $ [init_declarator]
		RPtrOp [repeat ptr_operator] RefId [reference_id] AttrSpec [attribute_spec] OptInit [opt initialization]
	deconstruct AttrSpec
		_ [aspire_attribute_prefix] ProtectionStr [stringlit] suffix [aspire_attribute_suffix]
	deconstruct suffix
		') ')') file_name [srcfilename] line_number [srclinenumber]
	construct _ [stringlit]
		_ [+ "EXECUTION ERROR: data annotation not trasformed (line "] [unparse line_number] [+ ")"] [print]
	construct _ [stringlit]
		_ [+ "variable: ["] [unparse RefId] [+ "]"] [print]
	construct _ [stringlit]
		_ [+ "annotation: ["] [unparse AttrSpec] [+ "]"] [print] [quit '1]
	by
		RPtrOp RefId AttrSpec OptInit
end rule

rule check_for_missed_code_annotation
	replace $ [declaration_or_statement]
		a_block_annotation [aspire_block]
	deconstruct a_block_annotation
		annot_begin [aspire_pragma_begin]_ [attr aspire_block_status]
			_ [repeat declaration_or_statement]
		annot_end [aspire_pragma_end]
	deconstruct annot_begin
		file_name [srcfilename] loc_being [srclinenumber]
		'_ASPIRE_BLOCK_BEGIN '( content [stringlit] ') 
	deconstruct annot_end
		_ [srcfilename] loc_end [srclinenumber]
		'_ASPIRE_BLOCK_END '( ') 
	construct _ [stringlit]
		_ [+ "EXECUTION ERROR: code annotation not trasformed (lines "] [unparse loc_being] 
		  [+ "-"] [unparse loc_end] [+ ")"] [print]
	construct _ [stringlit]
		_ [+ "annotation: ["] [unparse content] [+ "]"] [print] [quit '1]
	by
		a_block_annotation
end rule



function replace_result
	replace [program]
		p [compilation_unit]
	% TODO: check missing annotations
	import ANNOTATIONS [list json_value]
	construct result [json_array]
		'[ ANNOTATIONS ']
	by
		result
end function


% Common util function

function add_file_name file_name [srcfilename]
	replace [list json_pair]
		old_list [list json_pair]
	construct new_string [stringlit]
		_ [file_name_global] [file_name_non_global file_name]
	construct new_pair [json_pair]
		"file_name" ': new_string
	by
		new_pair ', old_list
end function

function file_name_global
	replace [stringlit]
		_ [stringlit]
	import TXLargs [repeat stringlit]
	deconstruct * [repeat stringlit] TXLargs
		"-file_name" name [stringlit] _ [repeat stringlit]
	by
		name
end function

function file_name_non_global file_name [srcfilename]
	replace [stringlit]
		""
	by
		_ [unparse file_name]		
end function

% Sort entries in the json file

function sort
	replace [program]
		'[ result [list json_value] ']
	construct sorted_result [list json_value]
		result [add_data_line_number] [add_code_line_number] [sort_json_values]
	by
		'[ sorted_result ']
end function


%'{  [IN] [list json_pair] [EX] [NL]'} [attr number] [NL]

rule add_data_line_number
	replace $ [json_object]
		'{  content [list json_pair] '} 
	deconstruct * [json_pair] content
		"line_number" ': line [number]
	by
		'{  content '}  line
end rule

rule add_code_line_number
	replace $ [json_object]
		'{  content [list json_pair] '} 
	deconstruct * [json_pair] content
		"line_number" ': '[ line [number] ', _ [number] ']
	by
		'{  content '}  line
end rule


rule sort_json_values
	replace [list json_value]
		first [json_object] ', second [json_object] ', rest [list json_value]
	deconstruct first
		'{ _ [list json_pair] '} first_line [number]
	deconstruct second
		'{ _ [list json_pair] '} second_line [number]
	where
		second_line [< first_line]
	by
		second ', first ', rest
end rule




