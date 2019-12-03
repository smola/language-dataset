include "boo.grm"
include "csharpAddon.grm"

function main
	replace [program]
		P [program]
	by
		P [convert_class_file] [convert_local_function_definition] [convert_variable_declaration] [convert_variable_o_declaration]
		[convert_function_declaration] [c_local_variable_definition] [convert_generic]
		[convert_for_in] [convert_for_in_typed] [convert_if] [convert_reverse_if] [convert_elif] [convert_if_body] [rename_single] [convert_import_stmt]
		[convert_while] [c_generic_type_declaration] [convert_callable] [convert_array_type_1] [convert_array_type_2] [convert_enum_definition]
		[convert_yield] [convert_yield_null] [convert_lambda_literal] [rename_type_callable] [rename_self]
		[rename_and] [rename_or] [rename_not]
		[convert_default_function_declaration] % comment this to forbidden
		[convert_constructor_name_class] [convert_constructor_name_struct] [convert_derive]
		[add_semicolon_stmt] [add_semicolon_member_variable] [convert_indent] [convert_comment]
end function

rule convert_class_file
	replace [class_file]
		NS [namespace_stmt] CM [opt comment] NL [newline] C [class_file_content]
	by
		NS '{ CM NL C '}
end rule

rule convert_constructor_name_class
	replace $ [class_definition]
		C [class_definition]
	deconstruct C
	    A [repeat attribute_stmt_newline] S [opt scope_modifier] 'class N [id] G [opt generic_type_declaration] D [opt derive] Body [class_body]
	by
		A S 'class N G D Body [convert_constructor_name_struct] [convert_constructor_name_class] [replace_constructor N]
end rule

rule convert_constructor_name_struct
	replace $ [struct_definition]
		C [struct_definition]
	deconstruct C
	    S [opt scope_modifier] 'struct N [id] Body [class_body]
	by
		S 'struct N Body [convert_constructor_name_struct] [convert_constructor_name_class] [replace_constructor N]
end rule


rule replace_constructor Name [id]
	replace [constructor_header]
	    'def 'constructor '( P [variable_declaration,] ')
	by
		Name '( P ')
end rule

rule convert_derive
	replace [derive]
	    '( T [type_base] ')
	by
		': T
end rule

rule convert_import_stmt
	replace [import_stmt_newline]
		'import N[id_with_dots] A [opt import_alias] S [opt ';] E [repeat endofline]
	by
		'using N A '; E
end rule

rule convert_enum_definition
	replace [enum_item]
		N [id] _ [endofline+]
	by
		N ',
end rule

rule convert_variable_declaration
	replace [variable_declaration]
		N [id] as T [type]
	by
		T N
end rule

rule convert_variable_o_declaration
	replace [variable_optional_declaration]
		N [id] as T [type]
	by
		T N
end rule

rule convert_function_declaration
	replace [function_header]
	    'def N [id] G [opt generic_type_declaration] '( P [variable_declaration,] ') T [opt type_declaration]
	deconstruct T
		'as _T [type]
	by
		_T N G '( P ')
end rule

rule convert_default_function_declaration
	replace [function_header]
	    'def N [id] G [opt generic_type_declaration] '( P [variable_declaration,] ')
	by
		'void N G '( P ')
end rule

rule convert_array_type_1
	replace [array_type]
		'( T [type] ')
	by
		T '[']
end rule

rule convert_array_type_2
	replace [array_type]
		'( T [type], '2 ')
	by
		T '[',']
end rule

rule convert_lambda_literal
	replace [lambda_function_literal]
		'{ P [variable_optional_declaration,] '| E [expression] '}
	by
		P '=> E
end rule

rule convert_for_in
	replace [for_in_stmt]
	    'for V [id] 'in E [expression] I [indent] B [repeat stmt_newline] D [dedent] TAIL[repeat endofline]
	by
	   	'foreach '( 'var V 'in E ') I B D TAIL   	
end rule

rule convert_for_in_typed
	replace [for_in_stmt]
	    'for V [id] 'as T [type] 'in E [expression] I [indent] B [repeat stmt_newline] D [dedent] TAIL[repeat endofline]
	by
	   	'foreach '( T V 'in E ') I B D TAIL   	
end rule

rule convert_if
	replace [if_header]
		'if E [expression]
	deconstruct not E
		'( _E [expression] ')
	by
		'if '( E ')
end rule

rule convert_reverse_if
	replace [if_stmt]
		S [single_stmt] H [if_header] _ [opt ';] _ [repeat endofline+]
	by
		H S ';
end rule

rule convert_elif
	replace [elif_header]
		'elif E [expression]
	deconstruct not E
		'( _ [expression] ')
	by
		'else 'if '( E ')
end rule

rule convert_if_body
	replace [if_body]
		': S [single_stmt]
	by
		S ';
end rule

rule convert_while
	replace [while_header]
		'while E [expression]
	deconstruct not E
		'( _ [expression] ')
	by
		while '( E')
end rule

rule convert_callable
	replace [callable_declaration]
		M [opt modifier] 'callable N [id] G [opt generic_type] '( P [variable_declaration,] ') as T [type]
	by
		M 'delegate T N G '( P ')
end rule

rule rename_single
	replace [id]
		'single
	by
		'float
end rule

rule rename_type_callable
	replace [type]
		'callable
	by
		'Action
end rule

rule rename_self
	replace [objvalue]
		'self
	by
		'this
end rule

rule rename_and
	replace [dual_op]
		'and
	by
		'&&
end rule

rule rename_or
	replace [dual_op]
		'or
	by
		'||
end rule

rule rename_not
	replace [una_op]
		'not
	by
		'!
end rule

rule c_generic_type_declaration
	replace [generic_type_declaration]
		'[ 'of N [id] D [opt constraint] ']
	by
		'< N '> D [replace_constraint N] 
end rule

rule convert_local_function_definition
	replace [local_function_definition]
		'def Name [id] '( P [variable_declaration,] ') _ [opt type_declaration] I [indent] B [function_body] D [dedent] NL [repeat endofline]
	by
		'var Name '= 'delegate '( P ') I B D NL
end rule

rule replace_constraint N [id]
	replace [constraint]
	    '( T [type_base] ')
	by
		'where N ': T
end rule

rule convert_generic
	replace [generic_type]
		'[ 'of T[type,] ']
	by
		'< T '>
end rule

rule c_local_variable_definition
	replace [local_variable_definition]
		N [id] 'as T [type] I [opt variable_initialization]
	by
		T N I
end rule

rule convert_yield
	replace [yield_stmt]
		'yield E [expression]
	by
		'yield 'return E
end rule

rule convert_yield_null
	replace [yield_stmt]
		'yield
	by
		'yield 'return null
end rule

rule add_semicolon_stmt
	replace [stmt_newline]
		S [single_stmt] D [delimiter] NL [repeat endofline]
	deconstruct D
		L [endofline]
	by
		S '; L NL
end rule

rule add_semicolon_member_variable
	replace [class_member_variable_declaration]
		M [modifier] D [variable_declaration] I [opt variable_initialization]
	by
		M D I ';
end rule

rule convert_indent
	replace [indent]
		': C [opt comment] _ [newline] '{ NL [repeat endofline]
	by
		'{ C NL
end rule

rule convert_comment
	replace [comment]
		M [comment]
	construct F [comment]
		M [: 1 1]
	construct _L [number]
		0
	construct L [number]
		_L [# M]
	construct Rest [comment]
		M [: 2 L]
	construct T [comment]
		'//
	where
		F [= "#"]
	by
		T [+ Rest]
end rule

