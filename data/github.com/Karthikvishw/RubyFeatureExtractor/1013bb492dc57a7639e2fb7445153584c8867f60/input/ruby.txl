include "ruby.grm"

function main
	match [program] 
	RubyProgram [program]
	
construct varstatements[statements*]
_ [^ RubyProgram]
construct _ [number]
_ [length varstatements] [putp"statements: %"]

construct varundef_definition[undef_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varundef_definition] [putp"undef_definition: %"]


construct varalias_definition[alias_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varalias_definition] [putp"alias_definition: %"]


construct varrequire_definition[require_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varrequire_definition] [putp"require_definition: %"]


construct varBEGIN_statement[BEGIN_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varBEGIN_statement] [putp"BEGIN_statement: %"]


construct varEND_statement[END_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varEND_statement] [putp"END_statement: %"]


construct varexpression[expression*]
_ [^ RubyProgram]
construct _ [number]
_ [length varexpression] [putp"Expression: %"]


construct varreturn_expression[return_expression*]
_ [^ RubyProgram]
construct _ [number]
_ [length varreturn_expression] [putp"return_expression: %"]


construct varyield_expression[yield_expression*]
_ [^ RubyProgram]
construct _ [number]
_ [length varyield_expression] [putp"yield_expression: %"]


construct variterator[iterator*]
_ [^ RubyProgram]
construct _ [number]
_ [length variterator] [putp"iterator: %"]


construct varblock_var[block_var*]
_ [^ RubyProgram]
construct _ [number]
_ [length varblock_var] [putp"block_var: %"]


construct varblock_vars[block_vars*]
_ [^ RubyProgram]
construct _ [number]
_ [length varblock_vars] [putp"block_vars: %"]


construct varstatement_modifier[statement_modifier*]
_ [^ RubyProgram]
construct _ [number]
_ [length varstatement_modifier] [putp"statement_modifier: %"]


construct varmodifier[modifier*]
_ [^ RubyProgram]
construct _ [number]
_ [length varmodifier] [putp"modifier: %"]


construct varrescue_statement[rescue_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varrescue_statement] [putp"rescue_statement: %"]


construct varensure_statement[ensure_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varensure_statement] [putp"ensure_statement: %"]


construct varreturn_statement[return_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varreturn_statement] [putp"return_statement: %"]


construct varprimary[primary*]
_ [^ RubyProgram]
construct _ [number]
_ [length varprimary] [putp"primary: %"]


construct varreference[reference*]
_ [^ RubyProgram]
construct _ [number]
_ [length varreference] [putp"reference: %"]


construct varyield_statement[yield_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varyield_statement] [putp"yield_statement: %"]


construct varif_statement[if_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varif_statement] [putp"if_statement: %"]


construct varunless_statement[unless_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varunless_statement] [putp"unless_statement: %"]


construct varwhile_statement[while_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varwhile_statement] [putp"while_statement: %"]


construct varuntil_statement[until_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varuntil_statement] [putp"until_statement: %"]


construct varcase_statement[case_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varcase_statement] [putp"case_statement: %"]


construct varfor_statement[for_statement*]
_ [^ RubyProgram]
construct _ [number]
_ [length varfor_statement] [putp"for_statement: %"]


construct varclass_definition[class_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varclass_definition] [putp"class_definition: %"]


construct varmodule_definition[module_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varmodule_definition] [putp"module_definition: %"]


construct varmethod_definition[method_definition*]
_ [^ RubyProgram]
construct _ [number]
_ [length varmethod_definition] [putp"method_definition: %"]


construct varcommand[command*]
_ [^ RubyProgram]
construct _ [number]
_ [length varcommand] [putp"command: %"]


construct varclass_inheritance[class_inheritance*]
_ [^ RubyProgram]
construct _ [number]
_ [length varclass_inheritance] [putp"class_inheritance: %"]


construct varliteral[literal*]
_ [^ RubyProgram]
construct _ [number]
_ [length varliteral] [putp"literal: %"]


construct varstring_literal[string_literal*]
_ [^ RubyProgram]
construct _ [number]
_ [length varstring_literal] [putp"string_literal: %"]


construct varnumeric_literal[numeric_literal*]
_ [^ RubyProgram]
construct _ [number]
_ [length varnumeric_literal] [putp"numeric_literal: %"]


construct varsymbol_literal[symbol_literal*]
_ [^ RubyProgram]
construct _ [number]
_ [length varsymbol_literal] [putp"symbol_literal: %"]


construct vararray_literal[array_literal*]
_ [^ RubyProgram]
construct _ [number]
_ [length vararray_literal] [putp"array_literal: %"]


construct varregular_expression[regular_expression*]
_ [^ RubyProgram]
construct _ [number]
_ [length varregular_expression] [putp"regular_expression: %"]


construct varcommand_output[command_output*]
_ [^ RubyProgram]
construct _ [number]
_ [length varcommand_output] [putp"command_output: %"]


construct varhere_document[here_document*]
_ [^ RubyProgram]
construct _ [number]
_ [length varhere_document] [putp"here_document: %"]


construct varvariable[variable*]
_ [^ RubyProgram]
construct _ [number]
_ [length varvariable] [putp"variable: %"]


construct varidentifier[identifier*]
_ [^ RubyProgram]
construct _ [number]
_ [length varidentifier] [putp"identifier: %"]


construct varvarname[varname*]
_ [^ RubyProgram]
construct _ [number]
_ [length varvarname] [putp"varname: %"]


construct varglobal_id[global_id*]
_ [^ RubyProgram]
construct _ [number]
_ [length varglobal_id] [putp"global_id: %"]


construct varinstance_id[instance_id*]
_ [^ RubyProgram]
construct _ [number]
_ [length varinstance_id] [putp"instance_id: %"]


construct varoperation[operation*]
_ [^ RubyProgram]
construct _ [number]
_ [length varoperation] [putp"operation: %"]


construct varfunction_call[function_call*]
_ [^ RubyProgram]
construct _ [number]
_ [length varfunction_call] [putp"function_call: %"]


construct varparen_call_args[paren_call_args*]
_ [^ RubyProgram]
construct _ [number]
_ [length varparen_call_args] [putp"paren_call_args: %"]


construct varcall_args[call_args*]
_ [^ RubyProgram]
construct _ [number]
_ [length varcall_args] [putp"call_args: %"]


construct varbinary_op[binary_op*]
_ [^ RubyProgram]
construct _ [number]
_ [length varbinary_op] [putp"binary_op: %"]


construct varunary_op[unary_op*]
_ [^ RubyProgram]
construct _ [number]
_ [length varunary_op] [putp"unary_op: %"]

 	construct identifierVar [identifier*]
	_ [^ RubyProgram]

    
    construct IncludeStmt [identifier*]   
    _ [getIncludeStmt  each identifierVar]
    construct _ [number]
    _ [length IncludeStmt] [putp "Include statement: %"]
    
    construct RespondToStmt [identifier*]   
    _ [getRespondToStmt  each identifierVar]
    construct _ [number]
    _ [length RespondToStmt] [putp "Respond To statement: %"]
    
    construct MethodMissingStmt [identifier*]   
    _ [getMethodMissingStmt  each identifierVar]
    construct _ [number]
    _ [length MethodMissingStmt] [putp "Method Missing statement: %"]

    construct DefineMethodStmt [identifier*]   
    _ [getDefineMethodStmt  each identifierVar]
    construct _ [number]
    _ [length DefineMethodStmt] [putp "Define Method statement: %"]
    
    construct AttrReaderStmt [identifier*]   
    _ [getAttrReaderStmt  each identifierVar]
    construct _ [number]
    _ [length AttrReaderStmt] [putp "Attr Reader statement: %"]
       
    construct AttrWriterStmt [identifier*]   
    _ [getAttrWriterStmt  each identifierVar]
    construct _ [number]
    _ [length AttrWriterStmt] [putp "Attr Writer statement: %"]    

    construct AttrAccessorStmt [identifier*]   
    _ [getAttrAccessorStmt  each identifierVar]
    construct _ [number]
    _ [length AttrAccessorStmt] [putp "Attr Accessor statement: %"]      
     
	construct InstanceVariableGetStmt [identifier*]   
    _ [getInstanceVariableGetStmt  each identifierVar]
    construct _ [number]
    _ [length InstanceVariableGetStmt] [putp "Instance Variable Get statement: %"]  
    
    	construct InstanceVariableSetStmt [identifier*]   
    _ [getInstanceVariableSetStmt  each identifierVar]
    construct _ [number]
    _ [length InstanceVariableSetStmt] [putp "Instance Variable Set statement: %"] 
    
	construct ClassVariableGetStmt [identifier*]   
    _ [getClassVariableGetStmt  each identifierVar]
    construct _ [number]
    _ [length ClassVariableGetStmt] [putp "Class Variable Get statement: %"]  
    
    	construct ClassVariableSetStmt [identifier*]   
    _ [getClassVariableSetStmt  each identifierVar]
    construct _ [number]
    _ [length ClassVariableSetStmt] [putp "Class Variable Set statement: %"] 
    
	construct ConstGetStmt [identifier*]   
    _ [getConstGetStmt  each identifierVar]
    construct _ [number]
    _ [length ConstGetStmt] [putp "Const Get statement: %"]  
    
    	construct ConstSetStmt [identifier*]   
    _ [getConstSetStmt   each identifierVar]
    construct _ [number]
    _ [length ConstSetStmt] [putp "Const Set statement: %"]     

 	construct SubscriptOrDotVar [function_call*]
	_ [^ RubyProgram]

	construct FunctionEvalCalls [function_call*]   
    _ [getEvalCalls  each SubscriptOrDotVar]
    construct _ [number]
    _ [length FunctionEvalCalls] [putp "Eval statement: %"]

	construct FunctionClassEvalCalls [identifier*]   
    _ [getClassEvalCalls  each identifierVar]
    construct _ [number]
    _ [length FunctionClassEvalCalls] [putp "Class Eval statement: %"]

	construct FunctionModuleEvalCalls [identifier*]   
    _ [getModuleEvalCalls  each identifierVar]
    construct _ [number]
    _ [length FunctionModuleEvalCalls] [putp "Module Eval statement: %"]     

	construct FunctionInstanceEvalCalls [identifier*]   
    _ [getInstanceEvalCalls  each identifierVar]
    construct _ [number]
    _ [length FunctionInstanceEvalCalls] [putp "Instance Eval statement: %"]   

	construct FunctionInstanceExecCalls [identifier*]   
    _ [getInstanceExecCalls  each identifierVar]
    construct _ [number]
    _ [length FunctionInstanceExecCalls] [putp "Instance Exec statement: %"]  
    
	construct FunctionSendCalls [identifier*]   
    _ [getSendCalls  each identifierVar]
    construct _ [number]
    _ [length FunctionSendCalls] [putp "Send statement: %"]  
    
end function

function getIncludeStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'include
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getRespondToStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'respond_to
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getMethodMissingStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'method_missing
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getDefineMethodStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'define_method
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getAttrReaderStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'attr_reader
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getAttrWriterStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'attr_writer
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getAttrAccessorStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'attr_accessor
	by
		PreviousInternal[.CurrentInternalFunc]		
end function
 
function getInstanceVariableGetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'instance_variable_get
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getInstanceVariableSetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'instance_variable_set
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getClassVariableGetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'class_variable_get
	by
		PreviousInternal[.CurrentInternalFunc]		
end function
 
function getClassVariableSetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'class_variable_set
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getConstGetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'const_get
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getConstSetStmt CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'const_set
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getEvalCalls CurrentInternalFunc[function_call]
	replace [function_call*]
		PreviousInternal[function_call*]	
		deconstruct CurrentInternalFunc
		'eval'( VarCall [call_args?] VARter[terminator?] ')
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getClassEvalCalls CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'class_eval
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getModuleEvalCalls CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'module_eval
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getInstanceEvalCalls CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'instance_eval 
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getInstanceExecCalls CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'instance_exec  
	by
		PreviousInternal[.CurrentInternalFunc]		
end function

function getSendCalls CurrentInternalFunc[identifier]
	replace [identifier*]
		PreviousInternal[identifier*]	
		deconstruct CurrentInternalFunc
		'send 
	by
		PreviousInternal[.CurrentInternalFunc]		
end function