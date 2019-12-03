% add_records_to_declaration: ADDS RECORDING TO PRE AND POST STATEMENTS FOR A DECLARATION

function add_records_to_declaration
    replace [declaration_or_statement]
        Point [id] LocalVariableDeclaration [local_variable_declaration]
    import AnyStatement [declaration_or_statement]
    construct PushAccount [declaration_or_statement]
        AnyStatement [create_push_account]
    construct PopAccount [declaration_or_statement]
        AnyStatement [create_pop_account]
    construct Occurrence [declaration_or_statement]
        AnyStatement [create_record_process_statement Point]
    import PostAdditions [repeat declaration_or_statement]
    export PostAdditions
        PostAdditions [. PopAccount][. Occurrence]
    construct ForExportOfRecords1 [declaration_or_statement]
        LocalVariableDeclaration [add_argument_passes]
    deconstruct LocalVariableDeclaration
        VariableDeclaration [variable_declaration]
    deconstruct VariableDeclaration
        _ [repeat modifier] _ [type_specifier] VariableDeclarators [variable_declarators] ';
    deconstruct VariableDeclarators
        VariableDeclaratorList [list variable_declarator+]
    construct ForExportOfRecords2 [declaration_or_statement]
        AnyStatement [add_records_to_declarator Point each VariableDeclaratorList]
    import PreAdditions [repeat declaration_or_statement]
    export PreAdditions
        PreAdditions [. PushAccount]
    by
        LocalVariableDeclaration
end function

% add_records_to_declarator: PUTS RECORDING OF DECLARATOR DEFINITION AS CAUSE AND DEPENENCIES AS EFFECT INTO POST-STATEMENTS

function add_records_to_declarator Point [id] VariableDeclarator [variable_declarator]
    replace [declaration_or_statement]
        DeclarationOrStatement [declaration_or_statement]
    deconstruct VariableDeclarator
        VariableName [variable_name] Initializer [equals_variable_initializer]
    deconstruct Initializer
        '= VariableInitializer [variable_initializer]
    deconstruct VariableInitializer
        Expression [expression]
    deconstruct VariableName
        Identifier [id] DeclaredName [declared_name] Dimensions [repeat dimension]
    deconstruct DeclaredName
        Name [id] _ [opt generic_parameter]
    construct VariableStateStatement [declaration_or_statement]
        DeclarationOrStatement [create_record_data_statement Identifier Point Name]
    construct GeneratedStatement [declaration_or_statement]
        DeclarationOrStatement [create_generated_relationship Identifier "Assigned Value In" Point]
    import PostAdditions [repeat declaration_or_statement]
    export PostAdditions
        PostAdditions [. VariableStateStatement][. GeneratedStatement]
    construct NewExpression [expression]
        Expression [add_records_for_dependences DeclarationOrStatement Point]
    by
        DeclarationOrStatement
end function
