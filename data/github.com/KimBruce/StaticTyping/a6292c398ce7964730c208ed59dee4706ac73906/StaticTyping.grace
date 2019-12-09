dialect "none"
import "standardGrace" as sg
import "ast" as ast
import "lexer" as lex
import "parser" as parser
import "xmodule" as xmodule
import "io" as io
import "sys" as sys
import "SharedTypes" as share
import "ScopeModule" as sc
import "ObjectTypeModule" as ot

inherit sg.methods

// Give imported types shorter names
// Types of AST nodes
type MethodType = share.MethodType
type MethodTypeFactory = share.MethodTypeFactory
type GenericType = share.GenericType
type GenericTypeFactory = share.GenericTypeFactory
type ObjectType = share.ObjectType
type ObjectTypeFactory = share.ObjectTypeFactory
type AstNode = share.AstNode
type MixPart = share.MixPart
type Param = share.Param
type Parameter = share.Parameter
type ParamFactory = share.ParamFactory
//This type is used for checking subtyping
type TypePair = share.TypePair

// Error resulting from type checking
def StaticTypingError: ExceptionKind is public = share.StaticTypingError

// imported constants relating to cacheing type info
def cache: Dictionary = sc.cache
def allCache: Dictionary = sc.allCache

def aMethodType : MethodTypeFactory = ot.aMethodType
def aGenericType : GenericTypeFactory = ot.aGenericType
def anObjectType : ObjectTypeFactory = ot.anObjectType
def scope: sc.Scope = sc.scope
def aParam: ParamFactory = ot.aParam
// TODO Remove eventually
def preludeTypes: Set[[String]] = share.preludeTypes

// debugging prints will print if debug is true
def debug: Boolean = false 

// return the return type of the block (as declared)
method objectTypeFromBlock(block: AstNode) → ObjectType 
                            is confidential {
    def bType = typeOf(block)
    
    if (debug) then {
        io.error.write "\n48: bType of block is {bType}"
    } 
    
    if(bType.isDynamic) then { return anObjectType.dynamic }

    def numParams: Number = block.params.size
    def applyName: String = if (numParams == 0) then {
        "apply"
    } else {
        "apply({numParams})"
    }
    def apply: MethodType = bType.getMethod(applyName)

    match(apply) case { (ot.noSuchMethod) →
        def strip = {x → x.nameString}
        StaticTypingError.raise ("1000: the expression " ++
            "`{share.stripNewLines(block.toGrace(0))}` " ++ 
            "of type '{bType}' on line {block.line} does "++
            "not satisfy the type 'Block'")with(block)
    } case { meth : MethodType →
        if (debug) then {
            io.error.write ("\n66: look up method to get {meth} returning {meth.retType}")
        }   
        return meth.retType
    }
}

// Return the return type of the block as obtained by type-checking
// the last expression in the block
method objectTypeFromBlockBody(body: Sequence⟦AstNode⟧) → ObjectType
                                  is confidential {
    if(body.size == 0) then {
        anObjectType.doneType
    } else {
        typeOf(body.last)
    }
}

// check the type of node and insert into cache associated with the node
method checkTypes (node: AstNode) → Done is confidential{
    def debug3: Boolean = false
    if (debug3) then {
        io.error.write "\n233: checking types of {node.nameString}"
    }
    cache.at (node) ifAbsent {
        if (debug3) then {
           io.error.write "\n235: {node.nameString} not in cache"
        }
        node.accept (astVisitor)
    }
}

// check type of node, put in cache & then return type
method typeOf (node: AstNode) → ObjectType {
    checkTypes (node)
    cache.at (node) ifAbsent {
        StaticTypingError.raise(
            "cannot type non-expression {node} on line " ++
            "{node.line}") with (node)
    }
}

// retrieve from cache the inheritable type of an object
method inheritableTypeOf (node: AstNode) → ObjectType 
                                        is confidential {
    allCache.at (node) ifAbsent {
        StaticTypingError.raise("cannot find confidential type of " ++
                        "{node} on line {node.line}") with (node)
    }
}

// Exceptions while type-checking. (Currently not used)DialectErr
//def ObjectError: outer.ExceptionKind = 
//      TypeError.refine("ObjectError")

// Class declaration error. (Currently not used)
//def ClassError: outer.ExceptionKind = 
//      TypeError.refine("Class TypeError")

// Declaration of method does not correspond to actual type
// def MethodError = TypeError.refine("Method TypeError")

// Def and var declarations.  Type of def or var declaration does not
// correspond to value associated with it
//def DialectError: outer.ExceptionKind = TypeError.refine("Def TypeError")

// Scoping error declaration with imports
//def ScopingError: outer.ExceptionKind = TypeError.refine("ScopingError")

// type of part of method request (actual call, not declaration)
type RequestPart = {
   args → List⟦AstNode⟧
   args:=(a: List⟦AstNode⟧) → Done
}

// Check if the signature and parameters of a request match
// the declaration, return the type of the result
method check (req : share.Request) against(meth' : MethodType)
                                → ObjectType is confidential {
    def debug3: Boolean = false
    if (debug3) then {
      io.error.write(
          "\n134 checking {req} of kind {req.kind} against {meth'}")
    }
    var meth : MethodType := meth'

    // instantiate generics if necessary
    if ((req.kind == "call") || (req.kind == "member")
                            || (req.kind == "identifier")) then {
        if ((false ≠ req.generics) && (meth.hasTypeParams)) then {
            meth := meth.apply(req.generics)
        }
    }

    def name: String = meth.nameString

    for(meth.signature) and (req.parts) do 
                {sigPart: MixPart, reqPart: RequestPart →
        def params: List⟦Param⟧ = sigPart.parameters
        def args: Collection⟦AstNode⟧   = reqPart.args

        checkParamArgsLengthSame (req, sigPart, params, args)
        for (params) and (args) do { param: Param, arg: AstNode →
            def pType: ObjectType = param.typeAnnotation
            def aType: ObjectType = typeOf(arg)
            if (debug) then {
                io.error.write (
                    "\n171 Checking {arg} of type {aType} is " ++
                    "subtype of {pType} while checking {req} " ++
                    "against {meth}")
                io.error.write("\n172 aType: {aType}, pType: {pType}")
            }

            // Make sure types of args are subtypes of parameter types
            if (aType.isConsistentSubtypeOf (pType).not) then {
                StaticTypingError.raise("the expression " ++
                    "`{share.stripNewLines(arg.toGrace(0))}` of type "++
                    "'{aType}' on  line {args.at(1).line} does " ++
                    "not satisfy the type of parameter '{param}' " ++
                    "in the method '{name}'") with(arg)
            }
        }
    }
    meth.retType
}

// If number of parameters and args differ, 
// throw a StaticTypingError exception
method checkParamArgsLengthSame(req, sigPart, params, args) -> Done
                            is confidential {
        def pSize: Number = params.size
        def aSize: Number = args.size

        if(aSize != pSize) then {
            def which: String = if (aSize > pSize) then {
                "many" 
            } else { 
                "few" 
            }
            def whereError: Number = if (aSize > pSize) then {
                args.at (pSize + 1)
            } else {
            // Can we get beyond the final argument?
                req.value
            }

            StaticTypingError.raise(
                "too {which} arguments to method part " ++
                "'{sigPart.name}' on line {req.line}, " ++
                "expected {pSize} but got {aSize}") with (whereError)
        }
}

//method DialectError(message: String) with (node) → Done {
//    io.error.write(message)
//    sys.exit(2)
//}

// Check the type of node to make sure it matches eType.
// Throw error only if type of node is not consistent subtype of eType
method check (node: AstNode) matches (eType : ObjectType)
        inMethod (name : String) → Done is confidential {
    def aType: ObjectType = typeOf(node)
    if (aType.isConsistentSubtypeOf (eType).not) then {
        StaticTypingError.raise("the method '{name}' on line {node.line} "++
            "declares a result of type '{eType}', but returns an " ++
            "expression of type '{aType}'") with (node)
    }
}

// break up input string into list of strings as divided by separator
method split (input : String, separator : String) → List⟦String⟧
                                is confidential {
    var start: Number := 1
    var end: Number := 1
    var output: List⟦ List⟦String⟧ ⟧ := list[]
    while {end < input.size} do {
        if (input.at(end) == separator) then {
            var cand := input.substringFrom(start)to(end-1)
            if (cand.size > 0) then {
                output.push(cand)
            }
            start := end + 1
        }
        end := end + 1
    }
    output.push(input.substringFrom(start)to(end))
    return output
}

// Pair of public and confidential types of an expression
// Generating objects
type PublicConfidential = {
    publicType → ObjectType
    inheritableType → ObjectType | false
}

// Returns pair of public and confidential type of expression that 
// can be inherited from
class pubConf (pType: ObjectType, cType: ObjectType ) → 
                            PublicConfidential is confidential {
    method publicType → ObjectType {pType}
    method inheritableType → ObjectType {cType}
    method asString → String {
        "confidential type is {cType}\npublic type is {pType}"
    }
}

// ******************************VISITOR CODE**************************
// Static type checker visitor
// methods return false if goes no further recursively
def astVisitor: ast.AstVisitor is public = object {
    inherit ast.baseVisitor

    // Default behavior serving as placeholder only 
    // for cases not yet implemented
    method checkMatch(node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "1436: checkMatch in astVisitor"
        }
        true
    }

    // type-check if statement
    method visitIf (ifnode: share.If) → Boolean {
        def cond: AstNode = ifnode.value
        // make sure condition is compatible with Boolean
        if (typeOf (cond).isConsistentSubtypeOf 
                            (anObjectType.boolean).not) then {
            StaticTypingError.raise ("1366: the expression "++
                "`{share.stripNewLines (cond.toGrace (0))}` on " ++
                "line {cond.line} does not satisfy the type "++
                "'Boolean' for an 'if' condition'") with (cond)
        }

        def thenType: ObjectType = objectTypeFromBlock (
                                        ifnode.thenblock)

        def hasElse: Boolean = ifnode.elseblock.body.size > 0
        def elseType: ObjectType = if (hasElse) then {
            objectTypeFromBlock(ifnode.elseblock)
        } else {  // if no else clause then type must be Done
            anObjectType.doneType
        }

        // type of expression is whichever branch has largest type.
        // If incompatible return variant formed by the two types
        def ifType: ObjectType = if (hasElse) then {
            if (thenType.isConsistentSubtypeOf (elseType)) then {
                elseType
            } elseif {elseType.isConsistentSubtypeOf(thenType)} then {
                thenType
            } else {
                thenType | elseType
            }
        } else {
            anObjectType.doneType
        }

        // save type in cache
        cache.at (ifnode) put (ifType)
        false
    }

    // Type check block.  Fails if don't give types to 
    // block parameters
    method visitBlock (block: AstNode) → Boolean {
        // Raise exception if block parameters not given types
        for (block.params) do {p→
            if (((p.kind == "identifier") || {p.wildcard.not})
                            && {p.decType.value == "Unknown"}) then {
                StaticTypingError.raise("no type given to declaration of"++
                    " parameter '{p.value}' on line {p.line}") with (p)
            }
        }

        def body = sequence(block.body)
        // return type of block (computed)
        var retType: ObjectType

        // Type check body of block in new scope with parameters
        scope.enter {
            // add parameters & their types to new scope
            for(block.params) do { param →
                if (("string" ≠ param.dtype.kind)
                            && {"num" ≠ param.dtype.kind}) then {
                    if (debug) then {
                        io.error.write(
                            "\n1517: {param.value} has {param.dtype}")
                    }
                    scope.variables.at(param.value)
                        put (anObjectType.fromDType (param.dtype)
                                                with (emptyList))
                }
            }

            // check type of all statements in block
            for(body) do { stmt: AstNode →
                checkTypes(stmt)
            }

            retType := objectTypeFromBlockBody(body)
        }
        // At this point, know block type checks.

        // Now compute type of block and put in cache
        def parameters = list[]
        for(block.params) do { param: AstNode →
            if (param.dtype.kind == "string") then {
                parameters.push (aParam.withName (param.value)
                                    ofType (anObjectType.string))
            } elseif {param.dtype.kind == "num"} then {
                parameters.push (aParam.withName (param.value)
                                    ofType (anObjectType.number))
            } else {
                def newType: ObjectType = 
                    anObjectType.fromDType (param.dtype) with (emptyList)
                if (debug) then {
                    io.error.write "\n355: newType is {newType}"
                }
                parameters.push(aParam.withName (param.value) 
                                            ofType(newType))
            }
        }
        // The type of the block
        def blockType: ObjectType = 
            anObjectType.blockTaking(parameters) returning (retType)

        cache.at (block) put (blockType)
        if (debug) then {
            io.error.write "block has type {blockType}"
        }
        false
    }

    //type checks match-case statements. Makes sure that the types 
    //of the matchee and the params match, and puts the return type
    //of the match-case in the cache.
    method visitMatchCase (node: share.MatchCase) → Boolean {
        def debug3: Boolean = false
        // expression being matched and its type
        def matchee = node.value
        var matcheeType: ObjectType := typeOf(matchee)
        //Note: currently only one matchee is supported

        // Keep track of parameter types and return types in case
        def paramTypesList: List⟦ObjectType⟧ = 
                                    emptyList⟦ObjectType⟧
        def returnTypesList: List⟦ObjectType⟧ = 
                                    emptyList⟦ObjectType⟧

        //goes through each case and accumulates its parameter and 
        //return types
        for (node.cases) do {block →

            if(block.isMatchingBlock.not) then{
              StaticTypingError.raise("1518: The case you are " ++
                "matching to, {share.stripNewLines(block.toGrace(0))} " ++
                "on line {block.line}, has more than one " ++
                "argument on the left side. This is not currently "++
                "allowed.") with (matchee)
            }

            //If param is a general case(ie. n:Number), accumulate 
            //its type to paramTypesList; ignore if it is a specific 
            //case(ie. 47)
            def blockParam : AstNode = block.params.at(1)
            if (debug3) then {
                io.error.write"\nMy dtype is {blockParam.dtype}"
            }
            if (("string" ≠ blockParam.dtype.kind)
                    && {"num" ≠ blockParam.dtype.kind}) then {
                def typeOfParam = 
                    anObjectType.fromDType (blockParam.dtype) 
                                                with (emptyList)

                if (paramTypesList.contains(typeOfParam).not) then {
                    paramTypesList.add(typeOfParam)
                }
            }

            //Build return type collection
            def blockReturnType : ObjectType = 
                                    objectTypeFromBlock (block)
            if (returnTypesList.contains(blockReturnType).not) then {
              returnTypesList.add (blockReturnType)
            }
        }
        // Type covered by parameters in case (types are variants)
        def paramType: ObjectType = 
                            ot.fromObjectTypeList (paramTypesList)

        // Type returned by variant of all return types in cases
        def returnType: ObjectType = 
                        ot.fromObjectTypeList (returnTypesList)

        if (debug3) then {
            io.error.write "\nmatcheeType now equals: {matcheeType}"
            io.error.write "\nparamType now equals: {paramType}"
            io.error.write "\nreturnType now equals: {returnType}"
        }

        // If matchee not covered by cases then raise a type error
        if (matcheeType.isSubtypeOf(paramType).not) then {
            StaticTypingError.raise("1519: the matchee " ++
                "`{share.stripNewLines(matchee.toGrace(0))}` of type "++
                "{matcheeType} on line {matchee.line} does not " ++
                "match the type(s) {paramTypesList} of the case(s)") 
                                                with (matchee)
        }

        // returnType is type of the match-case statement
        cache.at(node) put (returnType)

        false
    }

    // Type check try-catch-finally clause.  If finally clause, 
    // then type of entire term is that of finally.  Otherwise 
    // disjunction of try and all case blocks.
    // BUG: Return statements in try and cases should be ignored 
    // if there is a finally clause.  Currently they must match
    // the return type of the method. Not sure if this is worth 
    // fixing or just put in type-checking rules.
    method visitTryCatch (node: share.TryCatch) → Boolean {
        // expression being matched and its type
        def body = node.value
        var bodyType: ObjectType := objectTypeFromBlock(body)
        if (debug) then {
           io.error.write "\n440: body with {node} with type {bodyType}"
        }
        // Keep track of return types of try and catch blocks
        def returnTypesList: List⟦ObjectType⟧ =
                                    list⟦ObjectType⟧[bodyType]

        // goes through each case and accumulates its parameter 
        // and return types
        for (node.cases) do {block →

            if(block.isMatchingBlock.not) then{
              StaticTypingError.raise("1518: The exception block you " ++
                "are matching to, {share.stripNewLines(block.toGrace(0))}" ++
                " on line {block.line}, has more than one parameter." ++
                "This is not allowed.") with (block)
            }

            //Build return type collection
            def blockReturnType : ObjectType = 
                                        objectTypeFromBlock(block)

            if (debug) then {io.error.write (
                        "\n460: blockReturnType is {blockReturnType}")
            }

            if (returnTypesList.contains (blockReturnType).not) then {
               returnTypesList.add(blockReturnType)
            }
        }

        // type of the try-catch
        var returnType: ObjectType
        // if there is a finally clause then try-catch returns that 
        // value, so use its type
        if (false != node.finally) then {
            returnType := objectTypeFromBlock (node.finally)
            if (debug) then {io.error.write 
                "\n470: using type of finally clause: {returnType}"
            }
        } else {  // return type is variant of all block types.	   

            // Type returned by variant of all return types in cases
            returnType := ot.fromObjectTypeList(returnTypesList)
            if (debug) then {
               io.error.write ("\n475: no finally: " ++ "using types from try catch: {returnType}")
            }
        }

        if (debug) then {
            io.error.write "\nreturnType now equals: {returnType}\n"
        }


        // returnType is type of the match-case statement
        cache.at(node) put (returnType)

        false
    }

//    method visitMethodType (node) → Boolean {
//        io.error.write 
//           "\n1549: visiting method type {node} not implemented\n"
//
//        runRules (node)
//
//        node.parametersDo { param →
//            runRules (parameterFromNode(param))
//        }
//
//        return false
//    }

//    method visitType (node) → Boolean {
//        io.error.write 
//           "\n1561: visiting type {node} (not implemented)\n"
//        checkMatch (node)
////        io.error.write "432: done visiting type {node}"
//    }

    // type check method declaration
    method visitMethod (meth: AstNode) → Boolean {
        def debug3: Boolean = false
        if (debug3) then {
            io.error.write "\n515: Visiting method {meth}\n"
        }

        // ensure all parameters have known types and 
        // method has return type
        ensureKnown (meth)
     
        // meth.value is Identifier Node
        def name: String = meth.value.value
        // declared type of the method being introduced
        var mType: MethodType
        var returnType: ObjectType

        scope.enter {
            // Enter new scope with parameters to type-check body of method
            if (debug3) then {
                io.error.write "\n1585: Entering scope for {meth}\n"
            }

            //returns the type parameters associated with meth: AstNode to be added to the scope
            def transferBundle : ot.VisitMethodHelperBundle = obtainTypeParams (meth, name)

            //update mType and returnType accordingly
            mType := transferBundle.mType
            returnType := transferBundle.returnType
            // def typeParams: List[[String]] = transferBundle.typeParams
        }

        // if method is just a member name then can record w/variables
        if (isMember(mType)) then {
            scope.variables.at(name) put(returnType)
        }

        // always record it as a method
        scope.methods.at(name) put(mType)

        // Declaration statement always has type Done
        cache.at(meth) put (anObjectType.doneType)
        false

    }

    //Helper method of visitMethod
    //returns the type parameters associated with meth: AstNode to be added to the scope
    method obtainTypeParams (meth: AstNode, name: String)  -> ot.VisitMethodHelperBundle is confidential {
        def debug3: Boolean = false

        var mType: MethodType
        var returnType: ObjectType
        var typeParams: List⟦String⟧ := emptyList⟦String⟧

        if (false != meth.typeParams) then {
            if (debug3) then {
                io.error.write "\n634: meth.signature: {meth.signature}"
                io.error.write "\n634: meth.typeParams.params: {meth.typeParams.params}"
                io.error.write "\n634: meth.value: {meth.value}"
            }
            if (debug) then {
                io.error.write "\n546st: In has type params"
            }
            typeParams := ot.getTypeParams(meth.typeParams.params)
        }
        if (debug) then {
               io.error.write "\n547st: typeParams: {typeParams}"
        }

        mType := aMethodType.fromNode(meth) with (typeParams)
        returnType := mType.retType

        for (mType.typeParams) do { typeParamName : String →
            scope.types.at(typeParamName) 
                  put (anObjectType.typeVble (typeParamName))
        }

        //enter all the parameters to the scope
        for(meth.signature) do { part: AstNode →
            for(part.params) do { param: AstNode →
                scope.variables.at(param.value)
                    put (anObjectType.fromDType (param.dtype) 
                                with (typeParams))
            }
        }
        // We used to collect the type definitions in method 
        // bodies but those are currently not allowed
        // collectTypes((meth.body))
        if (debug) then {
            io.error.write 
                "\n595: collected types for {list(meth.body)}\n"
        }
        // Check types of all methods in the body.  
        // Special case for returns
        checkMethodTypes (meth, returnType, name)
      
        if (debug) then {
            io.error.write "\n594: Done checking body"
        }
        // If no body then the method must return type Done
        if(meth.body.size == 0) then {
            if (anObjectType.doneType.isConsistentSubtypeOf 
                                    (returnType).not) then {
                StaticTypingError.raise(
                    "the method '{name}' on line {meth.line} " ++
                    "declares a result of type '{returnType}'," ++
                    " but has no body") with (meth)
            }
        } else {
            calculateLastExpression(meth,returnType, name)
        }

        return ot.visitMethodHelperBundle (mType, returnType, typeParams)
    }

    //Helper method for obtainTypeParams
    // Calculate type of last expression in body and 
    // make sure it is a subtype of the declared 
    // return type
    method calculateLastExpression (meth: AstNode, returnType: ObjectType, name:String) -> Done is confidential {
        def lastNode: AstNode = meth.body.last

        if (share.Return.matches(lastNode).not) then {
            
            if (debug) then {
                io.error.write("\n694: meth.body.last: {meth.body.last}")
            }

            def lastType = typeOf(lastNode)

            if (debug) then {
               io.error.write 
                  "\n607st: type of lastNode is {lastType}"
            }
            if(lastType.isConsistentSubtypeOf 
                                    (returnType).not) then {
                StaticTypingError.raise(
                    "the method '{name}' on line " ++
                    "{meth.line} declares a result of " ++
                    "type '{returnType}', but returns " ++
                    "an expression of type '{lastType}'") 
                                with (lastNode)
            } else {
                if (debug) then {
                    io.error.write ("\n707: {lastType} is consistent subtype of {returnType}")
                }
            }
        }

        if (debug) then {
            io.error.write (
                "\n2048 type of lastNode in method " ++
                "{meth.nameString} is {lastNode.kind}")
        }
        // If last node is an object definition, the 
        // method can be inherited from so calculate the 
        // supertype (confidential) and put in allCache
        if (lastNode.kind == "object") then {
            visitObject(lastNode)
            def confidType: ObjectType = allCache.at(lastNode)
            allCache.at(meth.nameString) put (confidType)
            if (debug) then {
                io.error.write (
                    "\n2053 confidType is {confidType} " ++
                    "for {meth.nameString}")
            }
        }
    }

    
    // ensure all parameters have known types and 
    // method has return type
    //Helper method for visitMethod
    method ensureKnown (meth:AstNode) -> Done is confidential {
        for (meth.signature) do {s: AstNode →
            for (s.params) do {p: AstNode →
                if (((p.kind == "identifier") && {p.wildcard.not})
                            && {p.decType.value=="Unknown"}) then {
                    StaticTypingError.raise("no type given to declaration" ++
                        " of parameter '{p.value}' on line {p.line}")
                                    with (p)
                }
            }
        }
        if (meth.decType.value=="Unknown") then {
            StaticTypingError.raise(
                "no return type given to declaration of method"++
                " '{meth.value.value}' on line {meth.line}") 
                                with (meth.value)
        }
    }

    // Check types of all methods in the body.  
    // Special case for returns
    //HElper method for visitMethod
    method checkMethodTypes (meth: AstNode, returnType: ObjectType, name : String) -> Done {
        for(meth.body) do { stmt: AstNode →
                checkTypes(stmt)

                // Write visitor to make sure return statements 
                // have right type
                stmt.accept(object {
                    inherit ast.baseVisitor

                    // Make sure return statement return a value 
                    // of same type as the method return type
                    method visitReturn(ret) → Boolean is override {
                        check (ret.value) matches (returnType) 
                                            inMethod (name)
                        // note sure why record returnType?
                        cache.at(ret) put (returnType)
                        return false
                    }
                    // Don't check inside embedded methods as they 
                    // have different return type from the 
                    // outer method
                    method visitMethod(node) → Boolean is override {
                        false
                    }
                })
        }

    }

    // type check a method request
    method visitCall (req: AstNode) → Boolean {
        // Receiver of request
        def rec: AstNode = req.receiver
        def debug3 = false

        if (debug3) then {
            io.error.write ("\n1673: visitCall's call is: "++
                "{rec.toGrace(0)}.{req.nameString}"++
                " with kind {rec.kind}")
        }

        // type of receiver of request
        def rType: ObjectType = typeOfReceiver(rec)

        def callType: ObjectType = if (rType.isDynamic) then {
            if (debug) then {
                io.error.write "\n624: rType: {rType} is dynamic}"
            }
            anObjectType.dynamic
        } else {
            //Since we can't have a method or a type with the 
            //same name. A call on a name can be searched in 
            //both method and type lists.  Just have to assume 
            //that the programmer used nonconflicting names

            var name: String := req.nameString
            if (name.contains "$object(") then {
                //Adjust name for weird addition when used 
                // in inherit node
                def dollarAt = name.indexOf("$object(")
                name := name.substringFrom(1) to (dollarAt - 1)
            }
            // String showing what call looks like
            def completeCall : String = 
                    "{req.receiver.toGrace(0)}.{req.nameString}"
                    ++ " with kind {req.receiver.kind}"
            if (debug3) then {
                io.error.write "\n2154: {completeCall}"
                io.error.write "\n2155: {req.nameString}"

                io.error.write 
                    "\n2000: rType.methods is: {rType.methods}"
            }
            getResultType(req, rec, rType, name, completeCall)
            
        }
        if (debug3) then {
            io.error.write "\n1701: callType: {callType}"
        }
        cache.at(req) put (callType)
        // tells the callNode to typecheck its receiver, 
        // arguments, and generics
        true
    }

    // compute and return type of the receiver of method request
    method typeOfReceiver (rec: AstNode) -> ObjectType 
                                    is confidential {
            // type of receiver of request
        def debug3: Boolean = false
        if (rec.nameString == "self") then {
            if (debug3) then {
                io.error.write "\n1675: looking for type of self"
            }
            scope.variables.find("$elf") butIfMissing {
                StaticTypingError.raise "type of self missing" with(rec)
            }
        } elseif {rec.nameString == "module()object"} then {
            // item from prelude
            if (debug3) then {
                io.error.write "\n602: looking for type of module"
            }
            scope.variables.findFromLeastRecent("$elf") butIfMissing {
                StaticTypingError.raise "type of self missing" with(rec)
            }
        } elseif {rec.kind == "outer"} then {
            if (debug3) then {
                io.error.write "\n610: looking for type of outer"
                io.error.write "\n611: levels: {rec.numberOfLevels}"
            }
            def outerMethodType: MethodType =
                scope.methods.findOuter (rec.numberOfLevels) 
                    butIfMissing {
                        StaticTypingError.raise "type of outer missing" 
                            with(rec)
            }
            outerMethodType.retType
        } else {  // general case returns type of the receiver
            if (debug3) then {
                io.error.write "\n2085 rec.kind = {rec.kind}"
            }
            typeOf(rec)
        }
    }

    method getResultType (req: AstNode, rec: AstNode, 
            rType: ObjectType, name: String,
            completeCall: String) -> ObjectType is confidential {
       // look for method name in type of receiver
       match(rType.getMethod(name))
         case { (ot.noSuchMethod) →
             if (debug) then {
                io.error.write "\n2001: got to case noSuchMethod "++
                    "while looking for {name}"
                io.error.write 
                    "\n2002: method scope here is {scope.methods}"
             }
             StaticTypingError.raise(
                 "no such method or type '{req.nameString}' in " ++
                 "`{share.stripNewLines(rec.toGrace(0))}` of type\n" ++
                 "    '{rType}' \nin type \n  '{rType.methods}'" ++
                 " used on line {rec.line}")
                     with(req)
       } case { meth : MethodType →
             // found the method, make sure arguments match 
             // parameter types
             if (debug) then {
                 io.error.write 
                    "\nchecking request {req} against {meth}"
             }
             // returns type of result
             check(req) against(meth)
       }
    }

    // Type check an object.  Must get both public and 
    // confidential types
    method visitObject (obj: AstNode) → Boolean {
        def debug3: Boolean = false
        // type check body of the method
        if (debug3) then {
            io.error.write "\n684 Ready to type check {obj}***"
        }
        visitObjectHelper(obj, false)
    }

    method visitObjectHelper (obj: AstNode, hasImports: Boolean) → Boolean {
        def debug3: Boolean = false
        def pcType: PublicConfidential = scope.enter {
            var withoutImport: AstNode
            if(hasImports) then {
                def importNodes: List⟦AstNode⟧ = emptyList⟦AstNode⟧
                // All statements in module
                def bodyNodes: List⟦AstNode⟧ = list(obj.value)

                // Goes through the body of the module and processes imports
                for (bodyNodes) do{ nd : AstNode →
                    match (nd)
                      case {imp: share.Import →
                        // Visitimport processes the import and puts its type 
                        // on the variable scope and method scope
                        visitImport(imp)
                        importNodes.add(imp)
                    } else { } // Ignore non-import nodes
                }

                // Removes import statements from the body of the module
                for(importNodes) do{nd : AstNode →
                    bodyNodes.remove(nd)
                }

                // Create equivalent module without imports
                withoutImport := ast.moduleNode.body(bodyNodes)
                                named (obj.nameString) scope (obj.scope)

                visitObjectHelper(withoutImport, false)
            }

            if(hasImports) then {
                // Collect types declared in obj into new level of scope
                collectTypes (list (withoutImport.value))
                processBody (list (withoutImport.value), withoutImport.superclass)
            } else {
                collectTypes (list (obj.value))
                processBody (list (obj.value), obj.superclass)     
            }
        }
        // Record both public and confidential methods 
        // (for inheritance)
        cache.at(obj) put (pcType.publicType)
        allCache.at(obj) put (pcType.inheritableType)
        if (debug3) then {
            io.error.write "\n1971: *** Visited object {obj}***"
            io.error.write 
                "\n1973 public type is {pcType.publicType}"
            io.error.write 
                "\n1973 inheritable type is {pcType.inheritableType}"
            io.error.write( 
                "\n2153: Methods scope at end of visitObject is: " ++
                                                    scope.methods)
        }
        false
    }

    //Process dialects and import statements
    //TODO: handle dialects
    method visitModule (node: AstNode) → Boolean {  // added kim
        def debug3: Boolean = false
        if (debug3) then {
           io.error.write "\n1698: visiting module {node}"
        }
        // import statements in module
        visitDialect (node.theDialect)

        // type check the remaining object (w/o import statements)
        visitObjectHelper (node, true)
    }

    // array literals represent collections (sh'd fix to be lineups)
    method visitArray (lineUpLiteral) → Boolean {
        def debug2: Boolean = false
        if (debug2) then {
            io.error.write "\n1704: visiting array {lineUpLiteral}"
        }
        var arrayType: ObjectType := anObjectType.base
        def lupSize: Number = lineUpLiteral.value.size
        if (debug2) then {
            io.error.write "\n822: line up size is {lupSize}"
        }
        arrayType := anObjectType.bottom
        for (1..lupSize) do {index: Number ->
              def eltType: ObjectType = 
                typeOf (lineUpLiteral.value.at (index))
              if (debug2) then {
                  io.error.write "\n825 eltType is {eltType}"
              }
              arrayType := arrayType | eltType
              if (debug2) then {
                  io.error.write "\nback to 828"
                  io.error.write "\n827 new arrayType: {arrayType}"
              }
        }
        def newType: ObjectType = ot.collection.apply(list[arrayType])
        if (debug2) then {
            io.error.write "\n828: lineup holds elts of type {arrayType}"
            io.error.write "\n829: New type is {newType}"
        }
        cache.at (lineUpLiteral) put (newType)
        false
    }

    // members are type-checked like calls
    method visitMember (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write
                "\n744 visiting member of {node.toGrace(0)}"
        }
        visitCall (node)
    }

    // NOT YET IMPLEMENTED
    method visitGeneric (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write 
                "\n1715: visiting generic {node} (not implemented)"
        }
        checkMatch (node)
    }

    // look up identifier's type in scope
    method visitIdentifier (ident: AstNode) → Boolean {
        def idType: ObjectType = if (ident.value == "outer") then {
            // TODO: Don't think code ever gets here.  
            // Outer handled in visitCall instead
            // io.error.write 
            //     "Processing outer at {ident} with {scope.size}"
            outerAt(scope.size)
        } else {
            // TODO: Should raise an error, not just ignore!
            scope.variables.find(ident.value)
                                butIfMissing { anObjectType.dynamic }
        }
        cache.at (ident) put (idType)
        true

    }

    // Type check type declaration
    method visitTypeDec(node: share.TypeDeclaration) → Boolean {
        def debug3: Boolean = false
        if (debug3) then {
            io.error.write "\n875 visit type dec for {node}"
        }
        //check if the typedec is generic (type T⟦K,V⟧ = ...)
        if (false ≠ node.typeParams) then {
            // create GenericType to later be instantiated 
            // with real types
            def genType : GenericType = aGenericType.fromTypeDec(node)
            cache.at(node) put (genType)
        } else {
            //get the type of the right-hand side of the equals sign
            def vType : ObjectType = 
                anObjectType.fromDType (node.value) with (emptyList)
            if (debug3) then {
               io.error.write "\n875 type is {vType}"
               io.error.write "\n876 methList is {vType.methList}"
            }

            cache.at(node) put (vType)
        }
        false
    }

    // TODO: Fix later
    method visitOctets (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write 
                "\n1736: visiting Octets {node} (not implemented)"
        }
        false
    }

    // type of string is String
    method visitString (node: AstNode) → Boolean {
        cache.at (node) put (anObjectType.string)
        false
    }

    // type of number is Number
    method visitNum (node: AstNode) → Boolean {
        cache.at (node) put (anObjectType.number)
        false
    }

    // If the op is & or |, evaluate it
    // Otherwise treat it as a call
    method visitOp (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write"\n2283 type checking op"
        }
        if(node.value == "&") then {
            cache.at(node) put (typeOf(node.left) & typeOf(node.right))
        } elseif {node.value == "|"} then {
            cache.at(node) put (typeOf(node.left) | typeOf(node.right))
        } else {
            visitCall(node)
        }
        false
    }

    method visitTypeLiteral(node: share.TypeLiteral) → Boolean {
        cache.at (node) put
            (anObjectType.fromDType (node) with (emptyList))
        false
    }

    method visitBind (bind: AstNode) → Boolean {
        if (debug) then {
           io.error.write "\n 1758: Visit Bind"
           io.error.write ("\n"++bind.pretty(0))
        }
        // target of assignment
        def dest: AstNode = bind.dest

        match (dest) case { _ : share.Member →
            // TODO: I don't believe this case is ever taken.  
            // Compiled as call instead.
            // Should be able to drop match altogether!  
            // io.error.write "{dest} in case member"
            var nm: String := dest.nameString
            if (! nm.endsWith ":=(1)") then {
                nm := nm ++ ":=(1)"
            }
            // rec.memb
            def rec: AstNode = dest.in

            // Type of receiver
            // if receiver is self then look it up, else type check it
            def rType: ObjectType = if(share.Identifier.matches(rec)
                        && {rec.value == "self"}) then {
                scope.variables.find("$elf") butIfMissing {
                    Exception.raise "type of self missing" with(rec)
                }
            } else {
                typeOf(rec)
            }

            if (rType.isDynamic) then {
                anObjectType.dynamic
            } else {
                // look up type of the method in the receiver
                match(rType.getMethod(nm))
                  case { (ot.noSuchMethod) →
                    StaticTypingError.raise("no such method '{nm}' in " ++
                        "`{share.stripNewLines(rec.toGrace(0))}` of "++
                        " type '{rType}' on line {bind.line}") 
                                                    with (bind)
                } case { meth : MethodType →
                    // create a new call node (instead of bind) 
                    // and type check
                    def req = ast.callNode.new(dest,
                        list [ast.callWithPart.new(dest.value, 
                                        list [bind.value])])
                    check(req) against(meth)
                }
            }

        } else {
            // destination type
            def dType: ObjectType = typeOf(dest)

            def value: AstNode = bind.value
            // value type
            def vType: ObjectType = typeOf(value)

            // make sure value consistent with destination
            if(vType.isConsistentSubtypeOf(dType).not) then {
                StaticTypingError.raise("the expression " ++
                    "`{share.stripNewLines(value.toGrace(0))}` of "++
                    " type '{vType}' online {value.line} does " ++
                    "not satisfy the type '{dType}' " ++
                    "of `{share.stripNewLines(dest.toGrace(0))}`") 
                                        with (value)
            }
        }
        // type of bind is always Done
        cache.at (bind) put (anObjectType.doneType)
        false
    }


    // type check both def and var declarations
    method visitDefDec (defd: AstNode) → Boolean {
        def debug2: Boolean = false
        def name: String = defd.nameString
        if (defd.decType.value=="Unknown") then {
            // raise error if no type given in declaration
            var typ: String := 
                if (share.Var.matches(defd)) then {"var"} else {"def"}
            StaticTypingError.raise("no type given to declaration" ++ 
               " of {typ} '{defd.name.value}' on line {defd.line}")
                                                    with (defd.name)
        }
        // Declared type of feature
        var defType: ObjectType := 
            anObjectType.fromDType (defd.dtype) with (emptyList)
        if (debug2) then {
            io.error.write "\n1170: defType for {name} is {defType}"
        }
        // initial value
        def value = defd.value
        // Make sure initial value (if any) is consistent 
        // with declared type
        if(false ≠ value) then {  // initial value provided
            def vType: ObjectType = typeOf(value)
            if (debug2) then {
               io.error.write "\n1179: vType for {name} is {vType}"
            }
            // infer type based on initial value if definition 
            // given w/out type
            if(defType.isDynamic && (defd.kind == "defdec")) then {
                defType := vType
            } elseif {vType.isConsistentSubtypeOf(defType).not} then {
                // initial value not consistent with declared type
                StaticTypingError.raise(
                    "the expression `{share.stripNewLines(value.toGrace(0))}`" ++
                    " of type '{vType}'  on line {value.line} does " ++
                    "not have type {defd.kind} annotation '{defType}'") 
                                                with (value)
            }
        }

        scope.variables.at(name) put(defType)
        // If field is readable and/or writable, add public methods
        //  for getting and setting
        if (defd.isReadable) then {
            scope.methods.at(name) put (
                        aMethodType.member (name) ofType (defType))
        }
        if (defd.isWritable) then {
            def name' = name ++ ":=(1)"
            def param = aParam.withName(name) ofType(defType)
            def sig = list[ot.aMixPartWithName(name') 
                                        parameters(list[param])]
            scope.methods.at (name') put(aMethodType.signature(sig) 
                                returnType(anObjectType.doneType))
        }
        cache.at (defd) put (anObjectType.doneType)
        false
    }

    // Handle variable declaration like definition declaration
    method visitVarDec (node: AstNode) → Boolean {
        visitDefDec (node)
    }

    // Grab information from gct file
    // Move processImport back into visitImport
    method visitImport (imp: AstNode) → Boolean {
        def debug3: Boolean = false
        if (debug3) then {
            io.error.write "\n1861: visiting import {imp}"
        }
        // headers of sections of gct form keys
        // Associated values are lines beneath the header
        def gct: Dictionary⟦String, List⟦String⟧⟧ = 
            xmodule.gctDictionaryFor(imp.path)
        def impName : String = imp.nameString
        if (debug3) then {
            io.error.write("\n1953 gct is {gct}")
            io.error.write("\n1954 keys are {gct.keys}\n")
        }

        //retrieves the names of public methods from imported module
        def importMethods : Set⟦MethodType⟧ = processGct(gct, impName)

        // Create the ObjectType and MethodType of import
        def impOType: ObjectType = anObjectType.fromMethods(importMethods)

        def sig: List⟦MixPart⟧ = list[ot.aMixPartWithName(impName)
                                    parameters (emptyList⟦Param⟧)]
        def impMType: MethodType = aMethodType.signature(sig)
                                            returnType (impOType)

        // Store import in scopes and cache
        scope.variables.at(impName) put(impOType)
        scope.methods.at(impName) put(impMType)
        cache.at(imp) put (impOType)
        if (debug3) then {
            io.error.write("\n2421: ObjectType of the " ++
                                "import {impName} is: {impOType}")
        }
        false
    }

    // Comb through an AstNode to prepend the import nickname to any type references
    // that are not prelude types.
    //
    // When importing a file with the nickname 'impName', we need to prepend
    // 'impName' to references to any type 'T' that are accessible in our import but
    // are only accessible to us as 'impName.T'. These type references are saved as
    // identifier nodes inside an Ast tree created from the type declarations and
    // method types saved in the gct file.
    class importVisitor(impName : String) → ast.AstVisitor {
        inherit ast.baseVisitor

        // Special case where we also want to prepend impName to the name of the
        // type definition as well as any type parameters it may have.
        method visitTypeDec(typeDec:ast.AstNode) → Boolean {
            //typeDec.name is a identifierBinding, so we need to manually prepend
            //impName here
            typeDec.name.name := "{impName}.{typeDec.nameString}"

            prependToTypeParam(typeDec)
            true
        }

        method visitMethod(meth:ast.AstNode) → Boolean {
            prependToTypeParam(meth)
            true
        }

        method visitMethodType(methType:ast.AstNode) → Boolean {
            prependToTypeParam(methType)
            true
        }

        // Prepend impName to references to types that are not prelude types
        //
        // Also make sure we are prepending impName to type annotations and not
        // parameter names(stored as identifierBindings).
        method visitIdentifier(ident:ast.AstNode) → Boolean {
            // identifierresolution can also prepend 'self' and 'module()Object' to
            // calls. These are not needed for type-checking so we ignore them
            if ((ident.name == "self") || {ident.name == "module()Object"}) then {
                ident.name := impName
            } elseif {preludeTypes.contains(ident.name).not} then {
                if (ident.isBindingOccurrence.not) then {
                    ident.name := "{impName}.{ident.name}"
                }
            }
            true
        }

        // Prepend impName to type parameters
        //
        // Do not need to check if type params are prelude types since the
        // identifier resolution makes sure their name is unused.
        method prependToTypeParam(node:ast.AstNode) → Done is confidential {
            if (false ≠ node.typeParams) then {
                for (node.typeParams.params) do { tParam : ast.AstNode →
                    tParam.name := "{impName}.{tParam.name}"
                }
            }
        }
    }

    method processGct(gct: Dictionary⟦String, List⟦String⟧⟧, 
                            impName: String) → Set⟦MethodType⟧ {
        def importMethods : Set⟦MethodType⟧ = emptySet
        def basicImportVisitor : ast.AstVisitor = importVisitor(impName)
        def typeDecs: List[[share.TypeDeclaration]] = emptyList
        gct.keys.do { key : String →
            if { key.startsWith("typedec-of:") } then {
                // Example key: 
                // typedec-of:MyType:
                //   type MyType = {
                //     method m -> Number
                //   }

                // Gets the name of the type
                def headerName : String = split(key, ":").at(2)
                def typeName : String = split(headerName, ".").last
                def prefx : String = headerName.substringFrom(1)
                            to(headerName.size - typeName.size - 1)

                def tokens = lex.lexLines(gct.at(key))
                def typeDec: AstNode = parser.typedec(tokens)
                
                if(typeName.startsWith("$")) then {
                    typeDec.value.accept(importVisitor(typeDec.nameString))
                }

                if (prefx == "") then {
                    typeDec.accept(basicImportVisitor)
                } else {
                    typeDec.accept(importVisitor("{impName}.{prefx}"))
                }

                // If the type name begins with a '$', then it 
                // is a type that returns an object corresponding 
                // to a module that was publicly imported by our 
                // own import. We use this type to construct the
                // method for accessing the imported module's 
                // public methods.
                if (typeName.at(1) == "$") then {
                    def importName : String = 
                        typeName.substringFrom (2)
                    def mixPart : MixPart = 
                        ot.aMixPartWithName(importName)
                                        parameters(emptyList⟦Param⟧)

                    importMethods.add (
                        aMethodType.signature (list[mixPart]) returnType 
                            (anObjectType.fromDType(typeDec.value)with(emptyList)))
                } else {
                    typeDecs.push(typeDec)
                    // Put type placholders into the scope
                    if(false ≠ typeDec.typeParams) then {
                        scope.generics.at(typeDec.nameString) put (ot.aGenericType.placeholder)
                    } else {
                        scope.types.at(typeDec.nameString) put (ot.anObjectType.placeholder)
                    }
                }
            }
        }
        for(typeDecs) do { typeDec ->
            updateTypeScope(typeDec)
        }
        gct.keys.do { key: String ->
            // Example key: 
            // publicMethod:d:
            // d → D⟦Number⟧MyType
            if (key.startsWith("publicMethod:")) then {
                def tokens = lex.lexLines(gct.at(key))
                def methodType = parser.methodInInterface(tokens)
                print("\nmethodType.typeParams: {methodType.typeParams}")
                methodType.accept(basicImportVisitor)
                importMethods.add(aMethodType.fromNode(methodType)
                                      with (emptyList[[String]]))
            }
        }
        importMethods
    }

    // type check expression being returned via return statement
    method visitReturn (node: AstNode) → Boolean {
        cache.at(node) put (typeOf(node.value))
        false
    }

    // Type check inherits clause in object
    method visitInherits (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1999: visit inherits with {node} "++
                            "which has kind {node.kind}"
            io.error.write "\n1999: receiver: {node.value.receiver}"
            io.error.write 
                "\n1999: parts: {node.value.parts.removeLast}"
        }
        def nodeType: ObjectType = typeOf(node.value)
        cache.at(node) put (nodeType)
        if (debug) then {
            io.error.write "\n2000 inherit {node} has type {nodeType}"
        }
        false
    }


    // TODO:  Not done
    // Should be treated like import, but at top level
    // Add to base type
    method visitDialect (node: AstNode) → Boolean {
        if (debug) then {
            io.error.write "\n1919: visiting dialect {node}"
        }
        false
    }

    // Grab information from gct file
    // Move processImport back into visitImport
    // TODO: Make this work
    method visitDialect2 (dlct: AstNode) → Boolean {
        def debug2: Boolean = false
        if (debug2) then {
            io.error.write "\n1861: visiting dialect {dlct}"
        }
        // headers of sections of gct form keys
        // Associated values are lines beneath the header
        def gct: Dictionary⟦String, List⟦String⟧⟧ = 
            xmodule.gctDictionaryFor(dlct.value)
        def dialectName : String = dlct.nameString
        if (debug2) then {
            // io.error.write("\n1953 gct is {gct}")
            // io.error.write("\n1954 keys are {gct.keys}\n")
        }

        //retrieves the names of public methods from imported module
        def dialectMethods : Set⟦MethodType⟧ = processGct(gct, dialectName)

        for (dialectMethods) do {meth: MethodType ->
            scope.methods.at(meth.nameString) put(meth)
            if (debug2) then {
               io.error.write ("\n2421: Adding from dialect method :"++
                            "{meth.nameString}")
            }
        }

        false
    }
}


// TODO: Don't believe this code is ever executed.  
// Handled in visitCall instead!!
// Not sure what this type is doing

method outerAt(i : Number) → ObjectType is confidential {
    // Required to cope with not knowing the prelude.
    if(i <= 1) then {
        return anObjectType.dynamic
    }
    if (debug) then {
        io.error.write "processing outer"
    }
    def vStack: List⟦ Dictionary⟦String, ObjectType⟧ ⟧ = 
                        scope.variables.stack

    def curr: Dictionary⟦String, ObjectType⟧ = vStack.at(i)

    return curr.at("outer") ifAbsent {
        def prev: ObjectType = outerAt(i - 1)

        def mStack: List⟦ Dictionary⟦String, MethodType⟧ ⟧ = 
                    scope.methods

        def vars: Dictionary⟦String, ObjectType⟧ = vStack.at(i - 1)
        def meths: Set⟦MethodType⟧ = mStack.at(i - 1).values

        def oType: ObjectType = anObjectType.fromMethods(meths)

        def mType: MethodType = 
                    aMethodType.member("outer") ofType(oType)

        curr.at("outer") put(oType)
        mStack.at(i).at("outer") put(mType)

        oType
    }
}

// Process type declaration by putting result in type or 
// generics scope at top level
method updateTypeScope(typeDec : share.TypeDeclaration) → Done 
                                            is confidential {
    //check whether the typeDec is a GenericType and 
    // process accordingly
    def debug3 = false
    var oType : ObjectType
    if(false ≠ typeDec.typeParams) then {
        if (debug) then {
            io.error.write "\n1243: creating genType from {typeDec}"
        }
        def genType : GenericType = aGenericType.fromTypeDec(typeDec)
        // oType := genType.oType
        scope.generics.at(typeDec.nameString) put (genType)
        if (debug) then {
            io.error.write "\n1246: added to generics: {genType}"
            io.error.write "\n1563: scope.generics.stack: {scope.generics.stack}"
        }
    } else {
        if (debug3) then {
            io.error.write "\n1243: creating oType from {typeDec}"
        }
        // DEBUG: Was definedByNode
        def typeName: String = typeDec.nameString
        oType := anObjectType.fromDType (typeDec.value) with (emptyList[[String]])
        scope.types.at(typeName) put(oType)
        if (debug3) then {
            io.error.write "\n1252: added to types: {oType}"
        }
    }
}

method updateMethScope(meth : AstNode) → MethodType is confidential {
    var mType
    if(false != meth.typeParams) then {
        def typeParams = ot.getTypeParams(meth.typeParams.params)
        mType := aMethodType.fromNode(meth) with (typeParams)  
    } else {
        mType := aMethodType.fromNode(meth) with (emptyList[[String]])  
    }
    scope.methods.at(mType.nameString) put (mType)
    mType
}

// Type check body of object definition
// TODO: This method is still way too long!!
method processBody (body : List⟦AstNode⟧, 
        superclass: AstNode | false) → PublicConfidential is confidential {
            
    def debug3: Boolean = false
    if (debug3) then {
        io.error.write "\n1958: superclass: {superclass}\n"
    }

    // Process inherited methods
    var inheritedMethods: Set⟦MethodType⟧ := emptySet
    def hasInherits = false ≠ superclass

    //CREATE SUPERTYPE OBJECTTYPE
    def transferPairTwo : PublicConfidential = superType (superclass)
    def superType: ObjectType = transferPairTwo.publicType
    def publicSuperType : ObjectType = transferPairTwo.inheritableType

    // set meaning of "outer:
    def outerVal: ObjectType = scope.variables.find("$elf") 
                                        butIfMissing {
        anObjectType.base
    }
    scope.variables.at ("outer") put (outerVal)
    scope.methods.at ("outer") put (aMethodType.member("outer") 
                                    ofType(outerVal))

    // Type including all confidential features
    var internalType: ObjectType

    // Type including only public features
    //CREATE THE PUBLICTYPE OBJECT, type including only public features
    //Also update internalType
    def transferPairThree : PublicConfidential= 
            publicType (body,superType, publicSuperType, outerVal)
    def publicType: ObjectType = transferPairThree.publicType
    internalType := transferPairThree.inheritableType

    // External type for self -- i.e., if self is used as a parameter
    // in a method request -- can't see confidential features
    scope.variables.at("self") put(publicType)
    if (debug3) then {
        io.error.write "\n2744: Type of self is {publicType}"
    }

    // Type-check the object body, dropping the inherit clause
    typeCheckObjectBody (hasInherits, body)

    if (debug3) then {
        io.error.write "\n 2673 generics scope is: {scope.generics}"
        io.error.write "\n 2674 types scope is: {scope.types}"
        io.error.write "\n 2675 methods scope is: {scope.methods}"
    }
    // return pair of public and internal type 
    // (so can inherit from it)
    pubConf(publicType,internalType)
}

//Add new confidential method for each alias given with super class
//Helper method for processBody and superType
method addAliasMethods (superclass: AstNode | false,
         inheritedMethods : Set⟦MethodType⟧) -> List⟦MethodType⟧ is confidential{

    def debug3 = false
    def aliasMethods: List⟦MethodType⟧ = emptyList
    for (superclass.aliases) do {aliasPair →
        for (inheritedMethods) do {im →
            if (debug3) then {
                io.error.write (
                    "\n1144 comparing {aliasPair.oldName.value}"++
                    " and {im.nameString}")
            }
            if (aliasPair.oldName.value == im.nameString) then {
                // Found a match for alias clause with im
                // Build a new method type for alias
                def oldSig: List⟦MixPart⟧ = im.signature
                var aliasNm: String := aliasPair.newName.value
                // unfortunately name has parens at end 
                // -- drop them
                def firstParen: Number = aliasNm.indexOf("(")
                if (firstParen > 0) then {
                    aliasNm := 
                        aliasNm.substringFrom (1) to (firstParen)
                }
                def newFirst: MixPart = 
                        ot.aMixPartWithName (aliasNm)
                            parameters (oldSig.at(1).parameters)
                def newSig: List⟦MixPart⟧ = 
                        oldSig.copy.at (1) put (newFirst)
                // method type for alias
                def newMethType: MethodType = 
                        ot.aMethodType.signature (newSig)
                                    returnType (im.retType)
                aliasMethods.add(newMethType)
                if (debug3) then {
                    io.error.write 
                        "\n1154: just added alias {newMethType}"
                }
            }
        }
    }

    return aliasMethods
}

//Create superType: ObjectType, type including the features of the superClass
//Helper method for processBody
method superType (superclass: AstNode | false)  -> PublicConfidential is confidential{

    def debug3 = false
    def hasInherits = false ≠ superclass
    var inheritedMethods: Set⟦MethodType⟧ := emptySet
    var publicSuperType: ObjectType := anObjectType.base

    def superType: ObjectType = if(hasInherits) then {
        def transfers: PublicConfidential = superTypeWithInherits (inheritedMethods, publicSuperType, superclass)
        publicSuperType := transfers.publicType
        transfers.inheritableType
    } else {
        anObjectType.base
    }
    // Finished computing supertype, but don't associate with "super"
    if (debug3) then {
        io.error.write "\n1989: superType is {superType}\n"
    }

    return pubConf (superType, publicSuperType)
   
}
//Throw away excluded methods from the inheritedMethods list and the pubInheritedMethods list
//Helper method of processBody and superType
method throwAwayExcludedMethods (superclass: AstNode | false,
        inheritedMethods': Set⟦MethodType⟧, pubInheritedMethods': Set⟦MethodType⟧)
        -> ot.SetMethodTypePair is confidential {

    var inheritedMethods := inheritedMethods'
    var pubInheritedMethods := pubInheritedMethods'
    var droppedMethods: List[[MethodType]] := 
                                emptyList[[MethodType]]
    var pubDroppedMethods: List[[MethodType]] := 
                                emptyList[[MethodType]]
    // Throw away methods being excluded
    for (superclass.exclusions) do {ex →
        // First throw away from list of all inherited methods
        for (inheritedMethods) do {im →
            if (ex.nameString == im.nameString) then {
                if (debug) then {
                    io.error.write "\n1126 removing {im}"
                }
                droppedMethods.add(im)
            }
        }
        // Throw away from public inherited methods
        for (pubInheritedMethods) do {im →
            if (ex.nameString == im.nameString) then {
                pubDroppedMethods.add(im)
            }
        }
    }
    inheritedMethods.removeAll(droppedMethods)
    pubInheritedMethods.removeAll(pubDroppedMethods)

    if (debug) then {
        io.error.write "aliases: {superclass.aliases}"
    }

    return ot.setMethodTypePair (inheritedMethods, pubInheritedMethods)
    
}

//Create superType when superClass is not false
method superTypeWithInherits (inheritedMethods': Set⟦MethodType⟧, publicSuperType': ObjectType, superclass: AstNode) -> PublicConfidential {

    var inheritedMethods := inheritedMethods'
    var publicSuperType := publicSuperType'
    def inheriting: AstNode = superclass
    // TODO: make sure no self in "inherit" clause??

    if (debug) then {
        io.error.write 
            "\nGT1981: checking types of inheriting = {inheriting}\n"
    }
    var name: String := inheriting.value.nameString
    if (name.contains "$object(") then {
        if(debug) then {
            io.error.write "\n inheriting.value.parts : {inheriting.value.parts}"
        }

        // fix glitch with method name in inheritance clause
        //inheriting.value.parts.removeLast
        def dollarAt = name.indexOf("$object(")
        name := name.substringFrom(1) to (dollarAt - 1)    
    }

    // Handle exclusions and aliases
    // Unfortunately no type info with exclusions, so if code 
    // says exclude p(s:String) and p in subclass takes a Number, 
    // it will be dropped without any warning of the error.
    var inheritedType: ObjectType := allCache.at(name)
    inheritedMethods := inheritedType.methods.copy
    publicSuperType := typeOf(inheriting.value)

    //All public methods from super type
    var pubInheritedMethods := publicSuperType.methods.copy

    def transferPairFour = throwAwayExcludedMethods 
            (superclass, inheritedMethods, pubInheritedMethods)
    inheritedMethods :=  transferPairFour.first
    pubInheritedMethods :=transferPairFour.second

    // Add new confidential method for each alias given 
    // with super class
    def aliasMethods: List⟦MethodType⟧ = addAliasMethods 
            (superclass, inheritedMethods)
  
    // Add all aliases to inherited methods, but not public ones
    inheritedMethods.addAll(aliasMethods)

    //Node of inheritedType could be inheriting.value 
    // or inheriting
    inheritedType := anObjectType.fromMethods(inheritedMethods)
    publicSuperType := 
            anObjectType.fromMethods(pubInheritedMethods)

    if (debug) then {
        io.error.write 
            "\1144: public super type: {publicSuperType}"
        io.error.write 
            "\n1145: inherited type: {inheritedType}"
    }

    pubConf (publicSuperType,inheritedType)
}

//Create type including only public features
//helper method of processBody
method publicType (body : List⟦AstNode⟧, superType: ObjectType,
        publicSuperType: ObjectType,  outerVal: ObjectType) 
        -> PublicConfidential is confidential{

    // Type including all confidential features
    var internalType : ObjectType 

    def publicType: ObjectType = if (superType.isDynamic) then {
        // if supertype is dynamic, then so is public type
        scope.variables.at("$elf") put (superType)
        superType
    } else {
        
        var allMethods: Set⟦MethodType⟧
        var publicMethods: Set⟦MethodType⟧

        def transferBundle: ot.PublicTypeReturnBundle = 
                publicTypeElse(outerVal, publicSuperType, superType, body)
        allMethods := transferBundle.allMethods
        publicMethods := transferBundle.publicMethods
        internalType:= transferBundle.internalType
        anObjectType.fromMethods(publicMethods)
    }

    return pubConf (publicType, internalType)
    //return ot.typePair (publicType, internalType)
    
}

//Helper method of publicType
//Handles the else case where superType is not dynamic 
method publicTypeElse (outerVal, publicSuperType,superType, body) -> ot.PublicTypeReturnBundle {

        var publicMethods : Set⟦MethodType⟧
        var allMethods : Set⟦MethodType⟧
        //var internalType : ObjectType

        def isParam: Param = 
            aParam.withName("other") ofType (anObjectType.base)
        def part: MixPart = 
            ot.aMixPartWithName ("isMe") parameters (list[isParam])

        // add isMe method as confidential
        def isMeMeth: MethodType = 
            aMethodType.signature(list[part]) 
                                returnType (anObjectType.boolean)
        def outerMeth: MethodType = 
                aMethodType.member("outer") ofType (outerVal)
        publicMethods := publicSuperType.methods.copy
        allMethods:= superType.methods.copy
        // isMe is confidential
        allMethods.add(isMeMeth)
        allMethods.add(outerMeth)
        // collect embedded types in these dictionaries
        def publicTypes: Set[[ObjectType⟧ = emptySet
        def allTypes: Set[[ObjectType⟧ = emptySet

        // gather types for all methods in object (including 
        // implicit ones for vars and defs)
        def transferPair: ot.SetMethodTypePair =
                 gatherTypesForMethods (body, allMethods, publicMethods)
        publicMethods := transferPair.first
        allMethods := transferPair.second


        if (debug) then {
            io.error.write "\n1201 allMethods: {allMethods}"
        }

        var internalType: ObjectType := anObjectType.fromMethods(allMethods)

        // Type of self as a receiver of method calls
        scope.variables.at("$elf") put (internalType)
        if (debug) then {
            io.error.write "\n1204: Internal type is {internalType}"
        }

        return ot.publicTypeReturnBundle (allMethods, publicMethods, internalType)
}

//Gather types for all methods in object (including 
//implicit ones for vars and defs), ensuring compatible overriding if applicable
//and storing in the scope in the progress
//Helper method for processbody and publicType
method gatherTypesForMethods (body : List⟦AstNode⟧, allMethods' :Set⟦MethodType⟧,
        publicMethods' : Set⟦MethodType⟧) -> ot.SetMethodTypePair  is confidential{

    var allMethods := allMethods'
    var publicMethods := publicMethods'
    def debug3 = false

    for(body) do { stmt: AstNode →
            if (debug3) then {
                io.error.write "\n2009: processing {stmt}"
                io.error.write 
                    "\n1375: stmt's toGrace is: {stmt.toGrace(0)}"
            }

            match(stmt) case { meth : share.Method →

                def transferPair: ot.SetMethodTypePair = 
                        methCase (meth, allMethods, publicMethods)
                publicMethods := transferPair.first
                allMethods := transferPair.second
              
            } case { defd : share.Def | share.Var →
                // Create type of method giving access 
                // to def or var value

                def transferPair: ot.SetMethodTypePair = 
                        defCase(defd, allMethods, publicMethods)
                publicMethods := transferPair.first
                allMethods := transferPair.second
                        

            } case { td : share.TypeDeclaration →
                //Now does nothing if given type declaration; might make this
                //raise an error as embedded types are disallowed.
            } else {
                if (debug3) then {
                        io.error.write"\n2617 ignored {stmt}"
                }
            }
    }

    return ot.setMethodTypePair (publicMethods, allMethods)
    
}
//Helper method for gatherTypesForMethods
//Handles the case where stmt is a share. Method
method methCase (meth, allMethods', publicMethods') -> ot.SetMethodTypePair is confidential{

    var allMethods := allMethods'
    var publicMethods := publicMethods'
    if (debug) then {
            io.error.write "\n1438 in method case"
    }
    def mType: MethodType = updateMethScope(meth)
    if (debug) then {
        io.error.write 
            "\n1440: found method type {mType}"
    }
    checkOverride(mType,allMethods,publicMethods,meth)

    // add new method to the collection of methods
    allMethods.add(mType)
    if (debug) then {
        io.error.write 
            "\n1158 Adding {mType} to allMethods"
        io.error.write 
            "\n1159 AllMethods: {allMethods}"
    }
    if(isPublic(meth)) then {
        publicMethods.add(mType)
    }

    // A method that is a Member has no parameter 
    // and is identical to a variable, so we also 
    // store it inside the variables scope
    if (isMember(mType)) then {
        scope.variables.at(mType.name) put(mType.retType)
    }

    return ot.setMethodTypePair(publicMethods, allMethods)

}

//Helper method for gatherTypesForMethods
//Handles the case where stmt is a share.Def | share.Var
//Create type of method giving access to def or var value
method defCase (defd, allMethods', publicMethods') -> ot.SetMethodTypePair is confidential{
    var allMethods := allMethods'
    var publicMethods := publicMethods'

    def mType: MethodType = aMethodType.fromNode(defd)
                                         with (emptyList[[String]])
    if (allMethods.contains(mType)) then {
        StaticTypingError.raise ("A var or def {mType} "
            ++ "on line {defd.line} may not override "++
            "an existing method from the superclass}")
                                with (mType)
    }
    allMethods.add(mType)
    if (debug) then {
        io.error.write "\n1177 AllMethods: {allMethods}"
    }
    //create method to access def/var if it is readable
    if(defd.isReadable) then {
        publicMethods.add(mType)
    }

    //update scope with reference to def/var
    scope.methods.at(mType.name) put(mType)
    scope.variables.at(mType.name) put(mType.retType)
    // add setter methods if variable in defd
    addSetterMethodFor(defd, publicMethods, allMethods)
    

    return ot.setMethodTypePair (publicMethods, allMethods)
    

}
// Construct setter method for variable declared in defd.  
// Add to allMethods, and, if writeable, to publicMethods.
method addSetterMethodFor(defd: share.Var | share.Def,
        publicMethods: Set⟦MethodType⟧, allMethods: Set⟦MethodType⟧)
                                    -> Done is confidential {
    if (defd.kind == "vardec") then {
        def name': String = defd.nameString ++ ":=" 
        // (1)"  ?? is name right?
        def dType: ObjectType = 
            anObjectType.fromDType (defd.dtype) with (emptyList)
        def param: Param = 
                aParam.withName(defd.nameString) ofType (dType)
        def sig: List⟦MixPart⟧ =
            list[ot.aMixPartWithName(name') parameters(list[param])]

        def aType: MethodType = 
            aMethodType.signature(sig) 
                            returnType(anObjectType.doneType)
        scope.methods.at (name') put (aType)
        allMethods.add (aType)
        if (debug) then {
           io.error.write "\n1197 AllMethods: {allMethods}"
        }

        //Add public setter method for writable vars
        if(defd.isWritable) then {
            publicMethods.add(aType)
        }
    }
}

// Make sure all statements in object body type-check
method typeCheckObjectBody(hasInherits: Boolean, body: List⟦AstNode⟧)
                                    -> Done is confidential {
    if (debug) then {
       io.error.write 
                "\n1980: hasInherits:{hasInherits}"   
    }                                   
                                 
    // Type-check the object body, dropping the inherit clause
    def indices: Collection⟦Number⟧ = if(hasInherits) then {
        2..body.size
    } else {
        body.indices
    }

    for(indices) do { i: Number →
        if (debug) then {
            io.error.write 
                "\n2070: checking index {i} at line {body.at(i).line}"
        }
        checkTypes(body.at(i))
        if (debug) then {
            io.error.write "\n2072: finished index {i}\n"
        }
    }
}

// If either allMethods or publicMethods contains a method with 
// the same name and number of parameters as mType then make 
// sure mType is a specialization,
// and remove the old one from allMethods and publicMethods
method checkOverride(mType: MethodType, allMethods: Set⟦MethodType⟧,
        publicMethods: Set⟦MethodType⟧, meth: AstNode) → Done
                                            is confidential {
    def oldMethType: MethodType = allMethods.find{m:MethodType →
        mType.nameString == m.nameString
    } ifNone {return}  
    // Nothing to be done if corresponding method not there
    if (debug) then {
        io.error.write 
            "\n1233 Found new method {mType} while old was {oldMethType}"
    }
    if (mType.isSpecialisationOf (oldMethType).ans.not) then {
        StaticTypingError.raise ("Type of overriding method {mType} "
            ++ "on line {meth.line} is not a specialization of " 
            ++ "existing method {oldMethType}") with (meth)
    }
    // remove the old method type
    allMethods.remove(oldMethType)
    // TODO: Should this be public only if declared so.  
    // Probably should be error to change from confidential to 
    // public rather than ignore as here.
    if (publicMethods.contains(oldMethType)) then {
        publicMethods.remove(oldMethType)
    }
}

def TypeDeclarationError = TypeError.refine "TypeDeclarationError"

// The first pass over a body, collecting all type declarations 
// so that they can reference one another declaratively.
method collectTypes(nodes : Collection⟦AstNode⟧) → Done 
                                    is confidential {
    def debug3 = false
    def typeDecs: List[[share.TypeDeclaration]] = emptyList[[share.TypeDeclaration]]

    if (debug3) then {
        io.error.write ("\n818st Types scope before "++
            "collecting types is {scope.types}")
    }

    // Collect all type declarations into typeDecs and insert their
    // left-hand side into scope with placholders
    for(nodes) do { node -> 
        if(node.kind == "typedec") then {

            if (debug3) then {
                io.error.write"\n1576: matched as typeDec {node}"
            }

            if(typeDecs.contains(node)) then {
                StaticTypingError.raise(
                    "The type {node.nameString} on line " ++
                    "{node.line} uses the same name as another "++
                    "type in the same scope") with (node)
            }

            typeDecs.push(node);

            // Put type placholders into the scope
            def typeName: String = node.nameString
            if(false ≠ node.typeParams) then {
                scope.generics.at(typeName) put (ot.aGenericType.placeholder)
            } else {
                scope.types.at(typeName) put (ot.anObjectType.placeholder)
            }
        }
    }

    if (debug3) then {
        io.error.write "\nGenerics scope is: {scope.generics}"
        io.error.write "\nType  scope is: {scope.types}"
    }

    // Update type placeholders in the scope to real types 
    for(typeDecs) do { typeDec →
        updateTypeScope(typeDec)
    }

    if (debug3) then {
        io.error.write ("\n823st Types scope after "++
            "collecting types is {scope.types}")
    }
}

// Determines if a node is publicly available.
method isPublic(node : share.Method | share.Def | share.Var)
                                → Boolean is confidential {
    match(node) case { _ : share.Method →
        for(node.annotations) do { ann →
            if(ann.value == "confidential") then {
                return false
            }
        }

        true
    } else {
        for(node.annotations) do { ann →
            if((ann.value == "public") || 
                    (ann.value == "readable")) then {
                return true
            }
        }

        false
    }
}
 

// Determines if a method will be accessed as a member.
method isMember (mType : MethodType) → Boolean is confidential {
    (mType.signature.size == 1) && {
        mType.signature.first.parameters.size == 0
    }
}


// Helper methods.

// For loop with break.
method for(a) doWithBreak(bl) → Done is confidential {
    for(a) do { e →
        bl.apply(e, {
            return
        })
    }
}

// For loop with continue.
method for(a) doWithContinue(bl) → Done is confidential{
    for(a) do { e →
        continue'(e, bl)
    }
}

// Used with for()doWithContinue
method continue'(e, bl) → Done is confidential {
    bl.apply(e, { return })
}


//class parameterFromNode (node) → Parameter is confidential {
//    inherit ast.identifierNode.new (node.name, node.dtype)
//    method kind { "parameter" }
//}

def thisDialect is public = object {
    method astChecker (moduleObj) { moduleObj.accept(astVisitor) }
}
