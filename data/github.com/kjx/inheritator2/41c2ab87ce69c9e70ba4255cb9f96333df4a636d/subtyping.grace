//typechcker module for inheritator2
//adapted from typed.grace by Tim Jones

//TODO MUST RIP OUT ASSUMPTIONS TO separate obejct

import "combinator-collections" as c
use c.abbreviations
import "errors" as errors
use errors.exports
import "utility" as utility
use utility.exports


method check (left) isSubtypeOf (right) {
  def leftObjectType = makeObjectType(left)
  def rightObjectType = makeObjectType(right)
  //print "checking: {left} isSubtypeOf {right}"
  leftObjectType.isSubtypeOf(rightObjectType)
}

method check (left) isTypeEquals (right) {
  def leftObjectType = makeObjectType(left)
  def rightObjectType = makeObjectType(right)
  //print "checking: {left} isTypeEquals {right}"
  //print "objectType: {leftObjectType} isTypeEquals {rightObjectType}"
  leftObjectType == (rightObjectType)
}


method makeObjectType(obj) {
  //print "MAKEOBJYEPE:{obj}"
  match (obj.kind)
    case { "ngUnknown" -> unknownObjectType } 
    case { "ngImplicitUnknown" -> unknownObjectType }
    case { "ngTypeType" -> obj.value }
    case { "ngInterface" -> objectType(obj) }
    case { "objectContext" -> objectConstructorType(obj,obj.body,obj.evilCtxt) }
}


//from Tim
type ObjectType = interface {
    methods -> Set[[MethodType]]
    methodNamed(name : String)
      ifAbsent[[T]](onAbsent : Action[[T]]) -> MethodType | T
    isUnknown -> Boolean
    isStructural -> Boolean
    isSubtypeOf(other : ObjectType) -> Boolean
    // Used for redispatch in isSubtypeOf(), and does not actually represent a
    // calculation of whether this type is a supertype of the given one.
    // b.reverseSubtypeOf(a) IMPLIES b.issubtypeOf(a )
    reverseSubtypeOf(other : ObjectType) -> Boolean
    // b.reverseNotSubtypeOf(a) IMPLIES NOT b.issubtypeOf(a)
    reverseNotSubtypeOf(other : ObjectType) -> Boolean
    |(other : ObjectType) -> ObjectType
    &(other : ObjectType) -> ObjectType
}



var otCount := 0 

class abstractObjectType {
   def methods is public = empty
   otCount := otCount + 1 
   def otID is public = otCount


   method asString {error "abstract!!!!!"}

   method methodNamed(name) ifAbsent (block)  {
        for (methods) do { meth ->
          if (meth.name == name) then {
            return meth
          }
        }
        block.apply
   }

   method reverseSubtypeOf(other : ObjectType) -> Boolean {
        def rv = isStructural && methods.isEmpty
        //print "ABST {self} {other}"
        //print "ABST REV {rv}"
        rv
   }
   method reverseNotSubtypeOf(other : ObjectType) -> Boolean {
        false  
   }

   method isSubtypeOf(oType : ObjectType) -> Boolean {
        if (self == oType) then {return true}
        // Let the given type have a say.
        oType.reverseSubtypeOf(self).orElse {
          oType.isStructural.andAlso {
            //print "sub: {self} {oType}"
            def rv = isSubtypeOf(oType) withAssumptions(dictionary)
            //print "SUB: {self} {oType} {rv}"
            rv
          }
        }
   }

   method isSubtypeOf(oType : ObjectType)
          withAssumptions(assumptions :
            MutableDictionary[[ObjectType, MutableSet[[ObjectType]] ]])
              -> Boolean {

      //print "ISTOWA {self} {oType}"

      if (oType.reverseSubtypeOf(self)) then {return true}
      if (oType.reverseNotSubtypeOf(self)) then {return false}

      if (oType.isUnknown) then {return true}

      //print "ISTOWA got this far"

      def against = assumptions.at(self) 
            ifAbsent { def newSet = list
                       assumptions.at(self) put(newSet)
                       newSet }
            
      if (against.contains(oType)) then {return true}

      against.add(oType)

      for (oType.methods) do { oMeth ->
        //print "METH {oMeth.name}"
        def sMeth = methodNamed(oMeth.name) ifAbsent { return false }

        if (sMeth.typeParameters.size != oMeth.typeParameters.size) 
          then {return false}

        def sParamTypes = sMeth.parametersObjectTypes
        def oParamTypes = oMeth.parametersObjectTypes

        assert {sParamTypes.size == oParamTypes.size} 
            because "two methods have the same name but different numbers of parameters"

        for (sParamTypes) and(oParamTypes) do { sParam, oParam ->
            if (!oParam.isSubtypeOf(sParam)
                withAssumptions(assumptions)) then {
              return false
            }
          }

        //print "METH RET {sMeth.returnObjectType} {oMeth.returnObjectType}"
        if (!sMeth.returnObjectType.isSubtypeOf(oMeth.returnObjectType)
                withAssumptions(assumptions)  )
          then { return false }
      }
      true
   }

   def isStructural : Boolean is public = false
   def isUnknown : Boolean is public = false

   def hashCache = cache { 
     var acc := 0 
     for (methods) do { m -> acc := (acc + m.name.hash) % (2 ^ 64) }
     acc
   }
   method hash { ^ hashCache }

   method !=(other) { !(self == other) }
   method ==(other) {
     // print "type=="
     // print "self: {self}"
     // print "self: {isUnknown} {isStructural} {hash}"
     // print "other: {other}"
     // print "other: {other.isUnknown} {other.isStructural} {other.hash}"
     if (isUnknown && other.isUnknown) then {return true}
     if (isStructural != other.isStructural) then {return false}
     if (hash != other.hash) then {return false}
     return (equalsOther(other))
   }
   method equalsOther(other) { other.equalsAbstractObjectType(self) }
   method equalsAbstractObjectType(other) { error "shouldn't happen" }   
   method equalsStructuralObjectType(other) { false }   
   method equalsSingletonObjectType(other) { false }
}




//build an object type from an interpreter level interface object
//(which will be the result of an interface construcctor in the source)
//

class objectType( ngInterface ) {
   inherit abstractObjectType


   method equalsOther(other) { 
     //print "ot=OTHER"
     other.equalsStructuralObjectType(self) }
   method equalsStructuralObjectType(other) {
     //print "ot.eSOT"
     //print "ctxt {ctxt.dbg}  other {other.ctxt.dbg} {ctxt == other.ctxt}" 
     //print "value {value} other {other.value}"
     //print "value {value.nodeID} other {other.value.nodeID} {(value == other.value)    }"
     //def rv = (ctxt == other.ctxt) && (value == other.value)    
     //def rv = (value == other.value)  

     ///print "removed assertion is evil"
     ///assert { (value.nodeID == other.value.nodeID) == (value == other.value) }

     def rv = (value == other.value)    
     //print "ot.eSOT rv = {rv}"
     rv
   }


   def ctxt is public = ngInterface.context  
   def value is public = ngInterface.value

   def methods is public =  //TODO rename as methodTypes sometime?
     for (ngInterface.value.signatures)
       map { sig -> methodType( sig, ctxt ) }

   def isStructural : Boolean is public = true
   def isUnknown : Boolean is public = false

   method asString { 
      match (methods.size)
        case { 0 -> return "interface \{\}"}
        case { 1 -> return "interface ot:{otID} value:{value.nodeID} \{ {methods.at(1)} \}" }
        case { _ -> }
      var rv := "interface ot:{otID} value:{value.nodeID} \{\n  "
      for (methods) do { meth -> 
        rv := rv ++ "{meth.name}" ++ "\n  "
      }
      rv := rv ++ "}"
      rv
   }
}


class objectConstructorType( ngObjectContext, body', ctxt' ) {
   inherit abstractObjectType

   //VERY VERY BROKEN. DOESN"T DEAL WITH INHERITANCE!!

   method reverseSubtypeOf(_) {print "FUCKWITT"}
   method reverseNotSubtypeOf(_) {print "FUCKWITT TOO"}

   method equalsOther(other) { 
     //print "ot=OTHER"
     other.equalsStructuralObjectType(self) }
   method equalsStructuralObjectType(other) {
     //print "ot.eSOT"
     //print "ctxt {ctxt.dbg}  other {other.ctxt.dbg} {ctxt == other.ctxt}" 
     //print "value {value} other {other.value}"
     //print "value {value.nodeID} other {other.value.nodeID} {(value == other.value)    }"
     //def rv = (ctxt == other.ctxt) && (value == other.value)    
     //def rv = (value == other.value)  

     assert { (value.nodeID == other.value.nodeID) == (value == other.value) }

     def rv = (value == other.value)    
     //print "ot.eSOT rv = {rv}"
     rv
   }


   def ctxt is public = ctxt'
   def value is public = body'

   def methods is public = list //TODO rename as methodTypes sometime?
     //the most broken of the broken shit...
     //for now: just dumps in  all methods /  no fields!!!

   for (value) do { node ->

         def MightBeMethodNode = interface {
            signature -> Signature
         }

         match (node)
           case { m : MightBeMethodNode ->
                    // print "DJT got method {m}"
                    methods.add( methodType( m.signature, ctxt ) ) }
           case { _ -> }

         //TODO other kinds of attributes in methods
   }

   def isStructural : Boolean is public = true
   def isUnknown : Boolean is public = false

   method asString { 
      match (methods.size)
        case { 0 -> return "objectCX \{\}"}
        case { 1 -> return "objectCX ot:{otID}  \{ {methods.at(1)} \}" }
        case { _ -> }
      var rv := "objectCX ot:{otID} \{\n  "
      for (methods) do { meth -> 
        rv := rv ++ "{meth}" ++ "\n  "
      }
      rv := rv ++ "}"
      rv
   }
}

class blockType( ngBlock, parameters, inferredReturnObjectType, ctxt' ) {
   inherit abstractObjectType

   //VERY VERY BROKEN. DOESN"T DEAL WITH INHERITANCE!!

   method reverseSubtypeOf(_) {print "FUCKWITT"}
   method reverseNotSubtypeOf(_) {print "FUCKWITT TOO"}

   method equalsOther(other) { 
     //print "ot=OTHER"
     other.equalsStructuralObjectType(self) }
   method equalsStructuralObjectType(other) {
     //print "ot.eSOT"
     //print "ctxt {ctxt.dbg}  other {other.ctxt.dbg} {ctxt == other.ctxt}" 
     //print "value {value} other {other.value}"
     //print "value {value.nodeID} other {other.value.nodeID} {(value == other.value)    }"
     //def rv = (ctxt == other.ctxt) && (value == other.value)    
     //def rv = (value == other.value)  

     assert { (value.nodeID == other.value.nodeID) == (value == other.value) }

     def rv = (value == other.value)    
     //print "ot.eSOT rv = {rv}"
     rv
   }

   def ctxt is public = ctxt'
   def value is public = ngBlock //????almost certainly wrong...

   def methods is public = list //TODO rename as methodTypes sometime?
     //the most broken of the broken shit...
     //for now: just dumps in  all methods /  no fields!!!


   // HEREHEREH

   def suffix = match (parameters.size)
           case { 0 -> "" }
           case { 1 -> "(_)" }
           case { 2 -> "(_,_)" }
           case { 3 -> "(_,_,_)" }
           case { 4 -> "(_,_,_,_)" }
           case { 5 -> "(_,_,_,_,_)" }
           case { _ -> error "CANT BE BOTHERED TO APPLY MORE VARARGS" }


   def pots = for (parameters) 
       map { p -> makeObjectType (p.typeAnnotation.eval(ctxt.withoutCreatio)) } 

   //KJX work backwards from here!
   methods.add( blockMethodType("apply" ++ suffix, pots, inferredReturnObjectType) )
   methods.add( blockMethodType("match" ++ suffix,
                                list(unknownObjectType) repeated(parameters.size),
                                booleanType) )

   def isStructural : Boolean is public = true
   def isUnknown : Boolean is public = false

   method asString { "blockCX [[{parameters.size}]] {methods}" }
}



class singletonObjectType {
  inherit abstractObjectType  
  method equalsOther(other) { other.equalsSingletonObjectType(self) }
  method equalsSingletonObjectType(other) {
         // print "TRUUUUUUMMMMMMMMMMMMP"
         // print (numberType.asString == numberType.asString)
         // print (asString)
         // print (other.asString)
         // def a = asString
         // def s = self.asString
         // def o = other.asString
         // print "SOT {a == s} {a == o}"
         asString == other.asString }

  //def hashCache = cache { asString.hash }
  method hash { 42 } 
}

def unknownObjectType is public = object {
  inherit singletonObjectType

  def methods is public = empty
  method isUnknown { true }  
  method isStructural { false }
  method isSubtypeOf(_ : ObjectType) -> Boolean { true }
  method isSubtypeOf(_ : ObjectType) withAssumptions(_)-> Boolean { true }
  method reverseSubtypeOf(_ : ObjectType) -> Boolean { true }
  method reverseNotSubtypeOf(_ : ObjectType) -> Boolean { false }
  method asString { "unknownObjectType" }
}


//TODO - almost all of the below types should go away
//I'm not sure why Tim started with them
//and then I kept on - kjx

def doneType is public = object { 
  inherit singletonObjectType
  
  def methods is public = empty
  method isUnknown { false }  
  method isStructural { false }
  method isSubtypeOf(other : ObjectType) -> Boolean { // Let other have a say.
        other.reverseSubtypeOf(self).orElse { self == other } }
  method reverseSubtypeOf(other : ObjectType) -> Boolean { self == other }
  method reverseNotSubtypeOf(other : ObjectType) -> Boolean { self != other }
  method asString { "doneObjectType" }
}


def numberType is public = object { 
  inherit singletonObjectType
  
  def methods is public = empty
  method isUnknown { false }  
  method isStructural { false }
  method isSubtypeOf(other : ObjectType) -> Boolean { // Let other have a say.
        //print "Number ISTO {other}"        
        other.isSupertypeOf(self).orElse { self == other } }
  method isSupertypeOf(other : ObjectType) -> Boolean { self == other }
  method asString { "numberType" }

  method reverseSubtypeOf(other : ObjectType) -> Boolean { self == other }
  method reverseNotSubtypeOf(other : ObjectType) -> Boolean { self != other }
}


def stringType is public = object { 
  inherit singletonObjectType
  
  def methods is public = empty
  method isUnknown { false }  
  method isStructural { false }
  method isSubtypeOf(other : ObjectType) -> Boolean { // Let other have a say.
        //print "String ISTO {other}"
        other.isSupertypeOf(self).orElse { self == other } }
  method isSupertypeOf(other : ObjectType) -> Boolean { self == other }
  method asString { "stringType" }

  method reverseSubtypeOf(other : ObjectType) -> Boolean { self == other }
  method reverseNotSubtypeOf(other : ObjectType) -> Boolean { self != other }
}



def booleanType is public = object { 
  inherit singletonObjectType
  
  def methods is public = empty
  method isUnknown { false }  
  method isStructural { false }
  method isSubtypeOf(other : ObjectType) -> Boolean { // Let other have a say.
        //print "String ISTO {other}"
        other.isSupertypeOf(self).orElse { self == other } }
  method isSupertypeOf(other : ObjectType) -> Boolean { self == other }
  method asString { "booleanType" }

  method reverseSubtypeOf(other : ObjectType) -> Boolean { self == other }
  method reverseNotSubtypeOf(other : ObjectType) -> Boolean { self != other }
}


//from tim, not sure this is right
type MethodType = interface {
    name -> String
    typeParamters -> Sequence[[Parameter]]
    parametersObjectTypes -> Sequence[[ObjectType]]
    returnObjectType -> ObjectType
    //isSpecialisationOf(other : MethodType) -> Boolean
    //isPublic -> Boolean
    //isPublic := (value : Boolean) -> Done
}

class methodType ( signatureNode, ctxt ) { 
   method name { signatureNode.name }
   //def returnObjectType is public = 
   //        makeObjectType(signatureNode.returnType.eval(ctxt.withoutCreatio))
   method returnObjectType { 
           makeObjectType(signatureNode.returnType.eval(ctxt.withoutCreatio)) }
   method typeParameters { signatureNode.typeParameters }
   //def parametersObjectTypes is public = 
   // for (signatureNode.parameters) 
   //   map { p -> makeObjectType (p.typeAnnotation.eval(ctxt.withoutCreatio)) }    
   method parametersObjectTypes {
     for (signatureNode.parameters) 
       map { p -> makeObjectType (p.typeAnnotation.eval(ctxt.withoutCreatio)) } }
   method asString { "method {name}  [[{typeParameters.size}]] ({parametersObjectTypes.size}) {returnObjectType}" }

}

class blockMethodType(name', parametersObjectTypes', returnObjectType') {
   method name { name' }
   method typeParameters { empty }
   method returnObjectType { returnObjectType' }
   method parametersObjectTypes { parametersObjectTypes' }
   method asString {"bM {name} [[{parametersObjectTypes.size}]]"}
}
