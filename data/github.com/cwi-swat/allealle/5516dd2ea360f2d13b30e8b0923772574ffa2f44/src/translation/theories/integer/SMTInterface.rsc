module translation::theories::integer::SMTInterface

import smtlogic::Ints;

import translation::SMTValueSyntax;

import List;
import String; 

str preamble(Sort::\int()) = "";  

str compileVariableDeclaration(<str name, Sort::\int()>) = "(declare-const <name> Int)";

@memo str compile(\int(int i))                          = "<i>"; 

@memo str compile(neg(Term e))                          = "(- <compile(e)>)"; 
@memo str compile(abs(Term e))                          = "(abs <compile(e)>)"; 

@memo str compile(addition(list[Term] terms))           = "(+ <for (t <- terms) {><compile(t)> <}>)";
@memo str compile(multiplication(list[Term] terms))     = "(* <for (t <- terms) {><compile(t)> <}>)";
@memo str compile(division(Term lhs, Term rhs))         = "(div <compile(lhs)> <compile(rhs)>)"; 
@memo str compile(modulo(Term lhs, Term rhs))           = "(mod <compile(lhs)> <compile(rhs)>)";

@memo str compile(lt(Term lhs, Term rhs))               = "(\< <compile(lhs)> <compile(rhs)>)";
@memo str compile(lte(Term lhs, Term rhs))              = "(\<= <compile(lhs)> <compile(rhs)>)";
@memo str compile(gt(Term lhs, Term rhs))               = "(\> <compile(lhs)> <compile(rhs)>)";
@memo str compile(gte(Term lhs, Term rhs))              = "(\>= <compile(lhs)> <compile(rhs)>)";

@memo
str compileWithoutIden(\int(int i))                      = "<i>"; 
@memo
str compileWithoutIden(neg(Term e))                      = "(- " + compileWithoutIden(e)+ ")"; 
@memo
str compileWithoutIden(abs(Term e))                      = "(abs " + compileWithoutIden(e) + ")"; 
@memo
str compileWithoutIden(addition(list[Term] terms)) {
   str clauses = "";
   for (t <- terms) {
    clauses += compileWithoutIden(t) + " ";
   }
   
  return "(+ " + intercalate(" ", [compileWithoutIden(t) | t <- terms]) + ")";
}
@memo
str compileWithoutIden(multiplication(list[Term] terms)){
   str clauses = "";
   for (t <- terms) {
    clauses += compileWithoutIden(t) + " ";
   }
   
  return "(* " + clauses + ")";
}
@memo
str compileWithoutIden(division(Term lhs, Term rhs))     = "(div " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")"; 
@memo
str compileWithoutIden(modulo(Term lhs, Term rhs))       = "(mod " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")";
@memo
str compileWithoutIden(lt(Term lhs, Term rhs))           = "(\< " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")";
@memo
str compileWithoutIden(lte(Term lhs, Term rhs))          = "(\<= " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")";
@memo
str compileWithoutIden(gt(Term lhs, Term rhs))           = "(\> " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")";
@memo
str compileWithoutIden(gte(Term lhs, Term rhs))          = "(\>= " + compileWithoutIden(lhs) + " " + compileWithoutIden(rhs) + ")";
@memo

Term getValue((SmtValue)`<Val v>`, <str _, Sort::\int()>) = lit(\int(toInt("<v>")));
Term getValue((SmtValue)`(- <Val v>)`, <str _, Sort::\int()>) = neg(lit(\int(toInt("<v>"))));
 
str negateVariable(str varName, lit(\int(int i))) = "(not (= <varName> <i>))";
str negateVariable(str varName, neg(lit(\int(int i)))) = "(not (= <varName> (- <i>)))";