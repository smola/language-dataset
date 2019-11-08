
class GrammarBuilderTest : Test
{
  
  Void testGeneric() {
    // any
    verifyGrammar("A <- .", GrammarImpl("A", ["A": E.any]))
    
    // terminals
    verifyGrammar("A <- 'a'", GrammarImpl("A", ["A": E.t("a")]))
    verifyGrammar("A <- \"a\"", GrammarImpl("A", ["A": E.t("a")]))
    verifyGrammar("A <- 'abc'", GrammarImpl("A", ["A": E.t("abc")]))
    verifyGrammar("A <- \"abc\"", GrammarImpl("A", ["A": E.t("abc")]))
    
    // classes
    verifyGrammar("A <- [a-z]", GrammarImpl("A", ["A": E.clazz(['a'..'z'])]))
    verifyGrammar("A <- [a]", GrammarImpl("A", ["A": E.clazz(['a'])]))
    verifyGrammar("A <- [ab]", GrammarImpl("A", ["A": E.clazz(['a', 'b'])]))
    verifyGrammar("A <- [bc-z]", GrammarImpl("A", ["A": E.clazz(['b', 'c'..'z'])]))
    
    // non-terminals
    verifyGrammar("A <- a", GrammarImpl("A", ["A": E.nt("a")]))
    
    // sequences
    verifyGrammar("A <- a b", GrammarImpl("A", ["A": E.seq(["#a", "#b"])]))
    verifyGrammar("A <- a 'b' c", GrammarImpl("A", ["A": E.seq(["#a", "b", "#c"])]))
    
    // choices
    verifyGrammar("A <- a / b", GrammarImpl("A", ["A": E.choice(["#a", "#b"])]))
    verifyGrammar("A <- a / 'b' / c", GrammarImpl("A", ["A": E.choice(["#a", "b", "#c"])]))
    
    // repetitions
    verifyGrammar("A <- a*", GrammarImpl("A", ["A": E.rep("#a")]))
    verifyGrammar("A <- 'a'*", GrammarImpl("A", ["A": E.rep("a")]))
    verifyGrammar("A <- a+", GrammarImpl("A", ["A": E.seq(["#a", E.rep("#a")])]))
    verifyGrammar("A <- 'a'+", GrammarImpl("A", ["A": E.seq(["a", E.rep("a")])]))
    
    // predicates
    verifyGrammar("A <- !a", GrammarImpl("A", ["A": E.not("#a")]))
    verifyGrammar("A <- &a", GrammarImpl("A", ["A": E.and("#a")]))
    verifyGrammar("A <- !'a'", GrammarImpl("A", ["A": E.not("a")]))
    verifyGrammar("A <- &'a'", GrammarImpl("A", ["A": E.and("a")]))
    
    // combinations
    verifyGrammar("A <- !B
                   B <- 'b' / 'c'", GrammarImpl("A", ["A": E.not("#B"), "B": E.choice(["b", "c"])]))
        
    // lazy repetitions
    verifyGrammar("A <- a*?b", GrammarImpl("A", ["A": E.seq([E.rep(E.seq([E.not("#b"), "#a"])),"#b"])]))
    verifyGrammar("A <- 'a'*?'b'", GrammarImpl("A", ["A": E.seq([E.rep(E.seq([E.not("b"), "a"])),"b"])]))
    
    
    // namespaces
    verifyGrammar("@Z A <- .", GrammarImpl("Z:A", ["Z:A": E.any], "Z", Str[,]))
    verifyGrammar("@Z A <- . U:B", GrammarImpl("Z:A", ["Z:A": E.seq([E.any, "#U:B"])], "Z", ["U"]))
    verifyGrammar("A <- .", GrammarImpl("A", ["A": E.any], ""))
    verifyGrammar("@A A <- a", GrammarImpl("A:A", ["A:A": E.nt("A:a")], "A"))

    // sparse blocks
    sparseGrammar := GrammarImpl("A", [
      "A": E.seq([E.rep(E.seq([E.not("#C"), E.choice(["#C", E.any])])), "#C"]), 
      "C": E.t("c")]
    )
    verifyGrammar("A <- {B} C\n B <- { C <- 'c' }", sparseGrammar)
    verifyGrammar("A <- {  B} C\n B <- { C <- 'c' }", sparseGrammar)
    verifyGrammar("A <- {B    } C\n B <- { C <- 'c' }", sparseGrammar)
  }
  
  Void testRefine() {
    verifyGrammar("A <- '\\n'", GrammarImpl("A", ["A": E.t("\n")]))
    verifyGrammar("A <- \"\\n\"", GrammarImpl("A", ["A": E.t("\n")]))
    verifyGrammar("A <- 'a\\nb\\tc'", GrammarImpl("A", ["A": E.t("a\nb\tc")]))
    
    verifyGrammar("A <- !'\\n'+", GrammarImpl("A", ["A": E.not(E.rep1("\n"))]))
    
    verifyGrammar("A <- [\\n]", GrammarImpl("A", ["A": E.clazz(['\n'])]))
    
    verifyGrammar("A <- '\\t'", GrammarImpl("A", ["A": E.t("\t")]))
    verifyGrammar("A <- '\\['", GrammarImpl("A", ["A": E.t("[")]))
    verifyGrammar("A <- '\\]'", GrammarImpl("A", ["A": E.t("]")]))
    verifyGrammar("A <- '\\\\'", GrammarImpl("A", ["A": E.t("\\")]))
    verifyGrammar("A <- '\\r'", GrammarImpl("A", ["A": E.t("\r")]))
    verifyGrammar("A <- '\\''", GrammarImpl("A", ["A": E.t("'")]))
    verifyGrammar("A <- '\\\"'", GrammarImpl("A", ["A": E.t("\"")]))
  }
  
  Void testInvalidGrammar() {
    verifyInvalidSyntax("A <- {B}\n")
    verifyInvalidSyntax("A <- {B} {C}\n")
  }
  
  private Void verifyGrammar(Str in, Grammar grammar) {
    lh := ListHandler()
    p := Parser(MetaGrammar.val, lh)
    p.run(in.toBuf)
    verifyEq(MatchState.success, p.match.state)
    verifyEq(GrammarBuilder.run(in, lh.blocks), grammar)
  }
  
  private Void verifyInvalidSyntax(Str in) {
    lh := ListHandler()
    p := Parser(MetaGrammar.val, lh)
    p.run(in.toBuf)
    verifyEq(MatchState.fail, p.match.state)
  }
}
