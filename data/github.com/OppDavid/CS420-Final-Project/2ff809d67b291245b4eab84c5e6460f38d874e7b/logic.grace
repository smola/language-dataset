import "utils" as util
import "equivalances" as equiv 
import "zip" as zip

factory method expression {
  method containedPredicates { 
    util.group (predicates) by { each -> "{each}" }
  }
  method states { 
    // All (true/false) states possible with predicates list
    // Example:
    // > def e = predicate('a') & predicate('b')
    // > e.states
    // [[true, true],[true, false], [false, true], [false, false]]
    buildTruthTableStates(self.containedPredicates.keys.size) 
  } 
  method truthValues { 
    // A list of all truth values for each predicate state
    // Example:
    // > (predicate('a') & predicate('b')).truthValues
    // [true, false, false, false]
    // > (predicate('a') | predicate('b')).truthValues
    // [true, true, true, false]

    var returnResults := list.empty() 
    def containedPredicatesIds = list.withAll(containedPredicates.keys)
    states.do { state ->
      (1..(state.size)).do { i ->
        def id = containedPredicatesIds.at(i)
        containedPredicates.at(id).do { each -> each.state := state.at(i) }
      }
      returnResults.addLast(self.evaluate)
    }
    returnResults
  }
  // These "is" functions substitute for type checking
  method isPredicate { abstract }
  method isUnaryOperator { abstract }
  method isBinaryOperator { abstract }

  method copy { abstract }

  method evaluate { abstract }
  method predicates { abstract }
  method not { notOperator(self) }  
  method and (other) { andOperator(self, other) }  
  method or (other) { orOperator(self, other) }  
  method implies (other) { impliesOperator(self, other) }  
  method iff (other) { iffOperator(self, other) }
  method &(other) { and(other) }
  method |(other) { or(other) }
  method =>(other) { implies(other) }
  method ≡(other) { iff(other) }
  method prefix~ { self.not }
    
  method ==(other) {
    if (self.asString == other.asString) then { return true }
    false
  }
    
  method !=(other) {
    (self == other).not
  }
    
  method isTautology {
    truthValues.filter(util.identity).asList.size == truthValues.size
  }
    
  method isContradiction {
    truthValues.filter(util.identity).asList.isEmpty
  }
    
  method isConditional {
    if (isTautology) then { return false }
    if (isContradiction) then { return false }
    true
  }
    
  method removeNots {
    // (~~a).removeNots -> a
    var returnExp := self.copy

    if (returnExp.isUnaryOperator) then {
      returnExp.operand := returnExp.operand.removeNots
      // Simplify pattern detected
      if (returnExp.isNotOperator) then {  
        if (returnExp.operand.isNotOperator) then {
          returnExp := returnExp.operand.operand
        }
      }
    } elseif (returnExp.isBinaryOperator) then {
      returnExp.operand1 := returnExp.operand1.removeNots
      returnExp.operand2 := returnExp.operand2.removeNots
    }

    returnExp
  }
    
  method removeImplies {
    // (a => b).removeImplies -> (~a | b)
    var returnExp := self.copy
    
    if (returnExp.isUnaryOperator) then {
      returnExp.operand := returnExp.operand.removeImplies
    } elseif (returnExp.isBinaryOperator) then {
      returnExp.operand1 := returnExp.operand1.removeImplies
      returnExp.operand2 := returnExp.operand2.removeImplies
      // Simplify pattern detected
      if (returnExp.isImpliesOperator) then {
        returnExp := orOperator(returnExp.operand1.not, returnExp.operand2)
      }
    }

    returnExp
  }
    
  method removeIff {
    // (a.iff(b)).removeIff -> ((a => b) & (b => a))  
    var returnExp := self.copy

    if (returnExp.isUnaryOperator) then {
      returnExp.operand := returnExp.operand.removeIff
    } elseif ( returnExp.isBinaryOperator ) then {
      returnExp.operand1 := returnExp.operand1.removeIff
      returnExp.operand2 := returnExp.operand2.removeIff
      // Simplify pattern detected
      if (returnExp.isIffOperator) then {
        returnExp := andOperator(impliesOperator(returnExp.operand1.copy, returnExp.operand2.copy),
                                 impliesOperator(returnExp.operand2, returnExp.operand1))
      }
    }

    returnExp
  }
    
  method distributeAndOverOr {
    // (a & (b | c)) -> ((a & b) | (b & c)) 
    var returnExp := self.copy

    if (returnExp.isUnaryOperator) then {
      returnExp.operand := returnExp.operand.distributeAndOverOr
    } elseif (returnExp.isBinaryOperator) then {
      returnExp.operand1 := returnExp.operand1.distributeAndOverOr
      returnExp.operand2 := returnExp.operand2.distributeAndOverOr
      if (returnExp.isAndOperator) then {
        if (returnExp.operand1.isOrOperator) then {
          returnExp := (returnExp.operand1.operand1.and(returnExp.operand2)).or(returnExp.operand1.operand2.and(returnExp.operand2))
        } elseif (returnExp.operand2.isOrOperator) then {
            returnExp := (returnExp.operand1.and(returnExp.operand2.operand1)).or(returnExp.operand1.and(returnExp.operand2.operand2))
        }
        returnExp.operand1 := returnExp.operand1.distributeAndOverOr
        returnExp.operand2 := returnExp.operand2.distributeAndOverOr
      }
    }

    returnExp
  }
    
  method distributeOrOverAnd {
    // (a | (b & c)) -> ((a | b) & (b | c)) 
    var returnExp := self.copy

    if (returnExp.isUnaryOperator) then {
      returnExp.operand := returnExp.operand.distributeOrOverAnd
    } elseif (returnExp.isBinaryOperator) then {
      returnExp.operand1 := returnExp.operand1.distributeOrOverAnd
      returnExp.operand2 := returnExp.operand2.distributeOrOverAnd          
      if (returnExp.isOrOperator) then {
        if (returnExp.operand1.isAndOperator) then {
          returnExp := (returnExp.operand1.operand1.or(returnExp.operand2)).and(returnExp.operand1.operand2.or(returnExp.operand2))
        } elseif (returnExp.operand2.isAndOperator) then {
          returnExp := (returnExp.operand1.or(returnExp.operand2.operand1)).and(returnExp.operand1.or(returnExp.operand2.operand2))
        }
        returnExp.operand1 := returnExp.operand1.distributeOrOverAnd
        returnExp.operand2 := returnExp.operand2.distributeOrOverAnd
      }
    }

    returnExp
  }
    
  method distributeNot {
    // ~(a & b) -> ( ~a | ~b )
    var returnExp := self.copy
    
    if (returnExp.isUnaryOperator) then {
      if ( returnExp.isNotOperator ) then {
        returnExp.operand := returnExp.operand.distributeNot
        if ( returnExp.operand.isAndOperator ) then {
          returnExp := (returnExp.operand.operand1.not).or(returnExp.operand.operand2.not)
          returnExp.operand1 := returnExp.operand1.distributeNot
          returnExp.operand2 := returnExp.operand2.distributeNot
        } elseif ( returnExp.operand.isOrOperator ) then {
          returnExp := (returnExp.operand.operand1.not).and(returnExp.operand.operand2.not)
          returnExp.operand1 := returnExp.operand1.distributeNot
          returnExp.operand2 := returnExp.operand2.distributeNot
        }
      }
    } elseif ( returnExp.isBinaryOperator ) then {
      returnExp.operand1 := returnExp.operand1.distributeNot
      returnExp.operand2 := returnExp.operand2.distributeNot
    }
    returnExp
  }
  
  method idempotent {
    // (A&A) -> A and (A|A) -> A
    var returnExp := self.copy
    if ( returnExp.isUnaryOperator ) then {
      returnExp.operand := returnExp.operand.idempotent
    } elseif ( returnExp.isBinaryOperator ) then {
      returnExp.operand1 := returnExp.operand1.idempotent
      returnExp.operand2 := returnExp.operand2.idempotent
      if ( returnExp.isAndOperator || returnExp.isOrOperator ) then {
        if ( (returnExp.operand1) == (returnExp.operand2) ) then {
          returnExp := returnExp.operand1
        }
      }
    }
    returnExp
  }
  
  method andComplimentation {
    // (A&~A) -> ℂ
    var returnExp := self.copy
    if ( returnExp.isAndOperator ) then {
      if ( returnExp.operand1.isNotOperator ) then {
        if ( (returnExp.operand1.operand) == (returnExp.operand2) ) then {
          returnExp := contradiction
        }
      } elseif ( returnExp.operand2.isNotOperator ) then {
        if ( (returnExp.operand1) == (returnExp.operand2.operand) ) then {
          returnExp := contradiction
        }
      }
    }
    returnExp
  }
  
  method orComplimentation {
    // (A|~A) -> 𝕋
    var returnExp := self.copy
    if ( returnExp.isOrOperator ) then {
      if ( returnExp.operand1.isNotOperator ) then {
        if ( (returnExp.operand1.operand) == (returnExp.operand2) ) then {
          returnExp := tautology
        }
      } elseif ( returnExp.operand2.isNotOperator ) then {
        if ( (returnExp.operand1) == (returnExp.operand2.operand) ) then {
          returnExp := tautology    
        }
      }
    }
    returnExp
  }
  
  method complimentation {
    // (A&~A) -> ℂ and (A|~A) -> 𝕋
    var returnExp := self.copy
    if ( returnExp.isUnaryOperator ) then {
      returnExp.operand := returnExp.operand.complimentation
    } elseif ( returnExp.isBinaryOperator ) then {
      returnExp.operand1 := returnExp.operand1.complimentation
      returnExp.operand2 := returnExp.operand2.complimentation
      returnExp := returnExp.andComplimentation
      returnExp := returnExp.orComplimentation
    }
    returnExp
  }
  
  method andTautologyIdentity {
    // (A&𝕋) -> A 
    var returnExp := self.copy
    if ( returnExp.isAndOperator ) then {
      if ( returnExp.operand1.isTautology ) then {
        returnExp := returnExp.operand2
      } elseif ( returnExp.operand2.isTautology ) then {
        returnExp := returnExp.operand1
      }
    }
    returnExp
  }
  
  method andContradictionIdentity {
    // (A&ℂ) -> ℂ
    var returnExp := self.copy
    if ( returnExp.isAndOperator ) then {
      if ( returnExp.operand1.isContradiction || returnExp.operand2.isContradiction ) then {
        returnExp := contradiction
      }
    }
    returnExp
  }
  
  method orTautologyIdentity {
    // (A|𝕋) -> 𝕋
    var returnExp := self.copy
    if ( returnExp.isOrOperator ) then {
      if ( returnExp.operand1.isTautology || returnExp.operand2.isTautology ) then {
        returnExp := tautology
      }
    }
    returnExp
  }
  
  method orContradictionIdentity {
    // (A|ℂ) -> A
    var returnExp := self.copy
    if ( returnExp.isOrOperator ) then {
      if ( returnExp.operand1.isContradiction ) then {
        returnExp := returnExp.operand2
      } elseif ( returnExp.operand2.isContradiction ) then {
        returnExp := returnExp.operand1
      }
    }
    returnExp
  }
  
  method identity {
    // (A&𝕋) -> A
    // (A&ℂ) -> ℂ
    // (A|𝕋) -> 𝕋
    // (A|ℂ) -> A
    var returnExp := self.copy
    if ( returnExp.isUnaryOperator ) then {
      returnExp.operand := returnExp.operand.identity
    } elseif ( returnExp.isBinaryOperator ) then {
      returnExp.operand1 := returnExp.operand1.identity
      returnExp.operand2 := returnExp.operand2.identity
      returnExp := returnExp.andTautologyIdentity
      returnExp := returnExp.andContradictionIdentity
      returnExp := returnExp.orTautologyIdentity
      returnExp := returnExp.orContradictionIdentity
    }
    returnExp
  }
    
  method removeImplications {
    // Removes all implications from expression
    var returnExp := self.copy
    returnExp := returnExp.removeIff
    returnExp := returnExp.removeImplies
    returnExp
  }
    
  method toCNF {
    var returnExp := self.copy
    returnExp := returnExp.removeImplications
    returnExp := returnExp.distributeOrOverAnd
    returnExp
  }
    
  method toDNF {
    var returnExp := self.copy
    returnExp := returnExp.removeImplications
    returnExp := returnExp.distributeAndOverOr
    returnExp
  }

  // A series of methods for any method traversing an expresion
  // to determine the expression type. A substitute for type checking
  method isNotOperator {
    if (self.isUnaryOperator) then {
      if (self.symbol == "~") then {
        return true
      }
    }
    false
  }
  
  method isAndOperator {
    if (self.isBinaryOperator) then {
      if (self.symbol == "&") then {
        return true
      }
    }
    false
  }
    
  method isOrOperator {
    if (self.isBinaryOperator) then {
      if (self.symbol == "|") then {
        return true
      }
    }
    false
  }
    
  method isImpliesOperator {
    if (self.isBinaryOperator) then {
      if (self.symbol == "=>") then {
        return true
      }
    }
    false
  }
    
  method isIffOperator {
    if (self.isBinaryOperator) then {
      if (self.symbol == "<=>") then {
        return true
      }
    }
    false
  }
}


method printTruthTable(exp) {
  // Prints a truth table for expression
  var output := ""
  var header := list.withAll(exp.containedPredicates.keys).fold { result, it -> 
                  "{result}{it} | "
               } startingWith ""
  header := " {header}{exp}\n" 
    
  (1..header.size).do { 
    header := "{header}-"
  }
   
  output := "{output}{header}\n"
  
  zip.together(list.with(exp.states, exp.truthValues)).map { each -> 
    def predicates = each.at(1)
    def conclusion = each.at(2)
    var conclusionSymbol
    if (conclusion) then { conclusionSymbol := "T" } else { conclusionSymbol := "F" }
    var row := predicates.map { x -> 
      var symbol
      if (x) then { symbol := "T" } else { symbol := "F" } 
      symbol 
    }.fold { result, it -> "{result} {it} |" } startingWith ""
    output := "{output}{row} {conclusionSymbol}\n" 
  }

  print(output)
}

method predicate(id) { predicate(id) withState (true) }
factory method predicate(id) withState (state') {
  inherits expression
  var state is public := state'
  method isPredicate { true }
  method isUnaryOperator { false }
  method isBinaryOperator { false }
  method asString { "{id}" }
  method evaluate { state }
  method predicates { list.with(self) }
  method hash { id.hash }
  method copy { predicate(id) }
}

factory method operator(symbol') { 
  inherits expression
  def symbol is public = symbol'
  method isPredicate { false }
}

factory method unaryOperator(operand', symbol') {
  inherits operator(symbol')
  var operand is public := operand'
  method isUnaryOperator { true }
  method isBinaryOperator { false }
  method predicates { operand.predicates }
  method asString { "{symbol}{operand}" }
}

factory method binaryOperator(operand1', operand2', symbol') {
  inherits operator(symbol')
  var operand1 is public := operand1'
  var operand2 is public := operand2'
  method isUnaryOperator { false }
  method isBinaryOperator { true }
  method predicates { 
    def newList = operand1.predicates.copy
    newList.addAll(operand2.predicates)
    newList.sortBy{ left, right ->
      if ("{left}" < "{right}") then {
        -1
      } elseif ("{left}" > "{right}") then {
        1
      } else {
        0
      }
    }
  }
  method asString { "({operand1}{symbol}{operand2})" }
}

factory method notOperator(operand') {
  inherits unaryOperator(operand', "~")
  method evaluate { equiv.not(operand.evaluate) }
  method copy { notOperator(operand.copy) }
}

factory method andOperator(operand1', operand2') {
  inherits binaryOperator(operand1', operand2', "&")
  method evaluate { equiv.and(operand1.evaluate, operand2.evaluate) }
  method copy { andOperator(operand1.copy, operand2.copy) }
}

factory method orOperator(operand1', operand2') {
  inherits binaryOperator(operand1', operand2', "|")
  method evaluate { equiv.or(operand1.evaluate, operand2.evaluate) }
  method copy { orOperator(operand1.copy, operand2.copy) }
}

factory method impliesOperator(operand1', operand2') {
  inherits binaryOperator(operand1', operand2', "=>")
  method evaluate { equiv.implies(operand1.evaluate, operand2.evaluate) }
  method copy { impliesOperator(operand1.copy, operand2.copy) }
}

factory method iffOperator(operand1', operand2') {
  inherits binaryOperator(operand1', operand2', "<=>")
  method evaluate { equiv.iff(operand1.evaluate, operand2.evaluate) }
  method copy { iffOperator(operand1.copy, operand2.copy) }
}

factory method tautology(*operands)  {
  inherits expression
  method isTautology { true } 
  method isContradiction { false }
  method isPredicate { false }
  method isUnaryOperator { false }
  method isBinaryOperator { false }
  method copy { tautology(operands) }
  method evaluate { true }
  method predicates { list.empty }
  method asString { "𝕋" }
}

factory method contradiction(*operands)  {
  inherits expression
  method isTautology { false }
  method isContradiction { true } 
  method isPredicate { false }
  method isUnaryOperator { false }
  method isBinaryOperator { false }
  method copy { contradiction(operands) }
  method evaluate { false }
  method predicates { list.empty }
  method asString { "ℂ" }
}

method buildTruthTableStates(numberOfPredicates) {
  // buildTruthTableStates(2) ->
  // [[T, T], [T, F], [F, T], [F, F]] 
  var states := list.with(list.with(true), list.with(false))
  for (1..(numberOfPredicates-1)) do { i -> 
    states := util.setCrossProduct(states, list.with(true, false))
  }
  states
}


