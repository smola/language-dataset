
//SUPER-EVIL GLOBAL VARIABLE Courtesy of MR CHARLES WEIR
var currentIndentation := 0 

// type Object = { }

////////////////////////////////////////////////////////////
type InputStream = { 
 take( _ ) -> String
 rest ( _ ) -> InputStream
 atEnd -> Boolean
 indentation -> Number
} 


class stringInputStream(string : String, position' : Number) {
 def brand = "stringInputStrea"
 def position : Number is readable = position'

 method take(n : Number) -> String {
   var result := ""
   var endPosition := position + n - 1
   if (endPosition > string.size) then {endPosition := string.size}
   for (position .. endPosition) do { i : Number ->   
       result := result ++ string.at(i)
   }
   return result
 }

 method rest(n : Number)  {

  if ((n + position) <= (string.size + 1))
   then {return stringInputStream(string, position + n)}
   else { 
     Error.raise("FATAL ERROR END OF INPUT at {position} take {n}")
     }
 }

 method atEnd  {return position > string.size}

 method indentation {
   var cursor := position - 1
   while {(cursor > 0).andAlso {string.at(cursor) != "\n"}}
     do {cursor := cursor - 1}
   // now cursor is the char before the first in the line.
   cursor := cursor + 1
   var result := 0
   while {((cursor + result) <= string.size).andAlso {string.at(cursor + result) == " "}}
     do {result := result + 1}

   if ((cursor + result) > string.size) then {return 0} // lots of spaces then end
  
   return result
 }

}

////////////////////////////////////////////////////////////
// parse results

type ParseSuccessType = {
  brand -> String
  next -> InputStream
  result -> String
  succeeded -> Boolean
  resultUnlessFailed( _ ) -> ParseResult 
}

type ParseFailureType = {
  brand -> String
  message -> String
  succeded -> Boolean
  resultUnlessFailed( _ ) -> ParseResult 
}

//type ParseResult = (ParseSuccessType | ParseFailureType)

type ParseResult = ParseFailureType

class parseSuccess(next', result') {
 def brand = "parseSuccess"
 def next is public  = next' 
 def result is public = result'
 method succeeded { return true }
 method resultUnlessFailed (failBlock : Block) {
   return self
 }
}

class parseFailure(message') {
 def brand = "parseFailure" 
 def message is public = message'
 method succeeded { return false }
 method resultUnlessFailed (failBlock : Block) { 
   return failBlock.apply(self)
 }
}

 
////////////////////////////////////////////////////////////
// parsers

class abstractParser {
 def brand = "abstractParser"
 method parse(in) { }

 method ~(other) {sequentialParser(self,other)}
 method |(other) {alternativeParser(self,other)}
}

type Parser = { 
  parse(_ : InputStream) -> ParseResult
  ~(_ : Parser) -> Parser
  |(_ : Parser) -> Parser
}

// parse just a token - basically a string, matching exactly
class tokenParser(tken) {
 inherit abstractParser
 def brand = "tokenParser"
 method parse(in) {
   def size = tken.size
   if (in.take(size) == tken) then {
      return parseSuccess(in.rest(size), "{in.take(size)}" )
     } else {
      return parseFailure(
        "expected {tken} got {in.take(size)} at {in.position}")
   }
 }
}

// get at least one whitespace
class whiteSpaceParser {
 inherit abstractParser 
 def brand = "whiteSpaceParser"
 method parse(in) {
   var current := in

   while {(current.take(1) == " ") || (current.take(2) == "\\")} do {
     while {current.take(1) == " "} 
       do {current := current.rest(1)}
     if (current.take(2) == "//")
       then {
         current := current.rest(2)
         while {current.take(1) != "\n"} 
           do {current := current.rest(1)}
         current := current.take(1)
       }
   }

   if (current != in) then {
      return parseSuccess(current, " ")
     } else {
      return parseFailure(
        "expected w/s got {in.take(5)} at {in.position}")
   }
 }
}


// parser single character from set of acceptable characters (given as a string)
class characterSetParser(charSet) {
 inherit abstractParser
 def brand = "characterSetParser"

 method parse(in) {
   def current = in.take(1) 
   
   for (charSet) do { c -> 
      if (c == current) then {
        return parseSuccess(in.rest(1), current ) }
     }

   return parseFailure(
        "expected \"{charSet}\" got {current} at {in.position}")
 }
}

//does *not* eat whitespace!
class graceIdentifierParser { 
 inherit abstractParser

 def brand = "graceIdentifierParser"

 method parse(in) {
   if (in.take(1) == "_") then {
      return parseSuccess(in.rest(1), "_")                 
   }
   var current := in

   if (! isletter(in.take(1))) then {
      return parseFailure(
        "expected GraceIdentifier got {in.take(5)}... at {in.position}")
   }
   
   var char := current.take(1)
   var id := ""

   // print "char: <{char}>  current.atEnd <{current.atEnd}>"

   while {(!current.atEnd).andAlso {isletter(char) || isdigit(char) || (char == "'")}}
     do {
        id := id ++ char
        current := current.rest(1)
        char := current.take(1)
        // print "chlr: <{char}>  current.atEnd <{current.atEnd}>"
   }

   return parseSuccess(current, id)
 }

}


// dunno why this is here?
class digitStringParser { 
 inherit abstractParser
 def brand = "digitStringParser"
 method parse(in) {
   
   var current := in

   var char := current.take(1)
   var id := ""

   if (char == "-") then {
     id := "-"
     current := in.rest(1)
     char := current.take(1)     
   }

   if (! (isdigit(char))) then {
      return parseFailure(
        "expected DigitString got {in.take(5)}... at {in.position}")
   }

   while {isdigit(char)}
     do {
        id := id ++ char
        current := current.rest(1)
        char := current.take(1)
   }

   return parseSuccess(current, id)
 }

}



class sequentialParser(left, right) { 
 inherit abstractParser
 def brand = "sequentialParser"
 method parse(in) {
    def leftResult = left.parse(in)
          .resultUnlessFailed {f -> return f}
    def rightResult = right.parse(leftResult.next)
          .resultUnlessFailed {f -> return f}
    return parseSuccess(rightResult.next, 
           leftResult.result ++ rightResult.result)
 }
}


class optionalParser(subParser) { 
 inherit abstractParser
 def brand = "optionalParser"
 method parse(in) {
    return (subParser.parse(in)
          .resultUnlessFailed {f -> 
               return parseSuccess(in, "")})
}

}

//match as if SubParser, discard the result
class dropParser(subParser) {
 inherit abstractParser
 def brand = "dropParser"
 method parse(in) {
    def subRes = subParser.parse(in)
          .resultUnlessFailed {f -> return f}
    return parseSuccess(subRes.next, "")
 }

}


class alternativeParser(left, right) {
 inherit abstractParser
 def brand = "alternativeParser"
 method parse(in) {
    def leftResult = left.parse(in)
    if (leftResult.succeeded) then {
      return leftResult }
    return right.parse(in)
 }

}


//succeeds if both left & right succeed; returns LEFT parse
//e.g. both(identifier,not(reservedIdentifiers)) -- except that's wrong!
class bothParser(left, right) {
 inherit abstractParser
 def brand = "bothParser"
 method parse(in) {
    def leftResult = left.parse(in)
    if (!leftResult.succeeded) then {return leftResult}
    def rightResult = right.parse(in)
    if (!rightResult.succeeded) then {return rightResult}
    return leftResult
 }

}



class repetitionParser(subParser) {
 inherit abstractParser
 def brand = "repetitionParser"
 method parse(in) {
   var current := in

   var res := subParser.parse(in)
   var id := ""

   while {res.succeeded}
     do {
        id := id ++ res.result
        current := res.next
        res := subParser.parse(current)
   }

   return parseSuccess(current, id)
 }

}



class proxyParser(proxyBlock) { 
 inherit abstractParser
 def brand = "proxyParser"
 var subParser := "no parser installed"
 var needToInitialiseSubParser := true

 method parse(in) {

  if (needToInitialiseSubParser) then {
    subParser := proxyBlock.apply
    needToInitialiseSubParser := false
  }

  def previousIndentation = currentIndentation
  currentIndentation := in.indentation

  var result 

  //  if (currentIndentation < previousIndentation) then {
  //     print ("??Bad Indentation?? at {in.position}, wanted {previousIndentation} got {currentIndentation}")
  //     result := parseFailure("Bad Indentation, wanted {previousIndentation} got {currentIndentation}")
  //  } else {
  result := subParser.parse(in)
  //  }

  currentIndentation := previousIndentation

  return result
 }

}



class wrappingProxyParser(proxyBlock, string) {
 inherit abstractParser
 def brand = "wrappingProxyParser"
 var subParser := "no parser installed"
 var needToInitialiseSubParser := true

 method parse(in) {

  if (needToInitialiseSubParser) then {
    subParser := proxyBlock.apply
    needToInitialiseSubParser := false
  }
   
  def result = subParser.parse(in)
  if (!result.succeeded) then {return result}
  
  return parseSuccess(result.next, "[{string}{result.result}]")
 }

}



// get at least one whitespace
class atEndParser { 
 inherit abstractParser
 def brand = "atEndParser"
 method parse(in) {
   if (in.atEnd) then {
      return parseSuccess(in, "")
     } else {
      return parseFailure(
        "expected end got {in.take(5)} at {in.position}")
   }
 }

}

// succeeds when subparser fails; never consumes input if succeeds
class notParser(subParser) {
 inherit abstractParser
 def brand = "notParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (result.succeeded)
      then {return parseFailure("Not Parser - subParser succeeded so I failed")}
      else {return parseSuccess(in,"")}
 }

}


class guardParser(subParser, guardBlock) {
 inherit abstractParser
 def brand = "guardParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (!result.succeeded) then {return result}
    if (guardBlock.apply(result.result)) then {return result}
    return  parseFailure("Guard failure at {in.position}")
 }

}


class successParser {
 inherit abstractParser
 def brand = "successParser"
 method parse(in) {return parseSuccess(in,"!!success!!")}

}


// puts tag into output
class tagParser(tagx : String) {
 inherit abstractParser
 def brand = "tagParser"
 method parse(in) {return parseSuccess(in, tagx)}

}

// puts tagx around start and end of parse
class phraseParser(tagx: String, subParser) {
 inherit abstractParser
 def brand = "phraseParser"
 method parse(in) {
    def result = subParser.parse(in)

    if (!result.succeeded) then {return result}

    return parseSuccess(result.next,
              "<" ++ tagx ++ " " ++ result.result ++ " " ++ tagx ++ ">" )
 }

}


class indentationAssertionParser(indent : Number) {
 inherit abstractParser
 def brand = "indentationAssertionParser"
 method parse(in) {
   if (in.indentation == indent) 
    then {return parseSuccess(in,"")}
    else { print  "***Asserted indent=={indent}, actual indentation=={in.indentation}"
           return parseFailure "Asserted indent=={indent}, actual indentation=={in.indentation}"}
 }
}


class lineBreakParser(direction) { 
 inherit abstractParser
 def brand = "lineBreakParser"
 method parse(in) {

  if (in.take(1) != "\n") 
    then {return parseFailure "looking for a LineBreak-{direction}, got \"{in.take(1)}\" at {in.position}"}

  def rest = in.rest(1) 

  def actualDirection = 
    if (rest.atEnd) then { "left" }
      elseif {rest.indentation <  currentIndentation} then { "left" }
      elseif {rest.indentation == currentIndentation} then { "same" }
      elseif {rest.indentation >  currentIndentation} then { "right" }
      else {Error.raise "Tricotomy failure"}
    

  match (actualDirection) 
     case { _ : direction -> return parseSuccess(in.rest(1), "<<{direction}>>\n" ) }
     case { _ -> return parseFailure "looking for a LineBreak-{direction}, got {actualDirection} at {in.position}"
     }

  Error.raise "Shouldn't happen"
 }
}



////////////////////////////////////////////////////////////
// "support library methods" 

method assert  (assertion : Block) complaint (name : String) {
 if (!assertion.apply) 
   then {print "ASSERTION FAILURE"}
}


method isletter(c) -> Boolean {
  // print "callxd isletter({c})"
  if (c.size == 0) then {return false} //is bad. is a hack. works.
  if (c.size != 1) then {print "isletter string {c} too long"}
  //  assert {c.size == 1} complaint "isletter string too long" 
  return (((c >= "A") && (c <= "Z")) 
         || ((c >= "a") && (c <= "z")))
}

method isdigit(c) -> Boolean {
  // print "callxd isdigit({c})"
  //  assert {c.size == 1} complaint "isdigit string too long" 
  if (c.size == 0) then {return false} //is bad. is a hack. works. 
  return ((c >= "0") && (c <= "9")) 
}

////////////////////////////////////////////////////////////
// combinator functions - many of these should be methods
// on parser but I got sick of copying everything!

method dyn(d : Unknown) -> Unknown {return d}


def ws = rep1((whiteSpaceParser) | lineBreak("right"))
method opt(p : Parser)  {optionalParser(p)}
method rep(p : Parser)  {repetitionParser(p)}
method rep1(p : Parser) {p ~ repetitionParser(p)}
method drop(p : Parser) {dropParser(p)}
method trim(p : Parser) {drop(opt(ws)) ~ p ~ drop(opt(ws))}
method token(s : String)  {tokenParser(s)}
//method symbol(s : String) {trim(token(s))}
method symbol(s : String) {token(s) ~ drop(opt(ws))} // changed to token with following space?
method rep1sep(p : Parser, q : Parser)  {p ~ rep(q ~ p)}
method repsep(p : Parser, q : Parser)  {opt( rep1sep(p,q))}
method repdel(p : Parser, q : Parser)  {repsep(p,q) ~ opt(q)}
method rule(proxyBlock : Block)  {proxyParser(proxyBlock)}
//method rule(proxyBlock : Block) wrap(s : String)  {wrappingProxyParser(proxyBlock,s)}
method rule(proxyBlock : Block) wrap(s : String)  {proxyParser(proxyBlock,s)}

def end = atEndParser
method not(p : Parser)  {notParser(p)}
method both(p : Parser, q : Parser)  {bothParser(p,q)}
method empty  {successParser} 
method guard(p : Parser, b : Block)  {guardParser(p, b)} 
method tag(s : String) {tagParser(s)}
method phrase(s : String, p : Parser) { phraseParser(s, p) }
method indentAssert(i : Number) {indentationAssertionParser(i) }

method lineBreak(direction) {lineBreakParser(direction)}

method parse (s : String) with (p : Parser)  {
 p.parse(stringInputStream(s,1)).succeeded
}

////////////////////////////////////////////////////////////
// "tests" 

print("start")

var passedTests := 0
var failedTests := 0

def printPassedTests = false

method test (block : Block, result : Object, comment : String) {
 def rv = block.apply
 if  (rv == result) 
   then {if  (printPassedTests) then {print  ("------: " ++ comment)} else {print "."}}
   else {if (!printPassedTests) then {print ""}
         print  ("FAILED: " ++ comment)}
}

method test(block : Block) expecting(result : Object) comment(comment : String) {
   test(block,result,comment)
}


// method test (block : Block, result : Object, comment : String) {
//  def rv = block.apply
//  if  (rv == result) 
//    then {print  ("------: " ++ comment)}
//    else {print  ("FAILED: " ++ comment)} 
// }

method test(parser : Parser) on(s : String) correctly(comment : String) {
 def res = parser.parse(stringInputStream(s,1))
 if (res.succeeded) 
   then {if (printPassedTests) then {print  ("------: " ++ comment ++ " " ++  res.result)}  else {print "."}}
   else {if (!printPassedTests) then {print ""}
         print  ("FAILED: " ++ comment ++ " " ++  s)} 
}

method test(parser : Parser) on(s : String) wrongly(comment : String) {
 def rv = parser.parse(stringInputStream(s,1)).succeeded
 if  (!rv) 
   then {if (printPassedTests) then {print  ("------: " ++ comment ++ " " ++  s)}  else {print "."}}
   else {if (!printPassedTests) then {print ""}
         print  ("FAILED: " ++ comment ++ " " ++  s)} 
}


method testProgramOn(s : String) correctly(comment : String) {
  test(program) on(s) correctly(comment)
}

method testProgramOn(s : String) wrongly(comment : String) {
  test(program) on(s) wrongly(comment)
}

def strm   = stringInputStream("Hello World",1)
def strm2  = stringInputStream("   Hello World",1)
def strmus = stringInputStream("_",1)
def strmab  = stringInputStream("abc4de'a123",1)
def strmas  = stringInputStream("a bcb", 1)
def strmnn  = stringInputStream("1234  ",1)
def strmnx  = stringInputStream("1234",1)
def strmxx  = stringInputStream("xxxxx",1)
def strmxc  = stringInputStream("xcxcxf",1)
def strmcx  = stringInputStream("xcxcf",1)
def strmx  = stringInputStream("xf",1)
def strmxcx  = stringInputStream("xcxf",1)

//index          123 45678 90123
//indent         111 22222 0000" 
def indentStx = " 11\n  22\nnone"

currentIndentation := 0
test { stringInputStream(indentStx, 1 ).indentation } expecting (1) comment "ix indent at start of line"
test { stringInputStream(indentStx, 3 ).indentation } expecting (1) comment "Indentation 2"
test { stringInputStream(indentStx, 4 ).indentation } expecting (1) comment "ix EOL belongs to previous line"
test { stringInputStream(indentStx, 5 ).indentation } expecting (2) comment "Indentation 5"
test { stringInputStream(indentStx, 10 ).indentation } expecting (0) comment "Indentation 10"
test { stringInputStream(indentStx, 7 ).indentation } expecting (2) comment "Indentation 1"
test { stringInputStream(indentStx, indentStx.size + 1 ).indentation } expecting (0) comment "Indentation Line end"
test { stringInputStream(indentStx, indentStx.size + 1 ).indentation } expecting (0) comment "Indentation Line end"
test { stringInputStream("print(1)", 3 ).indentation } expecting (0) comment "print(0)"

currentIndentation := 0
test { lineBreak("left").parse(stringInputStream(indentStx, 1)).succeeded } expecting (false) comment "cnl1"
test { lineBreak("left").parse(stringInputStream(indentStx, 2)).succeeded } expecting (false) comment "cnl2"
test { lineBreak("left").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4"
test { lineBreak("left").parse(stringInputStream(indentStx, 5)).succeeded } expecting (false) comment "cnl5"
test { lineBreak("left").parse(stringInputStream(indentStx, 7)).succeeded } expecting (false) comment "cnl7"
test { lineBreak("left").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9"
test { lineBreak("left").parse(stringInputStream(indentStx, 10)).succeeded } expecting (false) comment "cnl10"

currentIndentation := 1
test { lineBreak("left").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4-1"
test { lineBreak("left").parse(stringInputStream(indentStx, 9)).succeeded } expecting (true) comment "cnl9-1"

currentIndentation := 2
test { lineBreak("left").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4-2"
test { lineBreak("left").parse(stringInputStream(indentStx, 9)).succeeded } expecting (true) comment "cnl9-2"

currentIndentation := 0
test { lineBreak("right").parse(stringInputStream(indentStx, 4)).succeeded } expecting (true) comment "cnl4-3"
test { lineBreak("right").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9-3"

currentIndentation := 1
test { lineBreak("right").parse(stringInputStream(indentStx, 4)).succeeded } expecting (true) comment "cnl4-4"
test { lineBreak("right").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9-4"

currentIndentation := 2
test { lineBreak("right").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4-5"
test { lineBreak("right").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9-5"


currentIndentation := 0
test { lineBreak("same").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4-6"
test { lineBreak("same").parse(stringInputStream(indentStx, 9)).succeeded } expecting (true) comment "cnl9-6"

currentIndentation := 1
test { lineBreak("same").parse(stringInputStream(indentStx, 4)).succeeded } expecting (false) comment "cnl4-7"
test { lineBreak("same").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9-7"

currentIndentation := 2
test { lineBreak("same").parse(stringInputStream(indentStx, 4)).succeeded } expecting (true) comment "cnl4-8"
test { lineBreak("same").parse(stringInputStream(indentStx, 9)).succeeded } expecting (false) comment "cnl9-8"

currentIndentation := 0




    
def hello = (tokenParser("Hello"))
def dsp   = digitStringParser
def ini   = sequentialParser(
                graceIdentifierParser,
                sequentialParser(
                        whiteSpaceParser,
                        graceIdentifierParser))
def ini2  = (graceIdentifierParser) ~ 
                        (whiteSpaceParser) ~
                                                graceIdentifierParser
def alt   = alternativeParser(hello,dsp)
def alt2  = hello | dsp
def rpx   = repetitionParser(tokenParser("x"))
def rpx2  = rep(tokenParser("x"))
def rpx1  = rep1(tokenParser("x"))
def rs    = repsep(tokenParser("x"),tokenParser("c"))
def r1s   = rep1sep(tokenParser("x"),tokenParser("c"))
def rd    = repdel(tokenParser("x"),tokenParser("c"))
//////////////////////////////////////////////////
// test!

test {strm.take(5)} 
    expecting "Hello" 
    comment "strm.take(5)"
test {strm.rest(6).take(5)} 
    expecting "World" 
    comment "strm.rest(6).take(5)"
test {tokenParser("Hello").parse(strm).succeeded}
    expecting(true)
    comment "tokenParser(\"Hello\")"
test {tokenParser("Hellx").parse(strm).succeeded}
    expecting(false)
    comment "tokenParser(\"Hellx\")"
test {whiteSpaceParser.parse(strm).succeeded}
    expecting(false)
    comment "whiteSpaceParser"
test {whiteSpaceParser.parse(strm2).succeeded}
    expecting(true)
    comment "whiteSpaceParser"
test {whiteSpaceParser.parse(strm2).next.position}
    expecting(4)
    comment "whiteSpaceParser - eating 4"
test {isletter "A"} expecting (true) comment "isletter A"
test {isletter "F"} expecting (true) comment "isletter F"
test {isletter "Z"} expecting (true) comment "isletter Z"
test {isletter "a"} expecting (true) comment "isletter a"
test {isletter "f"} expecting (true) comment "isletter f"
test {isletter "z"} expecting (true) comment "isletter z"
test {isletter "$"} expecting (false) comment "isletter $"
test {isletter "0"} expecting (false) comment "isletter 0"
test {isletter "1"} expecting (false) comment "isletter 1"
test {isletter "9"} expecting (false) comment "isletter 9"
test {isdigit "A"} expecting (false) comment "isdigit A"
test {isdigit "F"} expecting (false) comment "isdigit F"
test {isdigit "Z"} expecting (false) comment "isdigit A"
test {isdigit "a"} expecting (false) comment "isdigit a"
test {isdigit "f"} expecting (false) comment "isdigit f"
test {isdigit "z"} expecting (false) comment "isdigit z"
test {isdigit "$"} expecting (false) comment "isdigit $"
test {isdigit "0"} expecting (true) comment "isdigit 0"
test {isdigit "1"} expecting (true) comment "isdigit 1"
test {isdigit "9"} expecting (true) comment "isdigit 9"
test {whiteSpaceParser.parse(strm2).next.position}
    expecting(4)
    comment "whiteSpaceParser - eating 4"
test {graceIdentifierParser.parse(strmus).next.position}
    expecting(2)
    comment "graceIdentifierParser  us - eating 2"
test {graceIdentifierParser.parse(strmus).succeeded}
    expecting(true)
    comment "graceIdentifierParser us OK"
test {graceIdentifierParser.parse(strmus).result}
    expecting("_")
    comment "graceIdentifierParser. us _"
test {graceIdentifierParser.parse(strmab).next.position}
    expecting(12)
    comment "graceIdentifierParser ab12 "
test {graceIdentifierParser.parse(strmab).succeeded}
    expecting(true)
    comment "graceIdentifierParser ab OK"
test {graceIdentifierParser.parse(strmab).result}
    expecting("abc4de'a123")
    comment "graceIdentifierParser.ab - eating 2"
test {graceIdentifierParser.parse(strmas).next.position}
    expecting(2)
    comment "graceIdentifierParser as pos"
test {graceIdentifierParser.parse(strmas).succeeded}
    expecting(true)
    comment "graceIdentifierParser as"
test {graceIdentifierParser.parse(strmas).result}
    expecting("a")
    comment "graceIdentifierParser as OK"
test {graceIdentifierParser.parse(strmnn).succeeded}
    expecting(false)
    comment "graceIdentifierParser nn - eating 1"
test {digitStringParser.parse(strmnn).next.position}
    expecting(5)
    comment "digitStringParser as pos"
test {digitStringParser.parse(strmnn).succeeded}
    expecting(true)
    comment "digitStringParser as"
test {digitStringParser.parse(strmnn).result}
    expecting("1234")
    comment "digitStringParser as OK"
test {digitStringParser.parse(strmnx).next.position}
    expecting(5)
    comment "digitStringParser as pos"
test {digitStringParser.parse(strmnx).succeeded}
    expecting(true)
    comment "digitStringParser as"
test {digitStringParser.parse(strmnx).result}
    expecting("1234")
    comment "digitStringParser as OK"
test {sequentialParser(ws,hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {sequentialParser(ws,hello).parse(strm).succeeded}
    expecting(false)
    comment "sequentialParser strm OK"    
test {sequentialParser(ws,hello).parse(strmab).succeeded}
    expecting(false)
    comment "sequentialParser strm3 OK"    
test {ini.parse(strmas).succeeded}
    expecting(true)
    comment "sequentialParser ini OK"    
test {ini.parse(strmas).result}
    expecting("a bcb")
    comment "sequentialParser a bcb OK"    
test {sequentialParser(ws,hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {(ws ~ hello).parse(strm2).succeeded}
    expecting(true)
    comment "sequentialParser strm2 OK"    
test {ini2.parse(strmas).succeeded}
    expecting(true)
    comment "sequentialParser ini2 OK"    
test {ini2.parse(strmas).result}
    expecting("a bcb")
    comment "sequentialParser a bcb2 OK"    
test {opt(hello).parse(strm).succeeded}
    expecting(true)
    comment "optionalParser opt(hello) OK"    
test {opt(hello).parse(strmab).succeeded}
    expecting(true)
    comment "optionalParser opt(hello) abOK"    
test {alt.parse(strm).succeeded}
    expecting(true)
    comment "alt Hello OK"    
test {alt.parse(strmnn).succeeded}
    expecting(true)
    comment "alt nn OK"    
test {alt2.parse(strm).succeeded}
    expecting(true)
    comment "alt2 Hello OK"    
test {alt2.parse(strmnn).succeeded}
    expecting(true)
    comment "alt2 nn OK"
test {rpx.parse(strm).succeeded}
    expecting(true)
    comment "rpx Hello OK"    
test {rpx.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx xx OK"    
test {rpx.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx xxxxx OK"    
test {rpx2.parse(strm).succeeded}
    expecting(true)
    comment "rpx2 Hello OK"    
test {rpx2.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx2 xx OK"    
test {rpx2.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx2 xxxxx OK"    
test {rpx1.parse(strm).succeeded}
    expecting(false)
    comment "rpx1 Hello OK"    
test {rpx1.parse(strmxx).succeeded}
    expecting(true)
    comment "rpx1 xx OK"    
test {rpx1.parse(strmxx).result}
    expecting("xxxxx")
    comment "rpx1 xxxxx OK"    
test {rpx1.parse(strmxx).next.atEnd}
    expecting(true)
    comment "rpx1 atEnd OK"    
test {dropParser(hello).parse(strm).succeeded}
    expecting(true)
    comment "dropParser(\"Hello\")"
test {dropParser(hello).parse(strm).result}
    expecting("")
    comment "dropParser(\"Hello\") result"
test {dropParser(tokenParser("Hellx")).parse(strm).succeeded}
    expecting(false)
    comment "dropParser(tokenParser(\"Hellx\"))"
test {drop(hello).parse(strm).result}
    expecting("")
    comment "drop(hello) result"
test {trim(hello).parse(strm2).succeeded}
     expecting(true)
     comment "trim(hello) result"
test {trim(hello).parse(strm2).next.position}
     expecting(10) 
     comment "trim(hello) next"
test {trim(symbol("Hello")).parse(strm2).result}
     expecting("Hello")
     comment "trim(symbol(hello)) (not taking trailing space)"
test {rs.parse(strmxc).succeeded}
     expecting(true)
     comment "rs xc"
test {rs.parse(strmxc).next.position}
     expecting(6)
     comment "rs xc p   "
test {rs.parse(strmnn).succeeded}
     expecting(true)
     comment "rs nn"
test {rs.parse(strmnn).next.position}
     expecting(1)
     comment "rs nn p"
test {rs.parse(strmxcx).succeeded}
     expecting(true)
     comment "rs xcx"
test {rs.parse(strmxcx).next.position}
     expecting(4)
     comment "rs xcx p"
test {r1s.parse(strmx).succeeded}
     expecting(true)
     comment "r1s x f"
test {r1s.parse(strmx).next.position}
     expecting(2)
     comment "r1s x f"
test {r1s.parse(strmxc).succeeded}
     expecting(true)
     comment "r1s xc"
test {r1s.parse(strmxc).next.position}
     expecting(6)
     comment "r1s xc p"
test {r1s.parse(strmx).succeeded}
     expecting(true)
     comment "r1s x f"
test {r1s.parse(strmx).next.position}
     expecting(2)
     comment "r1s x f"
test {r1s.parse(strmnn).succeeded}
     expecting(false)
     comment "r1s nn"
test {rd.parse(strmxc).succeeded}
     expecting(true)
     comment "rd xc"
test {rd.parse(strmxc).next.position}
     expecting(6)
     comment "rd xc p   "
test {rd.parse(strmnn).succeeded}
     expecting(true)
     comment "rd nn"
test {rd.parse(strmnn).next.position}
     expecting(1)
     comment "rd nn p"
test {rd.parse(strmcx).succeeded}
     expecting(true)
     comment "rd cx"
test {rd.parse(strmcx).next.position}
     expecting(5)
     comment "rd cx p   "
test {rs.parse(strmcx).succeeded}
     expecting(true)
     comment "rs cx"
test {rs.parse(strmcx).next.position}
     expecting(4)
     comment "rs cx p   "
test {rule {tokenParser("Hello")}.parse(strm).succeeded}
    expecting(true)
    comment "rule tokenParser(\"Hello\")"
test {rule {tokenParser("Hellx")}.parse(strm).succeeded}
    expecting(false)
    comment "rule tokenParser(\"Hellx\")"
test {atEndParser.parse(rpx1.parse(strmxx).next).succeeded}
    expecting(true)
    comment "atEnd OK"    
test {atEndParser.parse(strmxx).succeeded}
    expecting(false)
    comment "not atEnd OK"    
test {characterSetParser("Helo Wrd").parse(strm).succeeded}
    expecting(true)
    comment "CSP OK"    
test {rep(characterSetParser("Helo Wrd")).parse(strm).next.position}
    expecting(12)
    comment "CSP next OK"    
test (not(hello)) on "Hello" wrongly "not(hello)"
test (not(hello)) on "Bood" correctly "not(hello)"
test (not(not(hello))) on "Hello" correctly "not(not(hello)) Hello"
test (both(hello,dsp)) on "Hello" wrongly "both1"
test (both(hello,hello)) on "Hello" correctly "both2"
test (both(hello,not(dsp))) on "Hello" correctly "both3"
test (empty) on "Hello" correctly "empty1"
test (empty) on "12345" correctly "empty2"
test (empty) on "" correctly "empty3"
test (empty ~ hello) on "Hello" correctly "e~h"
test (hello ~ empty) on "Hello" correctly "h~e"
test (hello | empty) on "Hello" correctly "h|e H"
test (empty | hello) on "Hello" correctly "e|h H"
test (hello | empty) on "  " correctly "h|e ws"
test (empty | hello) on "  " correctly "h|e ws"
test (guard(dsp,{ s -> true})) on "1234" correctly "guard t"
test (guard(dsp,{ s -> false})) on "1234" wrongly "guard f"
test (guard(dsp, { s -> s == "1234" } )) on "1234" correctly "guard 1234"
test (guard(dsp, { s -> s == "1234" } )) on "1235" wrongly "guard f"

 
print "------: done combinator tests"

//////////////////////////////////////////////////
// Grace Parser

//BEGINGRAMMAR
// top level
def program = rule {codeSequence ~ rep(ws) ~ end}
def codeSequence = rule { repdel((declaration | statement | empty), semicolon) }
def innerCodeSequence = rule { repdel((innerDeclaration | statement | empty), semicolon) }

// def comment = 

// declarations

def declaration = rule {
      varDeclaration | defDeclaration | classDeclaration |
        typeDeclaration | methodDeclaration }

def innerDeclaration = rule { 
       varDeclaration | defDeclaration | classDeclaration | typeDeclaration }

def varDeclaration = rule { 
        varId ~ identifier ~  opt(colon ~ typeExpression) ~ opt(assign ~ expression) }

def defDeclaration = rule { 
        defId ~ identifier ~  opt(colon ~ typeExpression) ~ equals ~ expression }

def methodDeclaration = rule {
        methodId ~ methodHeader ~ methodReturnType ~ whereClause ~
                           lBrace ~ innerCodeSequence ~ rBrace }
def classDeclaration = rule {
        classId ~ identifier ~ dot ~ classHeader ~ methodReturnType ~ whereClause ~ 
                               lBrace ~ inheritClause ~ codeSequence ~ rBrace }

//def oldClassDeclaration = rule { classId ~ identifier ~ lBrace ~ 
//                             opt(genericFormals ~ blockFormals ~ arrow) ~ codeSequence ~ rBrace }


//warning: order here is significant!
def methodHeader = rule { accessingAssignmentMethodHeader | accessingMethodHeader | assignmentMethodHeader | methodWithArgsHeader | unaryMethodHeader | operatorMethodHeader | prefixMethodHeader  } 

def classHeader = rule { methodWithArgsHeader | unaryMethodHeader }
def inheritClause = rule { opt( inheritId ~ expression ~ semicolon ) }  

def unaryMethodHeader = rule { identifier ~ genericFormals } 
def methodWithArgsHeader = rule { firstArgumentHeader ~ repsep(argumentHeader,opt(ws)) }
def firstArgumentHeader = rule { identifier ~ genericFormals ~ methodFormals }
def argumentHeader = rule { identifier ~ methodFormals }
def operatorMethodHeader = rule { otherOp ~ oneMethodFormal } 
def prefixMethodHeader = rule { opt(ws) ~ token("prefix") ~ otherOp }  // forbid space after prefix?
def assignmentMethodHeader = rule { identifier ~ assign ~ oneMethodFormal }
def accessingMethodHeader = rule { lrBrack ~ genericFormals ~ methodFormals }
def accessingAssignmentMethodHeader = rule { lrBrack ~ assign ~ genericFormals ~ methodFormals }

def methodReturnType = rule { opt(arrow ~ nonEmptyTypeExpression )  } 

def methodFormals = rule { lParen ~ rep1sep( identifier ~ opt(colon ~ opt(ws) ~ typeExpression), comma) ~ rParen}
def oneMethodFormal = rule { lParen ~ identifier ~ opt(colon ~ typeExpression) ~ rParen}
def blockFormals = rule { repsep( identifier ~ opt(colon ~ typeExpression), comma) }

def matchBinding = rule{ (identifier | literal | parenExpression) ~
                           opt(colon ~ nonEmptyTypeExpression ~ opt(matchingBlockTail)) }

def matchingBlockTail = rule { lParen ~ rep1sep(matchBinding, comma)  ~ rParen }

def typeDeclaration = rule {
        typeId ~ identifier ~ genericFormals ~
            equals ~ nonEmptyTypeExpression ~ (semicolon | whereClause)}

//these are the things that work - 24 July with EELCO
def typeExpression = rule { (opt(ws) ~ typeOpExpression ~ opt(ws)) | opt(ws) }
def nonEmptyTypeExpression = rule { opt(ws) ~ typeOpExpression ~ opt(ws) }

//these definitely don't - 24 July with EELCO
// def typeExpression = rule { (opt(ws) ~ expression ~ opt(ws)) | opt(ws) }
//def nonEmptyTypeExpression = rule { opt(ws) ~ expression ~ opt(ws) }

def typeOp = rule { opsymbol("|") | opsymbol("&") | opsymbol("+") } 

// def typeOpExpression = rule { rep1sep(basicTypeExpression, typeOp) }

// this complex rule ensures two different typeOps have no precedence
def typeOpExpression = rule {  
  var otherOperator 
  basicTypeExpression ~ opt(ws) ~
    opt( guard(typeOp, { s -> otherOperator:= s;
                              true })
          ~ rep1sep(basicTypeExpression ~ opt(ws),
             guard(typeOp, { s -> s == otherOperator })
        )
    )
  } 

def basicTypeExpression = rule { nakedTypeLiteral | literal | pathTypeExpression | parenTypeExpression }  
   // if we keep this, note that in a typeExpression context { a; } is  interpreted as type { a; }
   // otherwise as the block { a; }

def pathTypeExpression = rule { opt(superId ~ dot) ~ rep1sep((identifier ~ genericActuals),dot) }

def parenTypeExpression = rule { lParen ~ typeExpression ~ rParen } 



// statements

def statement = rule { returnStatement | (expression ~ opt(assignmentTail))  } 
    // do we need constraints here on which expressions can have an assignmentTail
    // could try to rewrite as options including (expression ~ arrayAccess ~ assignmentTail)
    // expression ~ dot ~ identifier ~ assignmentTail 
     
def returnStatement = rule { returnId ~ opt(expression) }  //doesn't need parens
def assignmentTail = rule { assign ~ expression }

// expressions

def expression = rule { opExpression } 

//def opExpression = rule { rep1sep(addExpression, otherOp)}

// this complex rule ensures two different otherOps have no precedence
def opExpression = rule { 
  var otherOperator 
  addExpression ~ opt(ws) ~
    opt( guard(otherOp, { s -> otherOperator:= s;
                               true }) ~ rep1sep(addExpression ~ opt(ws),
           guard(otherOp, { s -> s == otherOperator })
        )
    )
  } 

def addExpression = rule { rep1sep(multExpression, addOp) }
def multExpression = rule { rep1sep(prefixExpression, multOp) }
def prefixExpression = rule { (rep(otherOp) ~ selectorExpression) | (rep1(otherOp) ~ superId) } 
      // we can have !super 

def selectorExpression = rule { primaryExpression ~ rep(selector) }

def selector = rule { 
                      (dot ~ unaryRequest) | 
                        (dot ~ requestWithArgs) |
                        (lBrack ~ rep1sep(expression,comma) ~ rBrack)  
                    }

def operatorChar = characterSetParser("!?@#$%^&|~=+-*/><:.") // had to be moved up

//special symbol for operators: cannot be followed by another operatorChar
method opsymbol(s : String) {trim(token(s) ~ not(operatorChar))}

def multOp = opsymbol "*" | opsymbol "/" 
def addOp = opsymbol "+" | opsymbol "-" 
def otherOp = rule { guard(trim(rep1(operatorChar)), { s -> ! parse(s) with( reservedOp ~ end ) })} 
    // encompasses multOp and addOp
def operator = rule { otherOp | reservedOp }  

def unaryRequest = rule { trim(identifier) ~ genericActuals ~ not(delimitedArgument) } 
def requestWithArgs = rule { firstRequestArgumentClause ~ repsep(requestArgumentClause,opt(ws)) }
def firstRequestArgumentClause = rule { identifier ~ genericActuals ~ opt(ws) ~ delimitedArgument }
def requestArgumentClause = rule { identifier ~ opt(ws) ~ delimitedArgument }
def delimitedArgument = rule { argumentsInParens | blockLiteral | stringLiteral }
def argumentsInParens = rule { lParen ~ rep1sep(drop(opt(ws)) ~ expression, comma) ~ rParen  }  

def implicitSelfRequest = rule { requestWithArgs |  rep1sep(unaryRequest,dot) }

def primaryExpression = rule { literal | nonNakedSuper | implicitSelfRequest | parenExpression }  

def parenExpression = rule { lParen ~ rep1sep(drop(opt(ws)) ~ expression, semicolon) ~ rParen } 
                                      // TODO should parenExpression be around a codeSequence?

def nonNakedSuper = rule { superId ~ not(not( operator | lBrack )) }

// "generics" 
def genericActuals = rule { 
                            opt(lGeneric ~ opt(ws)
                             ~ rep1sep(opt(ws) ~ typeExpression ~ opt(ws),opt(ws) ~ comma ~ opt(ws))
                             ~ opt(ws) ~ rGeneric) }

def genericFormals = rule { opt(lGeneric ~  rep1sep(identifier, comma)  ~ rGeneric) }

def whereClause = rule { repdel(whereId ~ typePredicate, semicolon) }
def typePredicate = rule { expression }

//wherever genericFormals appear, there should be a whereClause nearby.


// "literals"

def literal = rule { stringLiteral | selfLiteral | blockLiteral | numberLiteral | 
                         objectLiteral | tupleLiteral | typeLiteral } 

def stringLiteral = rule { opt(ws) ~ doubleQuote ~ rep( stringChar ) ~ doubleQuote ~ opt(ws) } 
def stringChar = rule { (drop(backslash) ~ escapeChar) | anyChar | space}
def blockLiteral = rule { lBrace ~ opt( (matchBinding | blockFormals) ~ arrow) 
                                 ~ innerCodeSequence ~ rBrace }
def selfLiteral = symbol "self" 
def numberLiteral = trim(digitStringParser)
def objectLiteral = rule { objectId ~ lBrace ~ inheritClause ~ codeSequence ~ rBrace }

//these are *not* in the spec - EELCO 
def tupleLiteral = rule { lBrack ~ repsep( expression, comma ) ~ rBrack }

def typeLiteral = rule { typeId ~ opt(ws) ~ nakedTypeLiteral }

//kernan
def nakedTypeLiteral = rule { lBrace ~ opt(ws) ~ repdel(methodHeader ~ methodReturnType, (semicolon | whereClause)) ~ opt(ws) ~ rBrace }

// terminals
def backslash = token "\\"    // doesn't belong here, doesn't work if left below!
def doubleQuote = token "\""
def space = token " " 
def semicolon = rule { (symbol(";") ~ opt(newLine)) | (opt(ws) ~ lineBreak("left" | "same") ~ opt(ws)) }
def colon = rule {both(symbol ":", not(assign))}
def newLine = symbol "\n" 
def lParen = symbol "("
def rParen = symbol ")" 
def lBrace = symbol "\{"
def rBrace = symbol "\}"
def lBrack = symbol "["
def rBrack = symbol "]"
def lrBrack = symbol "[]"
def arrow = symbol "->"
def dot = symbol "."
def assign = symbol ":="
def equals = symbol "="

def lGeneric = token "<"
def rGeneric = token ">"

def comma = rule { symbol(",") }
def escapeChar = characterSetParser("\\\"'\{\}bnrtlfe ")

def azChars = "abcdefghijklmnopqrstuvwxyz"
def AZChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
def otherChars = "1234567890~!@#$%^&*()_-+=[]|\\:;<,>.?/"

def anyChar = characterSetParser(azChars ++ AZChars ++ otherChars)

def identifierString = (graceIdentifierParser ~ drop(opt(ws)))

// def identifier = rule { bothAll(trim(identifierString),not(reservedIdentifier))  }   
                           // bothAll ensures parses take the same length
// def identifier = rule{ both(identifierString,not(reservedIdentifier))  }   
                          // both doesn't ensure parses take the same length
def identifier = rule { guard(identifierString, { s -> ! parse(s) with( reservedIdentifier ~ end ) })}
                        // probably works but runs out of stack

// anything in this list needs to be in reservedIdentifier below (or it won't do what you want)
def superId = symbol "super" 
def extendsId = symbol "extends"
def inheritId = symbol "inherit"
def classId = symbol "class" 
def objectId = symbol "object" 
def typeId = symbol "type" 
def whereId = symbol "where" 
def defId = symbol "def" 
def varId = symbol "var" 
def methodId = symbol "method" 
def prefixId = symbol "prefix" 
def interfaceId = symbol "interface"
def returnId = symbol "return"
//where is OUTER??? - EELCO July 25

//kernan
def reservedIdentifier = rule {selfLiteral | superId | extendsId | inheritId | classId | objectId | typeId | whereId | returnId | defId | varId | methodId | prefixId | interfaceId } // more to come

def reservedOp = rule {assign | equals | dot | arrow | colon | semicolon}  // this is not quite right

//ENDGRAMMAR

//////////////////////////////////////////////////
// Grace Parser Tests

print "------: starting parser tests defs"

def t001 = stringInputStream("print(\"Hello, world.\")",1)
def t001s = stringInputStream("print(\"Hello, world.\")",7)
def t001c = stringInputStream("print(\"Hello, world.\")",8)
def t001ss = stringInputStream("print \"Hello, world.\"",1)
def t001b = stringInputStream("print \{ foo; bar; \}",1)

def t002 = stringInputStream("hello",1)
def t003 = stringInputStream("print(\"Hello, world.\") print(\"Hello, world.\")" ,1)
def t003a = stringInputStream("print(\"Hello, world.\")print(\"Hello, world.\")" ,1)

print "------: starting parser tests"

//testing semicolon insertion

test ( semicolon ~ end ) on ";" correctly "XS1"
test ( semicolon ~ end ) on "\n" correctly "XS2"
test ( semicolon ~ end ) on "\n " correctly "XS3"
test ( semicolon ~ end ) on " \n" correctly "XS3a"
test ( semicolon ~ end ) on " \n " correctly "XS4"
test ( semicolon ~ end ) on ";\n" correctly "XS5"

test ( repsep( methodHeader ~ methodReturnType, newLine)) on "foo\n" correctly "X16d1"
test ( repsep( methodHeader ~ methodReturnType, newLine)) on "foo\nbar\n" correctly "X16d2"
test ( repsep( methodHeader ~ methodReturnType, newLine)) on "foo\nbar\nbaz" correctly "X16d3"

test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo\n" correctly "X16d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo\nbar\n" correctly "X16d2"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo\nbar\nbaz" correctly "X16d3"

test (codeSequence ~ end) on "foo\n" correctly "X13a1"
test (codeSequence ~ end) on "foo(\n" wrongly "X13x1"
test (codeSequence ~ end) on "foo\nbar\n" correctly "X13a11"
test (codeSequence ~ end) on "foo\nbar\nbaz" correctly "X13a12"
test (codeSequence ~ end) on "foo(x)\n  bar(x)\n" correctly "X13a13"
test (ws ~ identifier ~ end) on "\n x" correctly "X13a13.1"
test (ws ~ identifier ~ ws ~ identifier ~ end) on "\n   x\n   x" correctly "X13a13.2"
test (ws ~ end) on " " correctly "X13a13.3"
test (ws ~ end) on "  " correctly "X13a13.4"
test (ws ~ identifier ~ end) on "   \n     x" correctly "X13a13.5"
test (ws ~ identifier ~ end) on "   \n       x" correctly "X13a13.6"
test (ws ~ identifier ~ end) on "\n xx" correctly "X13a13.7"
test (codeSequence ~ end) on "foo(x)\n  bar(x)\n  baz(x)" correctly "X13a14"
test (codeSequence ~ identifier) on "var x := 4\nfoo\ndef b = 4\nbar\nbaz" correctly "013a2z"

currentIndentation := 1
test (codeSequence ~ identifier) on " var x := 4\n foo\n def b = 4\n bar\n baz" correctly "013a2"
test (codeSequence ~ identifier) on " var x := 4\n foo\n 3+4\n def b = 4\n bar\n 1+2\n baz\n" correctly "013a3"
currentIndentation := 0

testProgramOn "method foo \{a;\n b;\n c;\n\}" correctly "X17c1"
testProgramOn "     method foo \{a;\n b;\n c;\n\}" wrongly "X17c2"
testProgramOn "method foo \{\n     a;\n     b;\n     c;\n\}" correctly "X17c3"

testProgramOn "method foo<T>(a) where T < Foo; \{a;\n b;\n c;\n\}" correctly "X17d1"
testProgramOn "method foo<T>(a) where T < Foo; \{ a\n b\n c\n\}" wrongly "X17d2"      //hmm
testProgramOn "method foo<T>(a) where T < Foo; \{\n a\n b\n c\n\}" correctly "X17d3" 

test (ws ~ (token "1") ~ indentAssert(1)) on (indentStx) correctly "I20t1"
test (ws ~ (token "11") ~  (token "\n") ~ ws ~ indentAssert(2)) on (indentStx) correctly "I20t2"
test (ws ~ (token "11") ~ (token "\n") ~ ws ~ (token("22")) ~ (token "\n") ~ (token("no")) ~ indentAssert(0)) on (indentStx) correctly "I20t3"
test (ws ~ (token "11") ~  semicolon ~ indentAssert(2)) on (indentStx) wrongly "I20t4"
test (ws ~ (token "11") ~  semicolon ~ symbol("22")) on (indentStx) wrongly "I20t4a"
test (ws ~ (token "11") ~  semicolon ~ symbol("22") ~ semicolon) on (indentStx) wrongly "I20t4b"
test (ws ~ (token "11") ~ semicolon ~ (symbol("22")) ~ semicolon ~ (symbol("no")) ~ indentAssert(0)) on (indentStx) wrongly "I20t5"


print "done"

// return

test {symbol("print").parse(t001).succeeded}
    expecting(true)
    comment "symbol print"    
test {newLine.parse(t001).succeeded}
    expecting(false)
    comment "newLine"    
test {rep(anyChar).parse(t001c).succeeded}
    expecting(true)
    comment "anyChar"    
test {rep(anyChar).parse(t001c).next.position}
    expecting(14)
    comment "anyChar posn"    
test {rep(stringChar).parse(t001c).succeeded}
    expecting(true)
    comment "stringChar"    
test {rep(stringChar).parse(t001c).next.position}
    expecting(21)
    comment "stringChar posn"    
test {stringLiteral.parse(t001s).succeeded}
    expecting(true)
    comment "stringLiteral"    
test {program.parse(t001).succeeded}
     expecting(true)
     comment "001-print"
test(requestWithArgs ~ end) on("print(\"Hello World\")") correctly("001-RWA")
test(requestWithArgs ~ end) on("print \"Hello World\"") correctly("001-RWA-noparens")
test(implicitSelfRequest ~ end) on("print(\"Hello World\")") correctly("001-ISR")
test(implicitSelfRequest ~ end) on("print \"Hello World\"") correctly("001-ISR-noparens")
test(expression ~ end) on("print(\"Hello World\")") correctly("001-Exp")
test(expression ~ end) on("print \"Hello World\"") correctly("001-Exp-noparens")

test {program.parse(t002).succeeded}
     expecting(true)
     comment "002-helloUnary"
test {program.parse(t003).succeeded}
     expecting(true)
     comment "003-hello hello"
test {program.parse(t003).succeeded}
     expecting(true)
     comment "003a-hellohello"
test {program.parse(t001ss).succeeded}
     expecting(true)
     comment "001ss-stringarg"
test {program.parse(t001b).succeeded}
     expecting(true)
     comment "001b-blockarg"
testProgramOn("self") correctly("004-self")
testProgramOn("(self)") correctly("004p-self")
testProgramOn("(hello)") correctly("004p-hello")
test(expression ~ end) on("foo") correctly("005-foo")
test(expression ~ end) on("(foo)") correctly("005-foo")
test(primaryExpression ~ end) on("(foo)") correctly("005-pri(foo)")
test(argumentsInParens ~ end) on("(foo)") correctly("005-aIP(foo)")
test(requestArgumentClause ~ end) on("print(\"Hello\")") correctly("005-racqhello")

test(identifier ~ end) on("foo") correctly("006id")
test(expression ~ end) on("foo") correctly("006exp")
test(primaryExpression ~ end) on("foo") correctly("006primaryExp")
test(addOp ~ end) on("+") correctly("006plus is addOp")

test(expression ~ end) on("foo+foo") correctly("006exp")
test(expression ~ end) on("foo + foo") correctly("006exp")
test(addOp ~ end) on("+") correctly("006")
test(multExpression ~ end) on("foo") correctly("006mult")
test(addExpression ~ end) on("foo + foo") correctly("006add")
test(expression ~ end) on("foo + foo + foo") correctly("006expr")
test(expression ~ end) on("foo * foo + foo") correctly("006expr")
test(expression ~ end) on("((foo))") correctly("006expr")
test(parenExpression ~ end ) on("((foo))") correctly("006paren")
test(otherOp ~ end) on("%%%%%") correctly("006other")
test(opExpression ~ end) on("foo") correctly "006OpExprFOO"
test(opExpression ~ end) on("foo %%%%% foo") correctly("006OpExprTWO")
test(opExpression ~ end) on("foo %%%%% foo %%%%% foo") correctly("006OpExpr")
test(identifier ~ otherOp ~ identifier ~ otherOp ~ identifier ~ end) on("foo %%%%% foo %%%%% foo") correctly("006OpExprHACK")
test(identifier ~ otherOp ~ identifier ~ otherOp ~ identifier ~ end) on("foo%%%%%foo%%%%%foo") correctly("006OpExprHACKnows")
test(expression ~ end) on("foo %%%%% foo %%%%% foo") correctly("006expr")
test(parenExpression ~ end) on("(foo + foo)") correctly("006parenE")
test(lParen ~ identifier ~ addOp ~ identifier ~ rParen ~ end) on("(foo + foo)") correctly("006hack")
test(lParen ~ primaryExpression ~ addOp ~ primaryExpression ~ rParen ~ end) on("(foo + foo)") correctly("006hackPrimary")
test(lParen ~ multExpression ~ addOp ~ multExpression ~ rParen ~ end) on("(foo + foo)") correctly("006hackMult")
test(lParen ~ rep1sep(multExpression, addOp) ~ rParen ~ end) on("(foo + foo)") correctly("006hackRepSep")
test(lParen ~ repsep(multExpression, addOp) ~ rParen ~ end) on("(foo+foo)") correctly("006hackRepSep2")
test(lParen ~ multExpression ~ repsep(addOp, multExpression) ~ rParen ~ end) on("(foo+foo)") wrongly("006hackRepSep2F")
test(lParen ~ multExpression ~ repsep(addOp, multExpression) ~ rParen ~ end) on("(foo + foo)") wrongly("006hackRepSepF")
test(expression ~ end) on("(foo + foo)") correctly("006")
test(expression ~ end) on("(foo + foo) - foo") correctly("006")
test(expression ~ end) on("(foo + foo - foo)") correctly("006")
test(expression ~ end) on("(foo+foo)-foo") correctly("006")
test(expression ~ end) on("hello(foo+foo)") correctly("006")

testProgramOn "print(1)" correctly "006z1"
testProgramOn " print(1)" wrongly "006z2"
testProgramOn "print(  1     )" correctly "006z3"
testProgramOn "print(1 + 2)" correctly "006z4"
testProgramOn "print(1+2)" correctly "006z5"
testProgramOn "print(1     +2    )" correctly "006z6"
testProgramOn "print(10)" correctly "006z7"
testProgramOn "print (10)" correctly "006z8"
testProgramOn "print(10)  print(10)" correctly "006z9"
testProgramOn "print(10)print(10)" correctly "006z10"
testProgramOn "print(10)print(10)" correctly "006z11"
testProgramOn "print(1+2) print (3 * 4)" correctly "006z12"
testProgramOn "foo(10) foo(11)" correctly "006z13"
testProgramOn "print(foo(10) foo(11))" correctly "006z14"
testProgramOn "print   ( foo ( 10 ) foo   ( 11 ) )" correctly "006z15"
testProgramOn "print(foo(10) foo(11) foo(12))" correctly "006z16"
testProgramOn "print(foo(10) foo(11)) print (3)" correctly "006z17"
testProgramOn "3*foo" correctly "006z18"
testProgramOn " 3 * foo" correctly "006z19"
testProgramOn "print(3*foo)" correctly "006z20"
testProgramOn "print(3*foo) print(5*foo)" correctly "006z21"
testProgramOn "4;5;6" correctly "006z22"
testProgramOn " 4 ; 5 ; 6   " correctly "006z23"
testProgramOn "print(4,5,6)" correctly "006z24"
testProgramOn "print((4;5;6))" correctly "006z25"
testProgramOn "print ( 4   , 5 , 6 ) " correctly "006z26"
testProgramOn "print ( ( 4 ; 5 ; 6 ) ) " correctly "006z27"
testProgramOn " foo ; bar ; baz   " correctly "006z28"
testProgramOn "foo;bar;baz" correctly "006z29"
testProgramOn "foo := 3" correctly "006a30"
testProgramOn "foo:=3" correctly "006a31"
testProgramOn " foo := 3 " correctly "006a32"
testProgramOn " foo := (3) " correctly "006a33"
testProgramOn "foo:=(3)" correctly "006a34"
testProgramOn "foo := 3+4" correctly "006a35"
testProgramOn "foo := 3*4" correctly "006a36"
testProgramOn "foo := baz" correctly "006a37"
testProgramOn "foo := baz.bar" correctly "006ay"
testProgramOn "car.speed := 30.mph" correctly "006az"

testProgramOn "foo" correctly "007"
test (unaryRequest ~ end) on "foo" correctly "007unary"
test (rep1sep(unaryRequest,dot) ~ end) on "foo" correctly "007rep1sep unary"
test (implicitSelfRequest ~ end) on "foo.foo" correctly "007ISR"
test (expression ~ end) on "foo.foo" correctly "007Exp"
testProgramOn "foo.foo" correctly "007"
testProgramOn " foo . foo " correctly "007"
testProgramOn "foo.foo(10)" correctly "007"
testProgramOn "foo.foo.foo" correctly "007"

test (numberLiteral ~ multOp ~ trim(identifier) ~ lParen ~ numberLiteral ~ rParen) on "3*foo(50)" correctly "007hack"
test (numberLiteral ~ multOp ~ requestWithArgs ~ end) on "3*foo(50)" correctly "007hack"
test (implicitSelfRequest ~ end) on "foo(50)" correctly "007ISR"
testProgramOn "3*foo(50)" correctly "007"
testProgramOn " 3 * foo ( 50 )" correctly "007"
testProgramOn "(foo(50))*3" correctly "007"
testProgramOn "(foo ( 50 ) * 3)" correctly "007"
testProgramOn "foo(50)*3" correctly "007"
testProgramOn "foo ( 50 ) * 3" correctly "007"
testProgramOn "print(3*foo(50))" correctly "007"
testProgramOn "print ( 3 * foo ( 50 ) )" correctly "007"
testProgramOn "print(foo(10) foo(11)) print (3 * foo(50))" correctly "007"
testProgramOn "foo.foo(40).foo" correctly "007"

print "      : woot" 

test (typeExpression) on "  " correctly "008type1"
test (typeExpression ~ end) on "   " correctly "008type1"
test (typeExpression ~ end) on "Integer" correctly "008type2"
test (typeExpression ~ end) on "  Integer  " correctly "008type3"

testProgramOn "b(t(r(o)), not(re))" correctly "008x1" 
testProgramOn "\{ rep1(dot ~ unaryRequest) ~ rep(opRequestXXX) ~ opt(dot ~ keywordRequest) \}" correctly "008x2"
testProgramOn " if (endPosition > string.size) then \{endPosition := string.size\}" correctly "008x2"
testProgramOn "  if ((n + position) <= (string.size + 1)) then \{return stringInputStream(string, position + n)\}" correctly "008x4"
testProgramOn "return (((c >= \"A\") && (c <= \"Z\"))          | ((c >= \"a\") && (c <= \"z\")))" correctly "008x5"
testProgramOn "\{drop(opt(ws)) ~ p ~ drop(opt(ws))\}" correctly "008x6" // OK ok JS, crashes on C
testProgramOn "drop(opt(ws)) ~ doubleQuote ~ rep( stringChar ) ~ doubleQuote " correctly "008x7"
testProgramOn "" correctly "008"

test (token("return") ~ end) on "return" correctly "008xr1"
test (token("return") ~ end) on " return" wrongly "008xr2"
test (returnStatement ~ end) on "return" correctly "008xr3"
test (returnStatement ~ end) on " return" wrongly "008xr4"
test (returnStatement ~ end) on "return (subParser.parse(in))" correctly "008xr5"
test (returnStatement ~ end) on "return (subParser.parse(in) .resultUnlessFailed)" correctly "008xr9"
test (returnStatement ~ end) on " return (subParser.parse(in))" wrongly "008xr10"
test (returnStatement ~ end) on " return (subParser.parse(in) .resultUnlessFailed)" wrongly "008xr11"

testProgramOn "return (subParser.parse(in) .resultUnlessFailed)" correctly "008x12"
testProgramOn "return (subParser.parse(in))" correctly "008x8"
testProgramOn "return (subParser.parse(in) .resultUnlessFailed)" correctly "008x9"
testProgramOn " return (subParser.parse(in))" wrongly "008x10"
testProgramOn " return (subParser.parse(in) .resultUnlessFailed)" wrongly "008x11"
testProgramOn "return (subParser.parse(in) .resultUnlessFailed)" correctly "008x12"
testProgramOn "(subParser.parse(in) .resultUnlessFailed)" correctly "008x13"
testProgramOn " \{f ->  return parseSuccess(in, \"\")\}" wrongly "008x14"
testProgramOn " \{ return parseSuccess(in, \"\")\}" wrongly "008x15"
testProgramOn " return (subParser.parse(in) .resultUnlessFailed \{f ->  return parseSuccess(in, \"\")\})" wrongly "008x16"
testProgramOn "return (subParser.parse(in) .resultUnlessFailed \{f ->  return parseSuccess(in, \"\")\})" correctly "008x17"

testProgramOn "a" correctly "007x"
testProgramOn "a b" wrongly "007x"
testProgramOn "a b c" wrongly "007x"
testProgramOn "a b c d" wrongly "007x"

test (otherOp) on ".." correctly "008a"
test (trim(rep1(operatorChar))) on ".." correctly "008b"
testProgramOn "position .. endPosition" correctly "008c"
testProgramOn "for (position .. endPosition)" correctly "008d"
testProgramOn "for (position .. endPosition) do (42)" correctly "008e"
testProgramOn "\{ i : Number -> result \}" correctly "008f"
testProgramOn "\{ result ++ string.at(i); \}" correctly "008g"
testProgramOn "\{ result := result \}" correctly "008h"
testProgramOn "\{ result := result; \}" correctly "008h1"
testProgramOn "\{ result := result\n \}" correctly "008h2"
testProgramOn "\{ result := result ++ string.at(i); \}" correctly "008i"
testProgramOn "foo" correctly "008i1"
testProgramOn "   foo" wrongly "008i11"
testProgramOn "foo;  bar" correctly "008i11a"
testProgramOn "   foo;  bar" wrongly "008i11b"
testProgramOn "   foo\n   bar" wrongly "008i12"
testProgramOn "foo\nbar" correctly "008i13"
testProgramOn "\{ result := result ++ string.at(i) \}" correctly "008i2"
testProgramOn "\{ result := result ++ string.at(i) \}" correctly "008i3"
testProgramOn "\{ result := result ++ string.at(i); \}" correctly "008i3"
testProgramOn "for (position .. endPosition) do \{ i : Number -> result := result ++ string.at(i); \}" correctly "008j"
testProgramOn " for (position .. endPosition) do \{ i : Number -> result := result ++ string.at(i); \}" wrongly "008j2"
testProgramOn " for (position .. endPosition) do \{  i : Number -> result := result ++ string.at(i); \}" wrongly "008j3"
testProgramOn "a * * * * * * * * * b" correctly "008k"

test (genericActuals ~ end) on "" correctly "009"
test (genericActuals ~ end) on "<T>" correctly "009"
test (genericActuals ~ end) on "<T,A,B>" correctly "009"
test (genericActuals ~ end) on "<T,A<B>,T>" correctly "009"
test (genericActuals ~ end) on "<T, A<B> , T>" correctly "009"
test (genericActuals ~ end) on "<A & B>" correctly "009"
test (genericActuals ~ end) on "<A&B>" correctly "009"
test (lGeneric ~ opt(ws) ~ stringLiteral ~ opt(ws) ~ comma ~ opt(ws) ~ stringLiteral ~ opt(ws) ~ rGeneric ~ end) on "< \"foo\", \"bar\" >" correctly "009b"
test (lGeneric ~ opt(ws) ~ stringLiteral ~ opt(ws) ~ comma ~ opt(ws) ~ stringLiteral ~ opt(ws) ~ rGeneric ~ end) on "<\"foo\",\"bar\">" correctly "009b"
test (lGeneric ~ stringLiteral ~ opt(ws) ~ comma ~ opt(ws) ~ stringLiteral ~ rGeneric ~ end) on "<\"foo\" , \"bar\">" correctly "009b"
test (lGeneric ~ stringLiteral ~ opt(ws) ~ comma ~ opt(ws) ~ stringLiteral ~ rGeneric ~ end) on "<\"foo\",\"bar\">" correctly "009b"
test (genericActuals ~ end) on "< \"foo\", \"bar\" >" correctly "009c"
test (genericActuals ~ end) on "<\"foo\",\"bar\">" correctly "009c"
test (genericActuals ~ end) on "< A , B >" correctly "009c"
test (genericActuals ~ end) on "<A ,B >" correctly "009c"
test (genericActuals ~ end) on "< A, B>" correctly "009c"
test (genericActuals ~ end) on "    < A, B>" wrongly "009d"

testProgramOn "foo(34)" correctly "009"
testProgramOn "foo<T>" correctly "009"
testProgramOn "foo<T>(34)" correctly "009"
testProgramOn "foo<T,A,B>(34)" correctly "009"
testProgramOn "foo<T>(34) barf(45)" correctly "009"
testProgramOn "foo<T,A,B>(34) barf(45)" correctly "009"
testProgramOn "foo<T>" correctly "009"
testProgramOn "foo<T,A,B>" correctly "009"
testProgramOn "run(a < B, C > 1)" wrongly "009tim-a"
testProgramOn "a < B, C > 1" wrongly "009tim-b"
testProgramOn "a<B,C>(1)" correctly "009tim-c"
testProgramOn "run(a < B, C >(1))" correctly "009tim-d"

testProgramOn "a<B,C>" correctly "009eelco-a"
testProgramOn "a<B>" correctly "009eelco-b"
testProgramOn "m(a<B,C>(3))" correctly "009eelco-c"  /// particularly evil and ambiguous

testProgramOn "m((a<B),(C>(3)))" correctly "009eelco-d"  /// disambiguated
testProgramOn "m((a<B,C>(3)))" correctly "009eelco-e"  /// disambiguated

testProgramOn "1*2+3" correctly "010"
testProgramOn "1+2*3" correctly "010"
testProgramOn "1+2-3+4" correctly "010"
testProgramOn "5*6/7/8" correctly "010"
testProgramOn "1*3-3*4" correctly "010"
testProgramOn "!foo.bar" correctly "010"
testProgramOn "!foo.bar * zarp" correctly "010"
testProgramOn "1 %% 2 * 3 %% 4 + 5" correctly "010"
testProgramOn "1 %% 2 ** 3 %% 4 ++ 5" wrongly "010"
testProgramOn "1 ?? 2 !! 3 $$ 4" wrongly "010"

testProgramOn "1*2+3" correctly "010a"
testProgramOn "1+2*3" correctly "010a"
testProgramOn "1 @ 2+3" correctly "010a"
testProgramOn "1 + 2 @ 3" correctly "010a"
testProgramOn "1 @ 2*3" correctly "010a"
testProgramOn "1 * 2 @ 3" correctly "010a"
testProgramOn "1 @ 2*3 + 4" correctly "010a"
testProgramOn "1 * 2 @ 3 + 4" correctly "010a"


testProgramOn "foo[10]" correctly "010"
testProgramOn "foo[10,20]" correctly "010"
testProgramOn "foo[\"10\"]" correctly "010"
testProgramOn "foo[14+45]" correctly "010"
testProgramOn "foo[bar]" correctly "010"
testProgramOn "foo[bar.baz]" correctly "010"
testProgramOn "foo[10][20][30]" correctly "010"
testProgramOn "foo[bar(1) baz(2)]" correctly "010"
testProgramOn "foo[bar[10]]" correctly "010"
testProgramOn "foo[bar[10].baz[e].zapf]" correctly "010"

testProgramOn "super" wrongly "011"
testProgramOn "return super" wrongly "011"
testProgramOn "super.foo" correctly "011"
testProgramOn "super.foo.bar" correctly "011"
testProgramOn "super.foo(1) bar(2)" correctly "011"
testProgramOn "super + 3" correctly "011"
testProgramOn "super +&^#%$ 3" correctly "011"
testProgramOn "super[3]" correctly "011"
testProgramOn "!super" correctly "011"

testProgramOn "def" wrongly "012"
testProgramOn "def x" wrongly "012"
testProgramOn "def x = " wrongly "012"
testProgramOn "def x := " wrongly "012"
testProgramOn "def x : T =" wrongly "012"
testProgramOn "def x : T := 4" wrongly "012"

testProgramOn "var" wrongly "012"
testProgramOn "var x = " wrongly "012"
testProgramOn "var x := " wrongly "012"
testProgramOn "var x : T :=" wrongly "012"
testProgramOn "var x : T = 4" wrongly "012"

testProgramOn "def x : T = 4" correctly "012"
testProgramOn "var x : T := 4" correctly "012"
testProgramOn "def x = 4" correctly "012"
testProgramOn "var x := 4" correctly "012"
testProgramOn "var x:=4" correctly "012"
test (varId ~ identifier ~ assign ~ numberLiteral) on "var x := 4" correctly "012"
test (varId ~ identifier ~ assign ~ expression) on "var x := 4" correctly "012"
test (varId ~ identifier ~ opt(assign ~ expression)) on "var x := 4" correctly "012"
test (varDeclaration) on "var xVarDec := 4" correctly "012"
test (declaration) on "var xDec := 4" correctly "012"
test (codeSequence) on "var xCodeSeq := 4" correctly "012"
test (program) on "var xParenProg := 4" correctly "012"
testProgramOn "var xProg := 4" correctly "012"
testProgramOn "var x : TTT := foobles.barbles" correctly "012"
testProgramOn "var x := foobles.barbles" correctly "012"

test (defId ~ identifier) on "def x" correctly "012d"
test (defId ~ identifier) on "def typeExpression" correctly "012d"
test (defId ~ identifier ~ equals) on "def typeExpression =" correctly "012d"
test (defId ~ identifier) on "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d1"
test (defId ~ identifier ~ equals) on "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d2"
test (defId ~ identifier ~ equals ~ expression) on "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d3"
test (defDeclaration) on "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d4"
test (declaration) on "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d5"
testProgramOn "def typeExpression = rule \{ trim(identifier) | opt(ws) \}" correctly "012d6"
testProgramOn "rule \{ trim(identifier) | opt(ws) \}" correctly "012d7"
testProgramOn "rule ( trim(identifier) | opt(ws) )" correctly "012d8"

test (identifier) on "typeExpression" correctly "012d9"
test (identifier) on "superThing" correctly "012d9"
test (identifierString) on "typeExpression" correctly "012d10"
test (identifierString) on "superThing" correctly "012d10"

testProgramOn "var x := 4; foo; def b = 4; bar; baz" correctly "013a"
test (objectLiteral) on "object \{ \}" correctly "013a"
test (objectLiteral) on "object \{ var x := 4; foo; def b = 4; bar; baz \}" correctly "013a"
test (codeSequence) on "var x := 4; foo; def b = 4; bar; baz" correctly "013a"
test (codeSequence) on "var x := 4; foo; 3+4; def b = 4; bar; 1+2; baz;" correctly "013a"
testProgramOn "method foo \{a; b; c\}" correctly "013b"
testProgramOn "method foo \{a; b; c; \}" correctly "013b"
testProgramOn "method foo -> \{a; b; c\}" wrongly "013b2"
testProgramOn "method foo -> T \{a; b; c\}" correctly "013b3"
testProgramOn "method foo<T> \{a; b; c\}" correctly "013b4"
testProgramOn "method foo<T,V> \{a; b; c; \}" correctly "013b5"
testProgramOn "method foo<T> -> \{a; b; c\}" wrongly "013b6"
testProgramOn "method foo<T,V> -> T \{a; b; c\}" correctly "013b7"

test (methodHeader ~ end) on "foo" correctly "013c1"
test (firstArgumentHeader ~ end) on "foo(a)" correctly "013c11"
test (methodWithArgsHeader ~ end) on "foo(a)" correctly "013c11"
test (methodHeader ~ end) on "foo(a)" correctly "013c11"
test (firstArgumentHeader ~ end) on "foo(a,b)" correctly "013c12"
test (methodWithArgsHeader ~ end) on "foo(a,b)" correctly "013c12"
test (methodHeader ~ end) on "foo(a,b)" correctly "013c12"
test (methodWithArgsHeader ~ end) on "foo(a,b) foo(c,d)" correctly "013c13"
test (methodHeader ~ end) on "foo(a,b) foo(c,d)" correctly "013c13"
test (methodHeader ~ end) on "foo(a,b) foo " wrongly "013c14"

testProgramOn "method foo(a) \{a; b; c\}" correctly "013c"
testProgramOn "method foo(a, b, c) \{a; b; c\}" correctly "013c2"
testProgramOn "method foo(a : T) \{a; b; c\}" correctly "013c3"
testProgramOn "method foo(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c4"
testProgramOn "method foo(a, b, c) -> T \{a; b; c\} " correctly "013c5"
testProgramOn "method foo(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c6"
testProgramOn "method foo(a : T, b : T, c : T) foo(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c6"
testProgramOn "method foo(a, b, c) \{a; b; c\}" correctly "013c7"
testProgramOn "method foo(a, b, c) bar(d,e)\{a; b; c\}" correctly "013c7"
testProgramOn "method foo(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c8"
testProgramOn "method foo(a, b : T, c) -> F \{a; b; c\}" correctly "013c9"
testProgramOn "method foo<T>(a) \{a; b; c\}" correctly "013c"
testProgramOn "method foo<TER,MIN,US>(a, b, c) \{a; b; c\}" correctly "013c2"
testProgramOn "method foo<TXE>(a : T) \{a; b; c\}" correctly "013c3"
testProgramOn "method foo<T,U,V>(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c4"
testProgramOn "method foo<T,U>(a : T, b : T, c : T) foo(a : T, b : T, c : T) -> T \{a; b; c\}" correctly "013c6"
testProgramOn "method foo(a : T, b : T, c : T) foo<T,U>(a : T, b : T, c : T) -> T \{a; b; c\}" wrongly "013c6"
testProgramOn "method foo<>(a : T, b : T, c : T) foo<T,U>(a : T, b : T, c : T) -> T \{a; b; c\}" wrongly "013c6"
testProgramOn "method foo<>(a : T, b : T, c : T)  \{a; b; c\}" wrongly "013c6"
testProgramOn "method foo<> \{a; b; c\}" wrongly "013c6"

testProgramOn "method +(x) \{a; b; c\}" correctly "013d1"
testProgramOn "method ==(x) \{a; b; c\}" correctly "013d1"
testProgramOn "method =(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method :=(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method ++***&%&(x) \{a; b; c\}" correctly "013d1"
testProgramOn "method +(x: T) \{a; b; c\}" correctly "013d2"
testProgramOn "method +(x) -> T \{a; b; c\}" correctly "013d3"
testProgramOn "method +(x : T) -> T \{a; b; c\}" correctly "013d3"
test (methodHeader) on "+ -> T" wrongly "013d5a"
testProgramOn "method + -> T \{a; b; c\}" wrongly "013d5"
testProgramOn "method +(x,y) T \{a; b; c\}" wrongly "013d6"
testProgramOn "method +(x : T, y : T) -> T \{a; b; c\}" wrongly "013d7"
testProgramOn "method +(x) +(y) -> T \{a; b; c\}" wrongly "013d8"

testProgramOn "method prefix+ \{a; b; c\}" correctly "013e1"
testProgramOn "method prefix + \{a; b; c\}" correctly "013e1"
testProgramOn "method prefix++***&%& \{a; b; c\}" correctly "013e1"
testProgramOn "method prefix ! \{a; b; c\}" correctly "013e1"
testProgramOn "method prefix+ -> \{a; b; c\}" wrongly "013e2"
testProgramOn "method prefix+(x) -> T \{a; b; c\}" wrongly "013e3"
testProgramOn "method prefix+(x : T) -> T \{a; b; c\}" wrongly "013e3"
testProgramOn "method prefix+ -> T \{a; b; c\}" correctly "013e5"
testProgramOn "method prefix+(x,y) T \{a; b; c\}" wrongly "013e6"
testProgramOn "method prefix+(x : T, y : T) -> T \{a; b; c\}" wrongly "013e7"
testProgramOn "method prefix+(x) +(y) -> T \{a; b; c\}" wrongly "013e8"
testProgramOn "method prefix(x) -> T \{a; b; c\}" wrongly "013e9"
testProgramOn "method prefix:= \{a; b; c\}" wrongly "013e1"
testProgramOn "method prefix := \{a; b; c\}" wrongly "013e1"
testProgramOn "method prefix[] \{a; b; c\}" wrongly "013e1"

//what should the *grammar* say about assignment op return values
test (assignmentMethodHeader) on "foo:=(a)" correctly "013a1"
test (assignmentMethodHeader) on "foo := ( a : T )" correctly "013a1"
test (assignmentMethodHeader) on "foo" wrongly "013a1"
test (assignmentMethodHeader) on "foobar:=" wrongly "013a1"
testProgramOn "method foo:=(a) \{a; b; c\}" correctly "013f"
testProgramOn "method bar :=(a) \{a; b; c\}" correctly "013f2"
testProgramOn "method foo:=(a : T) \{a; b; c\}" correctly "013f3"
testProgramOn "method foo :=(a : T) -> T \{a; b; c\}" correctly "013f4"
testProgramOn "method foo:=(a) -> T \{a; b; c\} " correctly "013f5"
testProgramOn "method foo:=(a : T, b : T, c : T) -> T \{a; b; c\}" wrongly "013f6"
testProgramOn "method foo:=(a : T, b : T, c : T) foo(a : T, b : T, c : T) -> T \{a; b; c\}" wrongly "013f6"
testProgramOn "method foo:=(a, b, c) \{a; b; c\}" wrongly "013f7"
testProgramOn "method foo:=(a, b, c) bar(d,e)\{a; b; c\}" wrongly "013f7"
testProgramOn "method foo:=(a : T, b : T, c : T) -> T \{a; b; c\}" wrongly "013f8"
testProgramOn "method foo:=(a, b : T, c) -> F \{a; b; c\}" wrongly "013f9"


testProgramOn "method [](x) \{a; b; c\}" correctly "013d1"
testProgramOn "method [](x, y, z) \{a; b; c\}" correctly "013d1"
testProgramOn "method []=(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method [=](x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method []foo(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method foo[](x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method [][]***&%&(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method [](x: T) \{a; b; c\}" correctly "013d2"
testProgramOn "method [](x) -> T \{a; b; c\}" correctly "013d3"
testProgramOn "method [](x : T) -> T \{a; b; c\}" correctly "013d3"
testProgramOn "method [] -> T \{a; b; c\}" wrongly "013d5"
testProgramOn "method [](x,y) T \{a; b; c\}" wrongly "013d6"
testProgramOn "method [](x : T, y : T) -> T \{a; b; c\}" correctly "013d7"
testProgramOn "method [](x) [](y) -> T \{a; b; c\}" wrongly "013d8"


testProgramOn "method []:=(x) \{a; b; c\}" correctly "013d1"
testProgramOn "method []:=(x, y, z) \{a; b; c\}" correctly "013d1"
testProgramOn "method [] :=(x) \{a; b; c\}" correctly "013d1"
testProgramOn "method [] :=(x, y, z) \{a; b; c\}" correctly "013d1"
testProgramOn "method []:==(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method [=](x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method []:=foo(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method foo[]:=(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method []:=[]:=***&%&(x) \{a; b; c\}" wrongly "013d1"
testProgramOn "method []:=(x: T) \{a; b; c\}" correctly "013d2"
testProgramOn "method []:=(x) -> T \{a; b; c\}" correctly "013d3"
testProgramOn "method []:=(x : T) -> T \{a; b; c\}" correctly "013d3"
testProgramOn "method []:= -> T \{a; b; c\}" wrongly "013d5"
testProgramOn "method []:=(x,y) T \{a; b; c\}" wrongly "013d6"
testProgramOn "method []:=(x : T, y : T) -> T \{a; b; c\}" correctly "013d7"
testProgramOn "method []:=(x) []:=(y) -> T \{a; b; c\}" wrongly "013d8"

//evil list syntax
testProgramOn "[]" correctly "014a"
testProgramOn "[1,2,3]" correctly "014b"
testProgramOn "[ \"a\", \"a\", \"a\", 1]" correctly "014c"
testProgramOn "[ \"a\", \"a\", \"a\", 1" wrongly "014d"
testProgramOn "[ \"a\" \"a\" \"a\" 1]" wrongly "014e"
testProgramOn "[][3][4][5]" correctly "014f"

//  "Old" Class syntax
//
// testProgramOn "class Foo \{ \}" correctly "015a"
// testProgramOn "class Foo \{ a; b; c \}" correctly "015b"
// testProgramOn "class Foo \{ def x = 0; var x := 19; a; b; c \}" correctly "015c"
// testProgramOn "class Foo \{ a, b -> a; b; c;  \}" correctly "015d"
// testProgramOn "class Foo \{ a : A, b : B -> a; b; c;  \}" correctly "015e"
// testProgramOn "class Foo \{ <A> a : A, b : B -> a; b; c;  \}" correctly "015f"
// testProgramOn "class Foo \{ <A, B> a : A, b : B -> a; b; c;  \}" correctly "015g"
// testProgramOn "class Foo " wrongly "015h"
// testProgramOn "class Foo a; b; c" wrongly "015i"
// testProgramOn "class Foo \{ <A> def x = 0; var x := 19; a; b; c \}" wrongly "015j"
// testProgramOn "class Foo \{ -> a; b; c;  \}" correctly "015k"
// testProgramOn "class Foo \{ a : <A>, b : <B> -> a; b; c;  \}" wrongly "015l"
// testProgramOn "class Foo \{ -> <A> a : A, b : B  a; b; c;  \}" wrongly "015m"


// "new" aka "Alt" Class syntax
testProgramOn "class Foo \{ \}" correctly "015a"
testProgramOn "class Foo \{ a; b; c \}" correctly "015b"
testProgramOn "class Foo \{ method a \{\}; method b \{\}; method c \{\}\}" correctly "015b"
testProgramOn "class Foo \{ def x = 0; var x := 19; a; b; c \}" correctly "015c"
testProgramOn "class Foo(a,b) \{ a; b; c;  \}" correctly "015d"
testProgramOn "class Foo(a : A, b : B) \{ a; b; c;  \}" correctly "015e"
testProgramOn "class Foo<A>(a : A, b : B) new(a : A, b : B) \{ a; b; c;  \}" correctly "015f"
testProgramOn "class Foo<A, B>(a : A, b : B) \{ a; b; c;  \}" correctly "015g"
testProgramOn "class Foo " wrongly "015h"
testProgramOn "class Foo a; b; c" wrongly "015i"
testProgramOn "class Foo<A> \{ def x = 0; var x := 19; a; b; c \}" wrongly "015j"
testProgramOn "class Foo \{ -> a; b; c;  \}" wrongly "015k"
testProgramOn "class Foo \{ a : <A>, b : <B> -> a; b; c;  \}" wrongly "015l"
testProgramOn "class Foo \{ -> <A> a : A, b : B  a; b; c;  \}" wrongly "015m"

testProgramOn "class Foo \{ inherit Foo; \}" correctly "015ia"
testProgramOn "class Foo \{ inherit Foo; a; b; c \}" correctly "015ib"
testProgramOn "class Foo \{ inherit Foo(3,4); method a \{\}; method b \{\}; method c \{\}\}" correctly "015ib"
testProgramOn "class Foo \{ inherit Foo(3,4); def x = 0; var x := 19; a; b; c \}" correctly "015ic"
testProgramOn "class Foo(a,b) \{ inherit Foo<X>(4); a; b; c;  \}" correctly "015id"
testProgramOn "class Foo(a : A, b : B) \{ inherit goobles; a; b; c;  \}" correctly "015ie"
testProgramOn "class Foo<A>(a : A, b : B) new(a : A, b : B) \{ inherit OttDtraid; a; b; c;  \}" correctly "015if"
testProgramOn "class Foo<A, B>(a : A, b : B) \{ inherit Foo(2) new(4); a; b; c;  \}" correctly "015ig"
testProgramOn "class Foo \{ inherit; a; b; c \}" wrongly "015ih"

testProgramOn "3+4.i" correctly "015z"
test (expression) on "" wrongly "015zz"

test (typeId) on "type" correctly "16aa1"
test (typeId ~ end) on "type" correctly "16aa2"

test (typeLiteral) on "type \{ \}" correctly "016cl"
test (typeLiteral) on "type \{ foo \}" correctly "016cl1"
test (typeLiteral) on "type \{ foo; bar; baz; \}" correctly "016cl2"
test (typeLiteral) on "type \{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016cl3"

test (typeExpression ~ end) on "type \{ \}" correctly "016cx1"
test (typeExpression ~ end) on "type \{ foo \}" correctly "016cx2"
test (typeExpression ~ end) on "type \{ foo; bar; baz; \}" correctly "016cx3"
test (typeExpression ~ end) on "type \{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016cx4"
test (typeExpression ~ end) on "\{ \}" correctly "016cx5"
test (typeExpression ~ end) on "\{ foo \}" correctly "016cx5"
test (typeExpression ~ end) on "\{ foo; bar; baz; \}" correctly "016cx7"
test (typeExpression ~ end) on "\{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016cx8"

test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo -> T" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "prefix!" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "+(x : T)" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo;" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo; bar;" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo; bar; baz" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo<T> -> T" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo<T>" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "prefix<T> !" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "+(x : T)" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo<T>;" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo<T>; bar<T>;" correctly "016d1"
test ( repsep( methodHeader ~ methodReturnType, semicolon)) on "foo; bar<T>; baz<T>" correctly "016d1"
test (typeId ~ lBrace ~ repdel( methodHeader ~ methodReturnType, semicolon) ~ rBrace) on "type \{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016e"
test (repdel( methodHeader ~ methodReturnType, semicolon)) on "prefix!; +(other : SelfType); baz(a,b) baz(c,d)" correctly "016e"
test (typeExpression ~ end) on "T" correctly "016c"
test (lGeneric ~ typeExpression ~ rGeneric ~ end) on "<T>" correctly "016c"
test (typeExpression ~ end) on "type \{ \}" correctly "016c"
test (typeExpression ~ end) on "type \{ foo \}" correctly "016c"
test (typeExpression ~ end) on "type \{ foo; bar; baz; \}" correctly "016d"
test (typeExpression ~ end) on "type \{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016e"
test (typeExpression ~ end) on "type \{ prefix!; +(other : SelfType); baz(a,b) baz(c,d); \}" correctly "016e"
test (typeExpression ~ end) on "" correctly "016a"
test (typeExpression ~ end) on "T" correctly "016a"
test (typeExpression ~ end) on "=T" wrongly "016a"
test (typeExpression ~ end) on "!T" wrongly "016a"
test (typeExpression ~ end) on "T<A,B>" correctly "016c"
test (typeExpression ~ end) on "T<B>" correctly "016c"
test (typeExpression ~ end) on "A & B" correctly "016c"
test (typeExpression ~ end) on "A & B & C" correctly "016c"
test (typeExpression ~ end) on "A & B<X> & C" correctly "016ct"
test (typeExpression ~ end) on "A | B<X> | C" correctly "016ct"
test (expression ~ end) on "A & B(X) & C" correctly "016cx"
test (expression ~ end) on "A | B(X) | C" correctly "016cx"
test (expression ~ end) on "A & B<X> & C" correctly "016cx"
test (expression ~ end) on "A | B<X> | C" correctly "016cx"
test (typeExpression ~ end) on "A & B | C" wrongly "016c"
test (typeExpression ~ end) on "A & type \{ foo(X,T) \}" correctly "016c"
test (typeExpression ~ end) on " \"red\"" correctly "016t1"
test (typeExpression ~ end) on " \"red\" | \"blue\" | \"green\"" correctly "016t1"
test (typeExpression ~ end) on " 1 | 2 | 3 " correctly "016t1"
test (expression ~ end) on "\"red\"|\"blue\"|\"green\"" correctly "016t1"
test (expression ~ end) on " \"red\" | \"blue\" | \"green\"" correctly "016t1"
test (expression ~ end) on " 1 | 2 | 3 " correctly "016t1"
test (typeExpression ~ end) on "super.T<A,B>" correctly "016pt"
test (typeExpression ~ end) on "super.A & x.B" correctly "016pt"
test (typeExpression ~ end) on "super.A & a.B & a.C" correctly "test"
test (typeExpression ~ end) on "super.A & B<super.X> & C" correctly "016ptt"
test (typeExpression ~ end) on "A | B<X> | C" correctly "016ptt"
test (typeExpression ~ end) on "T<super.A.b.b.B.c.c.C,super.a.b.c.b.b.B>" correctly "016pt"
test (typeExpression ~ end) on "a<X,super.Y,z.Z>.a.A & b.b.B" correctly "016pt"
test (typeExpression ~ end) on "a<X,super.Y,z.Z>.a.A & b.b.B & c.c.C" correctly "016pt"
test (typeExpression ~ end) on "a<X,super.Y,z.Z>.a.A & b.b.B<X> & c.c.C" correctly "016ptt"
test (typeExpression ~ end) on "a<X,super.Y,z.Z>.a.A | b.b.B<X> | c.c.C" correctly "016ptt"
test (typeDeclaration ~ end) on "type A = B;" correctly "016td1"
test (typeDeclaration ~ end) on "type A=B;" correctly "016td2"
test (typeDeclaration ~ end) on "type A<B,C> = B & C;" correctly "016td3"
test (typeDeclaration ~ end) on "type A<B> = B | Noo | Bar;" correctly "016td4"
test (typeDeclaration ~ end) on "type Colours = \"red\" | \"green\" | \"blue\";" correctly "016td5"
test (typeDeclaration ~ end) on "type FooInterface = type \{a(A); b(B); \};" correctly "016td6"
test (typeDeclaration ~ end) on "type FooInterface = \{a(A); b(B); \};" correctly "016td7"
test (typeDeclaration ~ end) on "type PathType = super.a.b.C;" correctly "016td8"
test (typeDeclaration ~ end) on "type GenericPathType<A,X> = a.b.C<A,X>;" correctly "016td9"

test (whereClause ~ end) on "where T <: Sortable;" correctly "017a1"
test (whereClause ~ end) on "where T <: Foo<A,B>;" correctly "017a2"
test (whereClause ~ end) on "where T <: Foo<A,B>; where T <: Sortable<T>;" correctly "017a3"
testProgramOn "method foo<T>(a) where T < Foo; \{a; b; c\}" correctly "017c1"
testProgramOn "method foo<TER,MIN,US>(a, b, c) where TERM <: MIN <: US; \{a; b; c\}" correctly "017c2"
testProgramOn "method foo<TXE>(a : T) where TXE <: TXE; \{a; b; c\}" correctly "017c3"
testProgramOn "method foo<T,U,V>(a : T, b : T, c : T) -> T where T <: X<T>; \{a; b; c\}" correctly "017c4"
testProgramOn "method foo<T,U>(a : T, b : T, c : T) foo(a : T, b : T, c : T) -> T where T <: T; \{a; b; c\}" correctly "017c6"
testProgramOn "class Foo<A>(a : A, b : B) new(a : A, b : B) where T <: X; \{ a; b; c;  \}" correctly "017f"
testProgramOn "class Foo<A, B>(a : A, b : B) where A <: B; \{ a; b; c;  \}" correctly "017g"
testProgramOn "class Foo<A, B>(a : A, b : B) where A <: B; where A <: T<A,V,\"Foo\">; \{ a; b; c;  \}" correctly "017g"
testProgramOn "class Foo<A, B>(a : A, b : B) where A <: B; where A <: T<A,V,\"Foo\">; \{ method a where T<X; \{ \}; method b(a : Q) where T <: X; \{ \}; method c where SelfType <: Sortable<Foo>; \{ \} \}" correctly "017g"

test (matchBinding ~ end) on "a" correctly "018a1"
test (matchBinding ~ end) on "_" correctly "018a1"
test (matchBinding ~ end) on "0" correctly "018a1"
test (matchBinding ~ end) on "(a)" correctly "018a1"
test (matchBinding ~ end) on "\"Fii\"" correctly "018a1"
test (matchBinding ~ end) on "a : Foo" correctly "018a1"
test (matchBinding ~ end) on "a : Foo(bar,baz)" correctly "018a1"
test (matchBinding ~ end) on "a : Foo(_ : Foo(a,b), _ : Foo(c,d))" correctly "018a1"

test (blockLiteral ~ end) on "\{ _ : Foo -> last \}" correctly "018b1"
test (blockLiteral ~ end) on "\{ 0 -> \"Zero\" \}" correctly "018b"
test (blockLiteral ~ end) on "\{ s:String -> print(s) \}" correctly "018b"
test (blockLiteral ~ end) on " \{ (pi) -> print(\"Pi = \" ++ pi) \}" correctly "018c"
test (blockLiteral ~ end) on " \{ _ : Some(v) -> print(v) \}" correctly "018d"
test (blockLiteral ~ end) on " \{ _ : Pair(v : Pair(p,q), a : Number) -> print(v) \}" correctly "018e"
test (blockLiteral ~ end) on " \{ _ -> print(\"did not match\") \}" correctly "018f"

testProgramOn "\{ _ : Foo -> last \}" correctly "018b1"
testProgramOn "\{ 0 -> \"Zero\" \}" correctly "018b"
testProgramOn "\{ s:String -> print(s) \}" correctly "018b"
testProgramOn " \{ (pi) -> print(\"Pi = \" ++ pi) \}" correctly "018c"
testProgramOn " \{ _ : Some(v) -> print(v) \}" correctly "018d"
testProgramOn " \{ _ : Pair(v : Pair(p,q), a : Number) -> print(v) \}" correctly "018e"
testProgramOn " \{ _ -> print(\"did not match\") \}" correctly "018f"


testProgramOn "call(params[1].value)" correctly "100z1"
test (expression ~ end) on "call(params[1].value)" correctly "100z1"


testProgramOn "method x1 \{foo(3)\n    bar(2)\n    bar(2)\n    foo(4)\n\}" correctly "101a1"
testProgramOn "method x1 \{foo(3)\n    bar(2)\n    bar(2)\n    foo(4)\n  \}" wrongly "101a1x"
testProgramOn "method x2 \{foo(3) bar(2) bar(2)\}" correctly "101a2"
testProgramOn "method x3 \{\n                 foo(3)\n                     bar(2)\n                     bar(2)\n                 foo(4)  \n \}" correctly "101a3"
testProgramOn "method x2 \{\nfoo\nfoo\nfoo\n}\n" wrongly "102a1"


testProgramOn "0" correctly "99z1"
testProgramOn "\"NOT FAILED AND DONE\"" correctly "99z2"

testProgramOn "print (true && \{truemeth\} && \{true\})" correctly "Eelco1"



print "Done tests"


