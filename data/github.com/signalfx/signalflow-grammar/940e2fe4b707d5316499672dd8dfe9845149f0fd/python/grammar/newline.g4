/**
 * Copyright (C) 2016 SignalFx, Inc.
 */
NEWLINE
 : ( {self.atStartOfInput()}?   SPACES
   | ( '\r'? '\n' | '\r' ) SPACES?
   )
   {
newLine = re.sub("[^\r\n]+", "", self.text)
whiteSpaces = re.sub("[\r\n]+", "", self.text)
next = self._input.LA(1)
if self.opened > 0 or next == '\r' or next == '\n' or next == '#':
    # If we are inside a list or on a blank line, ignore all indents,
    # dedents and line breaks.
    self.skip()
else:
    self.emitToken(self.commonToken(SignalFlowV2Lexer.NEWLINE, newLine));
    indent = SignalFlowV2Lexer.getIndentationCount(whiteSpaces);
    previous = 0 if not self.indents else self.indents[-1]
    if indent == previous:
        # skip indents of the same size as the present indent-size
        self.skip()

    elif indent > previous:
        self.indents.append(indent)
        self.emitToken(self.commonToken(SignalFlowV2Parser.INDENT, whiteSpaces))
    else:
        # Possibly emit more than 1 DEDENT token.
        while self.indents and self.indents[-1] > indent:
            self.emitToken(self.createDedent())
            self.indents.pop()
    }
 ;
