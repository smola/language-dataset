NB. FUNCS gives the available functions to be used on text.
NB. each should take and return a string.
onlines =: (;._2)(@:(,LF#~LF~:{:))
lines =: <onlines :. unlines
unlines =: ;@:(,&LF&.>) :. lines
aslines =: (<@:) onlines (-.&a: @:) ((}:@:unlines)@:)

execute =: ":@". aslines
assign =: ''[(''[".)onlines
latexlines =: latex aslines @: (rplc&(' ';~'\',LF))
getstring =: ('' [ {.~ 4 :'(x) =: y' (}.~>:)) i.&LF
cocurrent 'pjdoc'
  J =: execute_base_
  A =: assign_base_
  L =: latexlines_base_
  P =: pd_base_
  S =: getstring_base_
  I =: '$','$',~latexlines_base_
  D =: '\[','\]',~ latexlines_base_`({:,~latexlines_base_@:}:)@.(',.'e.~{:)
cocurrent 'base'

NB. execute \?(expr) using the correct function for ? .
ex_jdoc =: 3 :0
  'func data' =. (({.~;}.~) i.&'(') y
  , ('_pjdoc_',~}.func)~  process_jdoc }.}:data
)

NB. take the text of a document and process it.
process_jdoc =: 3 :0
  getfuncs =. ('\',,&'(')&.> @: nl_pjdoc_
  nest =. +/\ 1 _1 0 {~ '()'i. text =. y
  ptext =. ''
  while. (start =. <./ ; (getfuncs'') (1 i.~E.)&.> <text) < #text
  do.
    ptext =. ptext,add ['add text'=.start ({.;}.) text
    nest =. start}.nest
    length =. >: nest ((i.{:)@:|.~ + ]) '('i.~ text
    ptext =. ptext,add ['add text'=.length (ex_jdoc@:{. ; }.) text
    nest =. length}.nest
  end.
  ptext,text
)
