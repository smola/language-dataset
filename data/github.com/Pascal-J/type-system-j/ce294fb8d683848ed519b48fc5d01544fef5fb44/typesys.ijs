Note 'towards a J type system'
  Core elements of a type system are coercers/convertors and validators.  
  Consider a function f that accepts a numeric temperature expected to be in Celsius.
  f '33' f '98F' f '98 Farenheit' f '98 F' f '300K' and f 33 could all be parseable into the last valid input
  Validators and coercers can use the same function as the test component.  Their difference is that 
	Coercers modify the input if they can.  
	Convertors are more complex coercers
	Validators either raise an error or bring up an input box for manual correction.
  Implementations as a verb or adverb are possible.
	Verbs can be more straightforwardly applied to x and y arguments.
	Adverbs can be chained infinitely
)

assign_z_ =: 4 : '(x) =: y'

amdt_z_ =: 2 : '(u"_ (v{ ]))`(v"_@:])`]} ]' NB. use u@] for monad amenditems, u@[ for function applied to y instead of v{y.  or u for dyad amenditem.
ORassign_z_ =: assign`(".@[)@.(_1 < 4!:0@<@[)
assignwith_z_ =: 1 : ('y assign u (y~ [ ]) :: ((i.0)"1) 1';':';'y assign x u (y~ [ ]) :: ((i.0)"1) 1')
assignwithC_z_ =: 2 : ('y assign u (y~ [ ]) :: (n"_) 1';':';'y assign x u (y~ [ ]) :: (n"_) 1 ')
NB.uucp=:u:@(7&u:)
uucp_z_ =:  ucp`]`(ucp@:u:)@.(1 i.~ 2 131072 = 3!:0)"1 :.utf8
utf8_z_ =: [: ": uucp`(uucp ::u:@:({&a.))@.([: *./ 256&>)^:(1 4 64 e.~ 3!:0)"1 :.uucp
futf_z_ =: 3 u: ":@:uucp2 :.fucp
fucp_z_ =: 3 u: uucp2 :.futf

coclass 'OOP'
OOP_z_ =: <'OOP'
coclass_z_ =: 18!:4@boxxopen@:[ ((('_OOP_' ,~ 'C' ,  ]) assign boxopen)^:(0 = L.))
coclass =: 18!:4@boxxopen@:[ ((('_OOP_' ,~  'C' ,  ]) assign (][ coerase)@:boxopen)^:(0 = L.))
codestroy_z_ =: (coerase@coname ] destroy :: ])
reassign_z_ =: ( assign [ codestroy@:[ :: ])
coinsert_z_ =: 3 : 0
n=. ;: :: ] y
 p=. ; (, 18!:2) @ boxopen each n
 p=. ~. ( 18!:2 coname''), p
(p /: p = <,'z') 18!:2 coname''
)

New =: 3 : 'y conew coname '''''
loc =: (,&'_'@[ ,&'_'@, ":@>@])"1 0
locs =: 1 : 'm loc 18!:5 '''''

inl =: (cocurrent@] ".@] [)"1 0
inlC =: 2 : 0 
 (([: <^:(0=L.) v"_) inl~ m , ' ', lr@:]) :  (([: <^:(0=L.) v"_) inl~ (lr@:[), ' ' ,m , ' ', lr@:] )
)
rifN =:  (1 : 'if. 0 = 4!:0 < ''u'' do. m&[ else. u end.')
ebN =: 4 : 'x eval rifN y'
ql=: 2 : '(quote m) , n'
ebNR =: 1 : 'm ql '' & ebN'' locs & ebN'
inlA =: 1 : 'u inlC  (18!:5 '''')'
joinB =: (1 : ' ((- # m)&}.@;@(,&m&.>"1))')(@: (] : ;))

NB. require jpath '~/zutils.ijs'
require 'format/printf'
cocurrent 'z'
pD_z_ =:  1!:2&2
eval_z_ =: 1 : ' a: 1 :  m'
hook_z_ =: 2 : '([: u v) : (u v) '
lrA_z_ =: 1 : '5!:5 < ''u'''
lr_z_ =: 3 : '5!:5 < ''y'''
linearize =: (, $~ 1 -.~ $)
dflt =: ([`]@.(0 = #@>@[)"0 0) (1 : (':'; '(linearize (#y) {. x) u y'))&(<^:(L. = 0:))
a2v =: 1 : 0 NB. for dyad adverb, where adverb takes noun arg.  ie (3 1,: 6 2) '}' a2v reduce i.5
  4 : ('''a b'' =. x label_. a (b(', u  , ')) y')
)

dvA =: 2 : ',. n ( <;.2@,~) u lrA' NB. display long verb cut by a common character such as ) ^ or @ or (.  Adds extra trailing char that must be removed if trying to reparse.
notfalse =: 0:`((0~: ]) *.(a:~:]))@.(0<#@:])(+./@:) NB. :: 0:

NB. like Fork.  Adverb creates a conjunction. whose result is (f@:[ g h@:]) g is adverb parameter. f and h are u and v of conjunction result.
Fxhy_z_ =: 1 : ' 2 : (''u@:[ '' , ''('', u lrA , '')'' , '' v@:]'')'
daS =: 2 : (' a =. (u lrA , '' u '' , n) label_. 1 : (''u  1 :'' , quote a)')  NB. allows swapping order of double adverb arguments.  ex: 'num' +: daS 'c'

coclass__OOP 'typesys'
coinsert 'OOP'
  noP =: 1 : ']'
  sfX =: 1 : '][u' 
 

 Y =: (&{::)(@:])
 Y =: 1 : 'if. 3 = 4!:0 < ''u'' do. u@:] else. m {:: ] end.'
 X =: (&{::)(@:[)
 X =: 1 : 'if. 3 = 4!:0 < ''u'' do. u@:[ else. m {:: [ end.'
multicut =: [:>[:cut leaf/ ([: <"_1 ({&a.)^:(1 4  e.~ 3!:0)@:linearize@:maybenum@:[) (, <) ]
unmulticut =: [ (([ [^:('' -: ]) leaf joinstring (L:1)) reduce) ": leaf@:] NB. to get string, level must be = to #@[.  Smaler #@[ can be useful.
unmulticut =:[ (<"_1@[ ([: >@:(([ [^:('' -: ])L:0 joinstring L:1)&.>/) ,) <@:]) ":L:0@:]
 varargs1 =: 1 : '(m ql '' =. '' , lr@:dflt) ,&< [ }.~ #@]'
 do0ret1 =: 1&{:: [ [: ". 0&{::
varargs =: 1 : '[: do0ret1 m varargs1'

tonull =: NULL"_^:(0 = #)
dvAp =: dvA ')'  NB. examples: (+: + +:)^:(5 < ]) dvAp	 ] 'num ' cp dvAp		] 'num 2&count' cp dvAp		] 'num 2&count' cp dvA '('
numerify =: 0&".^:(2 = 3!:0)
maybenum =: 0&".^:(] -:&linearize ":@:numerify)
intify =: <.@numerify
roundify =: 0.5 <.@+ numerify
inrange =: (1 X >: ]) *.&(*./) 0 X <: ]
copylistV =: <:&# *. #@] *./@:<: #@] {.(!._) [
copylist =: ((#@] {.!._ [) (] ([`]@.(0 = #@>@[)"0 0) ,@:({ ::(''"_)("0 _))) ])  NB. (] ([`]@.(0 = #@>@[)"0 0) is defaults.
raiseErr =: 4 : '0 assert~ ''forced error: '', x'
NB.  TypeName CoercionFunction ValidationTest ErrorText
sDYNTYPES =: (9{a.) cut &> cutLF 0 : 0  NB. adds empty column for parameterized type/validations
num	0&".					1 4 8 16 64 128 e.~ 3!:0	Must be numeric
int	intify					1 4 64  e.~ 3!:0		Must be integer
intx	[: x:@:intify (,&'x')^:(2 = 3!:0)		64  e.~ 3!:0		Must be extended integer (do not append x to string)
intR	roundify					1 4 64  e.~ 3!:0		Must be roundable to interger
str	":					2 = 3!:0			Must be string
box	<"_1					0<L.			Must be boxed
byteVals	a.&i.				 0 255&(inrange :: 0:) *. 1 4 e.~ 3!:0	Must be convertable to byte list
ascii	('unconvertable' raiseErr ])`utf8@.(1 4 64 131072 e.~ 3!:0)		2 = 3!:0	Must be convertable to ascii/utf8 
text	utf8`uucp@.(0 255&inrange) 			2 131072 e.~ 3!:0		Must be convertable to ascii/utf8 or unicode/superascii
short	fucp			0:`(0 65536&inrange)@.(1 4 64  e.~ 3!:0)	Must be number in range of 0 to 65536. Unicode and negative numbers will be converted.
no1dim	linearize					[: -.@:notfalse 1 e.~ $	Must not include any shapes of 1
items	'itemless' raiseErr ]			0 < #			Must not be blank
evals	3 : 'y eval'				(2 ~: 3!:0)		Must be evaluatable string (to noun) or other noun (use with coerce not validators)
cuts	cut					0<L.			Must be boxed or string will be cut on spaces
words	;:					0<L.			Must be boxed or string will be cut by words
dltb	[: dltb leaf ('str') cV leaf ]		2 32  -.@e.~ 3!:0		Will trim trailing and leading blanks at leaf level (if string) Coercer intended.
localized	3 : 'y locs'				'_'&e. *. _2 < 4!:0@:<	Name (str) must be valid and localized
uucp	uucp					131072 = 3!:0		Must be unicode
any	]					1:"_			Test will always pass. Any param.	
)
NB. separate dyadic (parametrized) typelist for clarity.  Parameters always passed as string. maybenum converts if number is correct, otherwise likely an error.
DYNTYPES =: sDYNTYPES , (9{a.) cut &> cutLF 0 : 0
count	maybenum {.Fxhy ]		maybenum =Fxhy #			count must be equal to %s
mthan	maybenum {.Fxhy ]		maybenum <Fxhy #			count must be more than %s
fthan	maybenum {.Fxhy ]		maybenum >Fxhy #			count must be fewer than %s
gthan	maybenum@[ >. ]		maybenum@:[ *./@:<: ]		Must be greater or equal than %s	
lthan	maybenum@[ <. ]		maybenum@:[ *./@:>: ]		Must be lesser or equal than %s
inrange	'unconvertable' raiseErr ]	maybenum@:[ inrange  ]		Must be within range of %s
unboxed	<@:[ cV every ]		0 = L.@:]				Must be coerceable to parameter %s (type) then unboxed
each	<@:[ cV each ]		[: *./@:; <@:[ vbV each ]		Must be coerceable to parameter %s (type) then  leaves boxed 
every	<@:[ cV every ]		[ vV"_ >@:]			Must be parameter type %s leaves boxed (use each instead normally)
d	maybenum@[		0 < #@]				Default value is %s
dv	4 : '(x eval)"_ y'		0 < #@]				Default value is verb(named if compound) '' %s ''(applied to null) or value of '' %s '' within locale if not specified
cut	maybenum@:[ multicut ]	0<L.@:]				Must be boxed or string will be repeatedly cut by %s (if list must be numberic. if numeric, numbers are ascii values)
copies	maybenum@:[ copylist ]	maybenum@:[ copylistV ]		Must have at least as many items as in(%s), and each NUMBER in %s will copy that position (if blank) from the index at that position (if it exists)
evalto	'unconvertable' raiseErr ]	4 : 'a=. y eval label_.(maybenum x) = 4!:0 <''a'''	Str expression must eval to name class %s. 0 noun, 1 adverb, 2 conjunction, 3 verb
level	<@:]^:(maybenum@:[ - L.@:])	maybenum@:[ = L.@:]			boxed Level (L.) must be %s .
)
NB. alternates:
cut	(({&a.)^:(1 4  e.~ 3!:0)@:linearize@:maybenum@:[) cut ]	0<L.@:]	Must be boxed or string will be cut by %s (if numeric, number is ascii value)

NB. version meant to duplicate above "more permissive" Dynamic Types.  Dynamic types are usually optimized for coercion efficiency.  Strict for validation correctness. 
STRICTTYPES =: (9{a.) cut &> cutLF 0 : 0
num	maybenum			([: *./ (": -: [: ": (0&".)^:(2=3!:0)) every)	Must be numeric
gthan	maybenum@[ >. ]		maybenum@:[ *./@:<: 'num' vV ] 		Must be greater or equal than %s
)

TYPES =: DYNTYPES NB. default is dynamic, as not all types may be shaddowed by STRICT


wd2=: 3 : 'wd y' NB. bug workaround
typeparser =: boxopen L:1@:(> L:1)@:([: (4 : 0)/ '&'&cut)
  (}: , (}. y),~ [:< ({. y) ,~&< {:) ;: >x
)
typeparser =: (<<,'&') -.~ each (</.~ +/\@(-.@+. _1&|.)@:=&(<,'&'))@:;:
typeparser =: (<<,'&') -.~ each (</.~ +/\@(+: _1&|.)@:=&(<,'&'))@:;:

c =: 1 : 0
a =. typeparser m
o =. ]
for_i.  a do. d =.  linearize >^:(1 < L.)^:_ i  NB.'&' cut > i
b=. ({~ ({: d) i.~ {."1) TYPES  NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. o =. ((0 Y d) (1 Y b) eval  ^: (-.@:((2 Y b) eval)) ]) :: ((3 Y b)"_) @:o f.
  else. o =. (1 Y b) eval  ^: (-.@:((2 Y b) eval)) :: ((3 Y b)"_) @:o f. end. end.
1 : ('u hook (', o f. lrA , ' ) ')
)

NB. like c but returns true hook. so if monad add @:].  Raw uncoerced input is x param
cD =: 1 : 0
a =. typeparser m
o =. ]
for_i.  a do. d =.  linearize >^:(1 < L.)^:_ i  NB.'&' cut > i
b=. ({~ ({: d) i.~ {."1) TYPES  NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. o =. ((0 Y d) (1 Y b) eval  ^: (-.@:((2 Y b) eval)) ]) :: ((3 Y b)"_) @:o f.
  else. o =. (1 Y b) eval  ^: (-.@:((2 Y b) eval)) :: ((3 Y b)"_) @:o f. end. end.
1 : ('u  (', o f. lrA , ' ) ')
)
NB. like vm, but doesn't work yet.  returns error if coerce error per item.
cm =: 1 : 0
a =. typeparser m
o =. ]
for_i.  a do. d =.  linearize >^:(1 < L.)^:_ i  NB.'&' cut > i
b=. ({~ ({: d) i.~ {."1) TYPES  NB. index error means type not defined (misspelled?) in this locale.
 e =. (3 Y b) sprintf {. d
if. 1 < # d do.
 o =. ((0 Y d) (1 Y b) eval  ^: (-. M ((2 Y b) eval)) ]) mkerr2 e @:o f.
  else. o =. (1 Y b) eval  ^: (-. M ((2 Y b) eval)) mkerr2 e @:o f. end. end.
1 : ('u hook (', o f. lrA , ' ) ')
)
NB. conjunction version uses v to either make side effect (pD or log) or preprocess y.  v is monadic
cC =: 2 : 0
a =. typeparser m
o =. ]
for_i.  a do. d =.  linearize >^:(1 < L.)^:_ i  
b=. ({~ ({: d) i.~ {."1) TYPES  NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. o =. ( (0 Y d) ((1 Y b) eval  v) ^: (-.@:((2 Y b) eval)) ]) :: ((3 Y b)"_) @:o f.
  else. o =.  (1 Y b) eval hook v ^: (-.@:((2 Y b) eval)) :: ((3 Y b)"_) @:o f. end. end.
1 : ('u hook (', o f. lrA , ' ) ')
)

NB. posts to screen when coercion is applied.
NB. first alternate shows simple presented data
NB. second alternative shows lr (type and shape) of data.  NB. comment 2nd to use first
cp =: cC (('COERCE: ' pD@:, ":) sfX)    
cp =: cC (('COERCE: ' pD@:, lr) sfX)    

v =: 1 : 0
a =. typeparser m
o =. ]
for_i. boxopen a do. d =.  linearize >^:(1 < L.)^:_ i 
b=. ({~ ({: d) i.~ {."1) TYPES    NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. e =. (3 Y b) sprintf {. d
 o =.  (e assert sfX (0 Y d) (2 Y b) eval :: 0: ])   sfX @:o f.
  else. o =.  ((3 Y b) assert sfX  (2 Y b) eval :: 0:) sfX @:o f. end. end.
1 : ('u [ (', o f. lrA , ')')
)

NB. more simply returns boolean value based on whether or not type test passed
vb =: 1 : 0
a =. typeparser m
o =. 1:
for_i. boxopen a do. d =.  linearize >^:(1 < L.)^:_ i NB. '&' cut > i
b=. ({~ ({: d) i.~ {."1) TYPES   NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. o =.  ( (0 Y d) (2 Y b) eval ]) :: 0: , o f.
  else. o =.  ( (2 Y b) eval) :: 0: @:]  , o f. end. end.
1 : ('u [:  *./ every (', o f. each lrA , ' )' )
)

ceach =: 4 : '] x c y' each
veach =: 4 : '] x v y' each
ERROR =: 0 1 0 $ _
NB. M =:  1 : 'u`(]`(u hook (;@:linearize@:}.@:]))@.((,.0) -: 0 Y))@.(1 = L.@:])'  NB. handles maybe error chains.
isErr =: ((< ERROR) -: {.@{.@:])
M =:  `](@.isErr) NB. double {. to cover case of list of errors
mkerr2 =: 2 : '(n ;~ ,.@_9:)`1:@.u :: ((13!:11 ; 13!:12)@:(''''"_))' 
mkerr2 =: 2 : '(n ;~ ERROR"_)`1:@.u :: ((ERROR ; 13!:11 ; 13!:12)@:(''''"_))' 
NB.  (0 <]) mkerr2 'not gthan 0' _4
vm =: 1 : 0
a =. typeparser m
o =. 1:
for_i. boxopen a do. d =. linearize >^:(1 < L.)^:_ i 
b=. ({~ ({: d) i.~ {."1) TYPES    NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. e =. (3 Y b) sprintf {. d
  o =.  (  (0 Y d) (2 Y b) eval  mkerr2 e ])@:]  ; o f. 
  else. o =.  ( (2 Y b) eval mkerr2 (3 Y b))@:]  ; o f.  end. end.
NB.1 : ('u  ]`[@.(1-:])chkerrA (', o f. lrA , ')')
1 : ('( [: linearize@:> (<1) -.~ (', o f. lrA , '))`u@.( 1 -: [: *./  1 -: every (', o f. lrA , '))')
)

iv =: 1 : 0  NB. gives input box to correct validation err if fail.
a =. typeparser m
o =. ]
for_i. boxopen a do. d =. linearize >^:(1 < L.)^:_ i 
b=. ({~ ({: d) i.~ {."1) TYPES   NB. index error means type not defined (misspelled?) in this locale.
if. 1 < # d do. e =. (3 Y b) sprintf {. d
 o =.  (] ( [: wd2'mb input text "' , (0 Y b) , '" "' , e ,'" *' , ":@:[)`[@.]  (0 Y d) (2 Y b) eval :: 0: ]) @:o f.
  else. o =.  (] ([:  wd2 'mb input text "' , (0 Y b) , '" "' , (3 Y b) ,'" *' , ":@:[)`[@.]  (2 Y b) eval :: 0:)  @:o f. end. end.
  NB. else. o =.  (] ([: wd2 'mb info " ' , (0 Y b) , '" "' , (3 Y b) ,' " '"_ )@:[^:-.@]  (2 Y b) eval)  @:o f. end. end.
NB. pD o lrA
 1 : ('u hook (', o f. lrA , ')')
NB. o
)

strict =: 3 : 0 '0 1&inrange' v 
NB. y is 0 or 1. sets dynamic 0 vs strict 1 types by updating table from DYNAMIC or STRICT
NB. STRICTTYPES must include matching DYNTYPES (but not vis versa)
if. y do. TYPES =: STRICTTYPES (] '}'a2v~ [ ; i.~&([: linearize {."1)) TYPES else.
	TYPES =: DYNTYPES (] '}'a2v~ [ ; i.~&([: linearize {."1)) TYPES end.
)

t =: cp  NB. shaddow mode to easily switch among type constraining/enabling behaviours.  Either globally or within class that coinserts typesys
cV =: 4 : '] x c y' 
vV =: 4 : '] x v y' 
ivV =: 4 : '] x iv y' 
tV =: 4 : '] x t y' 
ci =: [ cV ivV  NB. verb that will first give input box check for failed validations, then coerce values.
cpV =: 4 : '] x cp y' 
vbV =: 4 : '] x vb y'
vbs =: 1 : 'u 1 : ''#~ m vb'''  NB. double adverb is compatible with t. instead of coercing, filters valid items to function.
off =: 1 : 'u  1 : '' hook ]'''  NB. can be used to turn off type system.  redefine any of c, cp, v, iv, vbs to off, and all code will/should work without type checks/actions
vmV =: 4 : '] x vm y'

iD =: ] : (] (4 : 0)~ '96 59&cut 2&count'cV [)
'cap headers' =. x

  if. 0 < # headers do. y =. headers ,:`,@.(1<#@$@]) <"1&.|:^:(0 = L.) y 
  if. 0 < # cap do. y =. cap ,:&< y end. 
else. (> cap) ,:`,@.(1<#@$@]) ": y end.
)
ipD=: ][pD@:iD