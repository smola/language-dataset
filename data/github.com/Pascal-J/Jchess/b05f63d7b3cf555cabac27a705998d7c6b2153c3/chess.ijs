amV_z_ =: (0 {:: [)`(1 {:: [)`]}  NB. dyad amend
boxscan =: &.>/(>@:) NB. applies u/ to any shape x and any shape y, as long as they are joined <x,<y
reduce =: 1 : '<"_1@[ ([: u boxscan ,) <@:]'

bmp =: (,. 4 4 <@$("2 0) 0 4) ,.~ (,:  4:^:(=&0)"0 leaf)   , <"2 (,  3:^:(=&1)"0) ((] ,: 2 (<3 3)} ])@{. , }.) 4 4 ($"1) 1 1 1 1 0 1 1 0 0 1 1 0 0 1 1 0 , 0 1 1 0 1 1 1 0 0 1 0 0 1 1 1 0 , 0 1 1 0 1 1 1 1 0 1 1 0 0 1 1 0 , 1 1 1 1 0 1 1 0 0 1 1 0 1 0 0 1 , 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 , 0 0 0 0 0 1 1 0 0 1 1 0 1 1 1 1 ,: 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0
bmp2 =: (,. 4 5 <@$("2 0) 0 4) ,.~ (,: 4:^:(=&0)"0 leaf) , <"2 (, 3:^:(=&1)"0) ((] ,: 2 (<3 4)} ])@{. , }.) 4 5 ($"1) 1 0 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 , 0 1 1 1 0 1 1 1 1 0 0 0 1 0 0 1 1 1 1 0 , 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 1 0 0 , 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 0 0 0 1 , 0 0 1 0 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 , 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 1 1 0 ,: 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 0
cbraw =. ( 2 | +/&i.~)8
border =: (a:, ;: ' a b c d e f g h') ,~ (|. <"0 >: i.8) ,. ]
toascii =: >@:(;@:(('.' #~ ".)^:(e.&'12345678')each) leaf@('/'&cut)) 
tobmp =: (8 8$' @sO.'{~leaf(,cbraw){"0 1|:@[{~[:,'rsnbqkpgRSNBQKPG.'i.])
ghostclear =: (] [`(amV~)@.(0< #@])  'Rr.' (] ,&<~ [ {~ 7 0 i."1 0 {.&>@])^:(0 <#@]) 4 <"1@$. $.@:(e.&'gG'"0))
ghostpawnCap =: (] amV~ '.' (; <) (>:@{. , {:)L:0@{:@[)`(] amV~ '.' (; <) (<:@{. , {:)L:0@{:@[)`]@.(2 <. (2 * 'pP' -.@e.~ {.@:(0 {:: [)) + 'gG' i. {:@:(0 {:: [))
ghostpawnAdd =: ]`(] amV~ ( (1 {:: [) <@:+ 0 ,~ _2 %~ (-/)@:({.every)@:}.@[) ,&<~ 'gG'{~'pP'i.{.@:(0{::[))@.(('pP'e.~{.@:(0{::[))*.2=|@(-/)@:({.every)@:}.@[)

moves=. ,/ > 'x-' ( [: ('87654321'&i.@{: , 'abcdefg'&i.@{.) each '-' cut rplc~) leaf > ' ' cut each  cutLF 0 : 0
e2-e4 c7-c5
f1-c4 g8-f6
c4xf7 e8xf7
e4-e5 d7-d5
e5xd6
)

   islowerg =: e.&'bnprgskq'
   isupperg =: e.&'BNPRGSKQ'
   islower =: e.&'bnprskq'
   isupper =: e.&'BNPRSKQ'
NB. capture routines x is from square idx, y is board. returns list of indexes that can be captured
Rcuts =: (1 (0}) each (8#0) <@:amV~("1)  1 ;"0 ])
Rspace =: (|:@:[ {~ {:@]) ; [ {~ {.@]
idxs =:(4$.$.)@:-.@i.
NB. returns maxlengths < > ^ V where infinity means till edge of board.
Rdirs =: ([: ;@;"1@((<0)&,^:(1=#)L:1)  ((']';'<:'){~(,-.)@(islower@[)) ((<./&:>L:1)@:(<./&:>leaf)@:(>:@(_:^:(0=#))@apply each))L:1(I.@islower;I.@isupper)@:((|.@]`(}.@]))@.([={.@]))leaf)
RdirsM =: ( 1 0 1 0 <;.(1) _1 1 _1 1 * Rdirs)"1
bounds =: 7&<.@(0&>.)
maxidx =: bounds leaf@(|."1@[ (<"1@(,. >)&{: , <"1@(,.~ >)&{.)"1 + each) 
Rmax =: ( idxs maxidx  [ RdirsM idxs ((Rcuts<;.1 each Rspace)~)"1 _ ])

Bspace =: ([ (] #~ e.&>) </.@:(|."1)@]) ,"0 [ (] #~ e.&>) </.@]
NB. returns maxlengths \< \> /> /< where infinity means till edge of board.
BdirsM =: (   [ > L:1@(<./@:,&:> L:1)@:( (({: ,~ >:leaf@{.)@] L:1)`(({. , >:leaf@{:)@] L:1)@.(islower@[)) ('|.leaf@]' ;'(}.leaf@])')  {.leaf@(_:^:(0=#)leaf)@(I.@islower;I.@isupper)@:apply each L:1 [(a:(_2{.,)]<;.1~1(0})-.@i.)leaf Bspace)"1 _
backtrack =: 2 : '] ([`]@.v"_) u' 
untilOB =: 2 : ' u backtrack(*./@:(_1&<)@] *. *./@:(8&>)@])(^:n)'
untilOBorCap =: 2 : ' u backtrack(*./@:(_1&<)@] *. *./@:(8&>)@])(^:n)'
Bmax =: (|:@:(<"1)@((_1 _1;1 1;_1 1;1 _1)(,."0 1)<"1@idxs) 4 :' a + untilOB y b  [ ''a b''=. x'  each"1 1 ;"1@BdirsM)

NB. all valid Night jumps
Nmax =: ((_2 1 ; _2 _1 ;2 1 ; 2 _1 ; 1 2 ; 1 _2 ; _1 2 ; _1 _2) (#~ (*./@:(_1&< ) *. *./@:(8&>))every)"1@|:@:(+ each"0 1) <"1@idxs)
Pmax =:  ( <"1@idxs (#~ (*./@:(_1&< ) *. *./@:(8&>))every)"1@(+ each"0 1) ( (_1 _1 ;_1 1) ,: 1 _1 ;1 1) {~  islower@[)

checksBlack =: ((] (] #~"1 'P' e.~ {~) Pmax) ,&, (] (] #~"1 'N' e.~ {~) Nmax) ,&, (] (] #~"1 'BQ' e.~ {~) Bmax) ,&, ] (] #~"1 'RQ' e.~ {~) Rmax)
checksWhite =: ((] (] #~"1 'p' e.~ {~) Pmax) ,&, (] (] #~"1 'n' e.~ {~) Nmax) ,&, (] (] #~"1 'bq' e.~ {~) Bmax) ,&, ] (] #~"1 'rq' e.~ {~) Rmax)

pieceandpositions =: 1 : '( ({~ ;"0 ]) (<"1)@(4 $. $.)@:(e.&m))'
pieceandpositionsV =: (] ({~ ;"0 ])  (<"1)@(4 $. $.)@:(e.~))
Rmax2 =: 1 : '(m pieceandpositions ((1{::[)maxidx(0{::[)RdirsM(1{::[)((Rcuts<;.1 each Rspace)~)"1 _])"1 _ ])'
Rmoves =: <@(3 : '(P;f) , < (<f) -.~ ~. ; <"1 each ((0 _1 ;l);(0 1 ;r);(_1 0 ;u);<(1 0 ; d))   (] , {:@] + 0 {:: [)^:({:@] -.@-:  1{::"1[)^:_ each   < ,: f [''P f l r u d'' =. y'"1)
RmaxM =: 1 : 'm pieceandpositions ,"1 m Rmax2'
amove =: (]amV reduce~ [|."1@:(;"0)'.'(,{.){)  NB. x is fromidx, toidx
buildmoves =:  >@(,&.>/)@:((1&{ (,"0) 2&({::)) each)
buildmoves2 =:  >@(,&.>/)@:((#~ 0 < # every)@:( (1&{ (,"0) 2&({::))&.>))
Rcheck =: 4 : '  (] (] #~ (toupper`]@.(*./@:isupper x) ''k'') #@(checksWhite`checksBlack@.(*./@:isupper x))"1 2  amove~"_ 1) buildmoves@:Rmoves@(x RmaxM)) y'

BdirsM2 =: (0 {::"1 [) BdirsM ]


Bmax3=:  (|:@:(<"1)@((_1 _1;1 1;_1 1;1 _1)(,."0 1)<"1@(1{:: [)) 4 :' a + untilOB y b  [ ''a b''=. x'  each"1 1 ;"1@{.@:(BdirsM2"1 _) )
Bmax2=: 1 : 'm pieceandpositions Bmax3"1 _ ]'
BmaxM =: 1 : 'm pieceandpositions ,"1 m Bmax2'
addBs =: 4 : ' a + untilOB y b  [ ''a b''=. x'
Bmax8 =: 4 : '(  |:@:(<"1)@((_1 _1;1 1;_1 1;1 _1) ,."0 1 <"1@(1 {::"1 x pieceandpositions)) addBs each"1 1  (a:, a:) ;"1@-.~  x ,/@:>@:(({."1)@:(_2&(<\)))@:( BdirsM"0 _) ]) y'
BmaxM =: 1 : '(m pieceandpositions ,"1 (m Bmax8 ]))'
BdirsM4 =: (<"0@[ ([ >L:1@(<./@:,&:>L:1)@:(({: ,~ >:L:0@{.)@]L:1`(({. , >:L:0@{:)@]L:1)@.(islower@[)) (<;._1 ' |.leaf@] (}.leaf@])') {.L:0@(_:^:(0 = #)L:0)@(I.@islower ; I.@isupper)@:apply&.>L:1   [ (a: (_2 {. ,) ] <;.1~ (1) 0} -.@i.)L:0  Bspace)each ]) boxopen

cleanowncolour =: (<@[ ({.@] , (1 { ]) (,<) (2{::]) #~ [ -.@isupper@{~  2 {::])`({.@] , (1 { ]) (,<) (2{::]) #~ [ -.@isupper@{~  2 {::])@.(0 isupper@{:: ])each ])  NB. x is board y is BRNPK-moves
Bcheck2 =: 4 : '  (] (] #~ (toupper`]@.(*./@:isupper x) ''k'') #@(checksWhite`checksBlack@.(*./@:isupper x))"1 2  amove~"_ 1) buildmoves@:(cleanowncolour Bmoves@(x BmaxM))) y'
Bcheck =: 4 : '  (] (] #~ (toupper`]@.(*./@:isupper x) ''k'') #@(checksWhite`checksBlack@.(*./@:isupper x))"1 2  amove~"_ 1) buildmoves@:(cleanowncolour Bmoves@(BmaxF))) y'
Boutcheck =: ((a:,a:) -.~ ,/)@:((('kK' {~ pD@*./@:isupper@[) (]#~0=#@((checksBlack("0 2)1&{::)`(checksWhite("0 2)1&{::"1)@.('K' = [))"1 1)](];"_1 amove~"_ 1)buildmoves@:Bmoves@BmaxF)"0 _)
cleanempty =: (<@[ ({.@] , (1 { ]) (,<) (2{::]) #~ [ -.@('.'&=)@{~  2 {::]) each ])
   Pcapmoves =: >@( ]  ( (] #~"1 '.' 0:`~:@.(2 > #@$@]) {~)L:1)   <"1@Pmax)
   Pfwd =: (] (] #~"1 '.' =  {~ ) <"1@idxs (#~ (*./@:(_1&<) *. *./@:(8&>))&>)"1@(+&.>"0 1) (_1 0;1 0) {~ islower@[)
   Pmoves =: 1 : 'm( <"1@(m pieceandpositions@]) (, <) each   a: -.~ L:1 a: -.~ [: <"1 Pfwd ,"1 Pcapmoves)y'
   Pcheck =: 4 : '  (] (] #~ (toupper`]@.(*./@:isupper x) ''k'') #@(checksWhite`checksBlack@.(*./@:isupper x))"1 2  amove~"_ 1) buildmoves2@:(] cleanowncolour x Pmoves)) y'

BUG FIXES:  oblique on transpose won't line up items for untransposed... penalty for being too cute.

BmaxF =:   4 :'pD isup =. {. *./@:isupper 0 ,@{::"1 a =.  (x pieceandpositions) y label_. (a ([, (_1 _1;1 1;_1 1;1 _1)+ backtrack((isup ~: y pD@isupper@{~ <@]) :: 0: *. *./@:(_1&<)@]*.*./@:(8&>)@]) each (_1 _1;1 1;_1 1;1 _1)+ backtrack((''.'' = y {~ <@]) :: 0: *. *./@:(_1&<)@]*.*./@:(8&>)@])^:_ each 1{[)"1 _]) y'

Routcheck =: 4 : '  (] (] #~ (toupper`]@.(*./@:isupper x) ''k'') #@(checksBlack`checksWhite@.(*./@:isupper x))"1 2  amove~"_ 1) buildmoves@:Rmoves@(x RmaxM)) y'

Pmoves =: 4 : 'x( <"1@(x pieceandpositions@]) (, <) each   a: -.~ L:1 a: -.~ [: <"1 Pfwd ,"1 Pcapmoves)y'
idxF =: 1 : '(i.@(0 0"_)`u@.(0 < #@idxs))'
Rbuild =: (4 : ' buildmoves@:Rmoves@(x RmaxM) y' idxF)
Bbuild =: ((buildmoves@:Bmoves@BmaxF)"1 _ idxF)
Bnight =: (((a:, a:) -.~ ,"2/)@:((<"0@<"1@idxs ,"0  every ] ((1 {::"1 ]) <@#~"1  (0 {::"1 ]) ~: [ islower@{~ 1 {::"1 ]) islower@[ (;<)"1 Nmax)"1 _) idxF)
Bnight =:  i.@(0 0"_)`(((a:, a:) -.~ ,"2/)@:(<"0@<"1@idxs ,"0&> ] ((1{::"1]) <@#~("1) ( 0{::"1]) ~: [ -.@islower@{~ 1{::"1]) (islower@[(;<)"1 Nmax)"1 _))@.(0 < #@idxs)
Pbuild =: (buildmoves2@:(] cleanowncolour Pmoves) idxF)
Kmax =: (_1 1;_1 _1;1 1;1 _1;1 0;0 1 ;_1 0;0 _1) (#~ (*./@:(_1&<) *. *./@:(8&>))&>)"1@|:@:(+&.>"0 1) <"1@idxs
Kbuild =:  (((a:, a:) -.~ ,"2/)@:(<"0@<"1@idxs ,"0&> ] ((1{::"1]) <@#~("1) ( 0{::"1]) ~: [ -.@islower@{~ 1{::"1]) (islower@[(;<)"1 Kmax)"1 _))
Kbuild =: {.@:(<"0@<"1@idxs ,"0&> ] ((1{::"1]) <@#~("1) ( 0{::"1]) = [ -.@islower@{~ 1{::"1 ])  (islower@[ (;<)"1 Kmax)"1 _)
legalwhite =:  1 : '(''K''  (] #~ 0 = #@(checksWhite("0 2) 1&({::))"1 1)  ] (] ; amove~)"_ 1 u)'
legalwhite =:  1 : '(''K''  ((0 {::"1]) #~ 0 = #@(checksWhite("0 2) 1&({::))"1 1)  ] (] ; amove~)"_ 1 u)'
checkblackF =: 1 : '(''k'' ((0 {::"1]) #~ 0 < #@(checksBlack("0 2) 1&({::))"1 1)  ] (] ; amove~)"_ 1 u)'
Wmove =: ('N'&Bnight , 'P'&Pbuild , 'BQ'&Bbuild , 'RQ'&Rbuild) legalwhite
Bmove =: ('k'&Kbuild , 'n'&Bnight , 'p'&Pbuild , 'bq'&Bbuild , 'rq'&Rbuild) legalblack

MoveTree =: ([:(a:-.~,)@:(((0&{((<'m')<@,~[)`(((1{::"1]);~"2 1[,0{::])each)@.(0<#@])(](]<@;amove~)"_ 1 Bmove)@(1&{::))"1)every)(a:-.~,)@:>@:((0&{((1{::"1]);~"2 1[,0{::])each((]<@;amove~)"_ 1 Wmove checkblackF)@:(1&{::))each))^:(('m')*./@:(-.@-:every)1{::each])
MoveTreeR =: 1 : '((0&{::) ((1{:: each]) <"1@,"0~ [ {.@:, 0{::each])((](]<@;amove~)"_ 1 u))@:(1{::[))'
MoveTreeR5 =: 1 : '(a: -.~  [: , (<"1)@(((0&{::) ((1{:: each])  ,~"0 [ , (L:1) 0{:: each]) ((] (((_1 _1 ; _1 _1) <@; [)`((] <@; amove~)"_ 1)@.(0 < #@])) u))@:(1{::[))  every))'
MateSolver =: ((1 {::"1 >@]) amove reduce~ (|.@{.@(_2]\every(0{::each])#~('m')(-:every)1{::each])MoveTree^:_)) 
