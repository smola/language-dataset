NB. util

EMPTY=: i.0 0

NB. =========================================================
col=: ,.@:>L:0
firstones=: > 0 , }:
ischar=: 2 = 3!:0
issymbol=: 65536 = 3!:0
round=: [ * [: <. 0.5 + %~
roundint=: <. @ +&0.5
roundtime=: (%86400000) & round

NB. =========================================================
NB.commasep v separates boxed items with comma
commasep=: 3 : 0
if. L. y do.
  }. ; (','&, @ ":) each y
else.
  , ": y
end.
)

NB. =========================================================
NB. cut open string on ' ,', ignoring empties
cutnames=: 3 : 0
if. L.y do. y return. end.
y=. ' ',y
a: -.~ (y e. ' ,') <;._1 y
)

NB. =========================================================
qread=: 3 : 'y qtoJ 8 }. read'''''
qreads=: 3 : 'y qtoJs 8 }. read'''''

NB. =========================================================
sizetype=: 3 : 'DATASIZES {~ DATANUMS i. y'

NB. =========================================================
NB. sizevarchar
NB.
NB. reads length of varchar data starting from current position.
sizevarchar=: 3 : 0
ndx=. POS
txt=. {.a.
len=. 20 * y
whilst. ndx < #DAT do.
  blk=. len <. (#DAT) - ndx
  txt=. txt, (ndx,:blk) [;.0 DAT
  ind=. I. txt = {.a.
  if. y < #ind do. y { ind return. end.
  ndx=. ndx+len
end.
_
)

NB. =========================================================
throw=: 3 : 0
msg=. y
if. ischar msg do. msg=. 1;msg end.
thrown=: msg
throw.
)

NB. =========================================================
NB. wrap socket command
wrapcmd=: 3 : 0
if. ischar y do.
  len=. #y
  tot=. 2 ic len + 14
  typ=. 1 ic iCHAR
  len=. 2 ic len
  tot,typ,len,y
else.
  throw 'data'
end.
)
