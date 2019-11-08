 z←layernum glw x;w;hhat0;nin;b;hhat;hhatinput;li;updates;numlayers;lr;input
 numlayers←⊃x[6]

 hhat0←⊃x[1] ⍝ use row of input as posterior
 b←⊃x[3] ⍝ biases
 w←⊃x[2] ⍝ weights
 lr←⊃x[4]
 nin←⊃x[5]

 ⍝ calculate hidden layer posterior
 :If 1=((layernum>1)∧(layernum≤numlayers))
     hhat←1 calclatesthhat(layernum)(b)(hhat0)(w)
     hhatinput←hhat
     li←(0)(layernum)(4)  ⍝ CD 4
 :Else
     hhatinput←hhat0
     li←(0)(layernum+1)(1) ⍝ indexing from 1 in APL
 :EndIf
 ⍝ create the input nested array here
 input←(hhatinput)(w)((numlayers,nin)⍴b)(lr)

 ⍝ CD here
 ⍝ update structure : (v1)(w)(b) - v1 is the visible
 ⍝ layer output from CD
 updates←li kcontdiv input
 updates←updates,(nin)(numlayers)
 :If layernum=numlayers
     z←updates
 :Else
     z←(layernum+1)glw updates
 :EndIf
