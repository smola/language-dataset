;;Geoff Dobson
;; CASOS - Carnegie Mellon University, 2016

;;In order to run this simulation use the GUI buttons to add terrain
;;and troops.  Then, click Go Forever.  The simulation will run for
;;100 ticks and you will find out how many vulnerabilities exist on the
;;terrain at the end of the time period by viewing the info box

globals [
  atv numATerrain numACompromised numATroops numBTroops mcRate totalmcRate
  vulsType1 vulsType2 vulsType3
  compType1 compType2 compType3
  numAterrainType1 numAterrainType2 numAterrainType3
  type1VulRate type2VulRate type3VulRate
  type1CompRate type2CompRate type3CompRate
  totalType1Vuls totalType2Vuls totalType3Vuls
  totalType1Comp totalType2Comp totalType3Comp
  ]

breed [alphaTerrains alphaTerrain]
;;breed [alphaOCOTroops alphaOCOTroop]
breed [alphaDCOTroops alphaDCOTroop]
breed [bravoOCOTroops bravoOCOTroop]

directed-link-breed [defensiveOps defensiveOp]
directed-link-breed [offensiveOps offensiveOp]

alphaTerrains-own [
  vul           ;; is vulnerable?
  attack        ;; is being attacked?
  comp          ;; is this system compromised by enemy
  terType       ;; type of the terrain: networking=1, server=2, user=3
]

;;links-own [
;;  attack
;;]

;;
;; Setup Procedures
;;

to setup
  clear-all
  set atv 0
  set vulsType1 0
  set vulsType2 0
  set vulsType3 0
  set compType1 0
  set compType2 0
  set compType3 0
  set numAterrain 0
  set numAterrainType1 0
  set numAterrainType2 0
  set numAterrainType3 0
  set type1VulRate 0
  set type2VulRate 0
  set type3VulRate 0
  set type1CompRate 0
  set type2CompRate 0
  set type3CompRate 0
  set numAtroops 0
  set numBtroops 0
  set mcRate 100
  set totalmcRate 0
  set totalType1Vuls 0
  set totalType2Vuls 0
  set totalType3Vuls 0
  set totalType1Comp 0
  set totalType2Comp 0
  set totalType3Comp 0

  set-default-shape alphaDCOTroops "person"
  set-default-shape bravoOCOTroops "person"

  reset-ticks
end


;;
;; Runtime Procedures
;;

to go
  ask links [die]

  generateVuls
  defendTerrain
  attackTerrain
  determineCompromise

  updateDashboard

  if ticks >= 720 [

    let avgType1VulRate 0
    set avgType1VulRate (totalType1Vuls / 720 )
    let avgType2VulRate 0
    set avgType2VulRate (totalType2Vuls / 720 )
    let avgType3VulRate 0
    set avgType3VulRate (totalType3Vuls / 720 )

    let avgType1CompRate 0
    set avgType1CompRate (totalType1Comp / 720 )
    let avgType2CompRate 0
    set avgType2CompRate (totalType2Comp / 720 )
    let avgType3CompRate 0
    set avgType3CompRate (totalType3Comp / 720 )

    let avgMCRate 0
    set avgMCRate (totalmcRate / 720 )

    file-open "BaseRandom1Percent.txt"
    ;file-type avgType1VulRate
    ;show avgType1VulRate
    ;file-type ","
    file-type avgType2VulRate
    show avgType2VulRate
    ;file-type ","
    ;file-type avgType3VulRate
    ;show avgType3VulRate
    ;file-type ","
    ;file-type avgType1CompRate
    ;file-type ","
    file-type avgType2CompRate
    file-type ","
    ;file-type avgType3CompRate
    ;file-type "\r"


    ;file-type avgMCRate
    file-type "\r"

    file-close
    stop]

  tick
end

to generateVuls
 let temp 0

 ask alphaTerrains [
     if terType = 1
      [
        if vul = 0
        [
          let r1 0
          set r1 random 100
          ;;show r1
          if environment = "base"
          [
            if r1 < 4 [ set vul 1 set color yellow ]
          ]
          if environment = "tactical"
          [
            if r1 < 7 [ set vul 1 set color yellow ]
          ]
          if environment = "industrial"
          [
            if r1 < 14 [ set vul 1 set color yellow ]
          ]
        ]
      ]
      if terType = 2
      [
        if vul = 0
        [
          let s1 0
          set s1 random 100
          ;;show s1
          if environment = "base"
          [
            if s1 < 7 [ set vul 1 set color yellow ]
          ]
          if environment = "tactical"
          [
            if s1 < 27 [ set vul 1 set color yellow ]
          ]
          if environment = "industrial"
          [
            if s1 < 14 [ set vul 1 set color yellow ]
          ]
        ]
      ]
      if terType = 3
      [
        if vul = 0
        [
          let t1 0
          set t1 random 100
          ;;show t1
          if environment = "base"
          [
            if t1 < 59 [ set vul 1 set color yellow ]
          ]
          if environment = "tactical"
          [
            if t1 < 30 [ set vul 1 set color yellow ]
          ]
          if environment = "industrial"
          [
            if t1 < 15 [ set vul 1 set color yellow ]
          ]
        ]
      ]

 ]

 ask alphaTerrains [ set temp (temp + vul)] ;;iterate through all terrain adding vul to temp
 set atv temp

end

to test1

end

to addAlphaDCOTroop ;; Adds DCO forces

  let xPos (numATroops * 3 - 8)

  create-alphaDCOTroops 1 [
     set color green
     setxy  xPos 28
   ]

  set numATroops numATroops + 1
end

to addBravoOCOTroop ;; Adds attacker

  let xPos (numBTroops * 3 - 8)

  create-bravoOCOTroops 1 [
     set color blue
     setxy  xPos -8
   ]

  set numBTroops numBTroops + 1
end

to defendTerrain

 let t1 0
 let t2 0
 set t1 ticks mod 3
 ;;show ticks
 ;;show t1
 set t2 ticks mod 6
 ;;show t2

 carefully [ ask alphaDCOTroops [ create-defensiveOp-to one-of other alphaTerrains with [vul = 1 AND terType = 3] ] ] [print error-message ]

 if t1 = 0 [
  carefully [ ask alphaDCOTroops [ create-defensiveOp-to one-of other alphaTerrains with [vul = 1 AND terType = 2] ] ] [print error-message ]
 ]

 if t2 = 0 [
  carefully [ ask alphaDCOTroops [ create-defensiveOp-to one-of other alphaTerrains with [vul = 1 AND terType = 1] ] ] [print error-message ]
 ]



 ask alphaTerrains with [any? my-in-links] [ set vul 0 set comp 0 set color brown ]

end

to attackTerrain
   ;;carefully [ ask bravoOCOTroops [ create-offensiveOp-to one-of other alphaTerrains ] ] [print error-message ]
  ask bravoOCOTroop 0 [
    if attack1Type = "Random" [
      create-offensiveOp-to one-of alphaTerrains
    ]
    if attack1Type = "DOS" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 2 ]
    ]
    if attack1Type = "RPA" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 1 ]
    ]
    if attack1Type = "Phishing" [
      create-offensiveOp-to one-of alphaTerrains with [terType = 3 ]
    ]
  ]

  ask bravoOCOTroop 1 [
    if attack2Type = "Random" [
      create-offensiveOp-to one-of alphaTerrains
    ]
    if attack2Type = "DOS" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 2 ]
    ]
    if attack2Type = "RPA" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 1 ]
    ]
    if attack2Type = "Phishing" [
      create-offensiveOp-to one-of alphaTerrains with [terType = 3 ]
    ]
  ]

  ask bravoOCOTroop 2 [
    if attack3Type = "Random" [
      create-offensiveOp-to one-of alphaTerrains
    ]
    if attack3Type = "DOS" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 2 ]
    ]
    if attack3Type = "RPA" [
      create-offensiveOp-to one-of alphaTerrains with [ terType = 1 ]
    ]
    if attack3Type = "Phishing" [
      create-offensiveOp-to one-of alphaTerrains with [terType = 3 ]
    ]
  ]


end

to determineCompromise

  ;show count offensiveOps

  ask offensiveOps [
     ask end2[
        let o1 0
        set o1 random 100
        ;show o1
        if vul = 1 [
          if o1 < 8 [
            set comp 1
            set color red
          ]
       ]
     ]
    ]
end


to updateDashboard
  set numATerrain count alphaTerrains
  ;;show numATerrain
  set numACompromised count alphaTerrains with [ comp = 1 ]
  ;;show numACompromised
  set mcRate (((numATerrain - numACompromised) / numATerrain) * 100)
  set totalmcRate mcRate + totalmcRate
  ;;show mcRate

  set numATerrainType1 count alphaTerrains with [terType = 1]
  set numATerrainType2 count alphaTerrains with [terType = 2]
  set numATerrainType3 count alphaTerrains with [terType = 3]

  set vulsType1 count alphaTerrains with [terType = 1 AND vul = 1 ]
  set vulsType2 count alphaTerrains with [terType = 2 AND vul = 1 ]
  set vulsType3 count alphaTerrains with [terType = 3 AND vul = 1 ]

  set compType1 count alphaTerrains with [terType = 1 AND comp = 1 ]
  set compType2 count alphaTerrains with [terType = 2 AND comp = 1 ]
  set compType3 count alphaTerrains with [terType = 3 AND comp = 1 ]

  set type1VulRate ((vulsType1 / numATerrainType1) * 100)
  set type2VulRate ((vulsType2 / numATerrainType2) * 100)
  set type3VulRate ((vulsType3 / numATerrainType3) * 100)

  set totalType1Vuls (totalType1Vuls + type1VulRate)
  set totalType2Vuls (totalType2Vuls + type2VulRate)
  set totalType3Vuls (totalType3Vuls + type3VulRate)

  set type1CompRate ((compType1 / numATerrainType1) * 100)
  set type2CompRate ((compType2 / numATerrainType2) * 100)
  set type3CompRate ((compType3 / numATerrainType3) * 100)

  set totalType1Comp (totalType1Comp + type1CompRate)
  set totalType2Comp (totalType2Comp + type2CompRate)
  set totalType3Comp (totalType3Comp + type3CompRate)

end

to addThreeTierTerrain
  let space numATerrain * 3
  let x space - 8
  let j 0
  let xPos 0

while [ j <= 1 ] [
      set j j + 1
      set xPos (j * 2 - 5)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos 16
      set vul 0
      set terType 1
      set shape "box"
      ]
    ]

set j 0
while [ j <= 3 ] [
      set j j + 1
      set xPos (j * 2 - 7)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos 12
      set vul 0
      set terType 1
      set shape "box"
      ]
    ]

set j 0
while [ j <= 6 ] [
      set j j + 1
      set xPos (j * 2 - 11)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos 8
      set vul 0
      set terType 1
      set shape "box"
      ]
    ]

set j 0
while [ j <= 20 ] [
      set j j + 1
      set xPos (j * 2 - 23)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos 4
      set vul 0
      set terType 2
      set shape "square"
      ]
    ]

set j 0
while [ j <= 30 ] [
      set j j + 1
      set xPos (j * 2 - 33)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos 0
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]
end

to addMobileDismountTerrain
  let space numATerrain * 3
  let x space - 8
  let j 0
  let xPos 0
  let yPos 0

while [ j < 2 ] [
      set j j + 1
      set xPos -24
      set yPos (j * 2)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]

set j 0

while [ j < 2 ] [
      set j j + 1
      set xPos -21
      set yPos (j * 2)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 2
      set shape "square"
      ]
    ]

set j 0

while [ j < 2 ] [
      set j j + 1
      ;;set xPos (j * 2 - 15)
      set xPos -18
      set yPos (j * 3)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 1
      set shape "box"
      ]
    ]

set j 0

while [ j < 6 ] [
      set j j + 1
      ;;set xPos (j * 2 - 15)
      set xPos -15
      set yPos (j * 3)

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 1
      set shape "box"
      ]
    ]

set j 0

while [ j < 13 ] [
      set j j + 1
      set xPos (j * 2 - 13)
      set yPos 15

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]

set j 0

while [ j < 7 ] [
      set j j + 1
      set xPos (j * 2 - 13)
      set yPos 11

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]

set j 0


while [ j < 7 ] [
      set j j + 1
      set xPos (j * 2 - 13)
      set yPos 7

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]

set j 0

while [ j < 4 ] [
      set j j + 1
      set xPos (j * 2 - 13)
      set yPos 3

      create-alphaTerrains 1 [
      set color brown
      setxy xpos yPos
      set vul 0
      set terType 3
      set shape "square 2"
      ]
    ]

set j 0
end
@#$#@#$#@
GRAPHICS-WINDOW
290
10
1010
751
35
35
10.0
1
10
1
1
1
0
1
1
1
-35
35
-35
35
1
1
1
ticks
15.0

BUTTON
13
93
91
126
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
102
93
193
126
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
17
42
119
75
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
17
165
286
198
Load Three Tier Distribution Terrain
addThreeTierTerrain
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
347
175
380
Add 1 DCO Troop
addAlphaDCOTroop
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
20
264
158
309
environment
environment
"base" "tactical" "industrial"
0

PLOT
1564
42
1853
286
Mission Capability Rate
time
MC Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot MCRate"

BUTTON
10
461
172
494
Add Attacker
addBravoOCOTroop
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
18
212
246
245
Load Mobile Dismount Terrain
addMobileDismountTerrain
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1026
41
1176
66
Terrain Type 1
20
0.0
1

MONITOR
1165
30
1271
83
Total Systems
numATerrainType1
17
1
13

PLOT
1015
94
1257
245
Terrain Type 1 Vul Rate
time
Vul Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type1VulRate"

PLOT
1285
93
1533
243
Terrain Type 1 Compromise Rate
time
Comp Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type1CompRate"

TEXTBOX
1023
287
1173
312
Terrain Type 2
20
0.0
1

MONITOR
1162
276
1268
329
Total Systems
numATerrainType2
17
1
13

PLOT
1015
344
1262
495
Terrain Type 2 Vul Rate
time
Vul Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type2VulRate"

PLOT
1287
344
1532
499
Terrain Type 2 Compromise Rate
time
Comp Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type2CompRate"

TEXTBOX
1021
520
1171
545
Terrain Type 3
20
0.0
1

MONITOR
1167
511
1273
564
Total Systems
numATerrainType3
17
1
13

PLOT
1015
579
1272
736
Terrain Type 3 Vul Rate
time
Vul Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type3VulRate"

PLOT
1290
576
1545
737
Terrain Type 3 Compromise Rate
time
Comp Rate
0.0
720.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot type3CompRate"

CHOOSER
35
510
173
555
attack1Type
attack1Type
"Random" "DOS" "RPA" "Phishing"
1

CHOOSER
37
572
175
617
attack2Type
attack2Type
"Random" "DOS" "RPA" "Phishing"
1

CHOOSER
38
636
176
681
attack3Type
attack3Type
"Random" "DOS" "RPA" "Phishing"
1

@#$#@#$#@
## WHAT IS IT?

This program is an example of a two-dimensional cellular automaton.  If you are not already familiar with 2D CA, see the model "Life" for a basic discussion.

Typical CAs use two cell states (live and dead), but Brian's Brian uses three: firing (white), refractory (red), and dead (black).

This CA is especially interesting to watch because it has many configurations that move steadily across the grid (as opposed to Life, which has only relatively few such configurations).

## HOW IT WORKS

Firing (white) cells always become refractory (red) at the next time step.

Refractory (red) cells always die (turn black) at the next time step.

A new firing (white) cell is born in any black cell that has exactly two firing (white) neighbors (of its eight surrounding cells).

## HOW TO USE IT

The INITIAL-DENSITY slider determines the initial density of cells that are firing.  SETUP-RANDOM places these cells.  GO-FOREVER runs the rule forever.  GO-ONCE runs the rule once.

If you want to draw an initial pattern yourself, or alter the pattern in the middle of a run, turn on the DRAW WHITE CELLS or DRAW RED CELLS button, then "draw" and "erase" with the mouse in the view.

## THINGS TO NOTICE

Lots of patterns stay stable and move steadily across the grid.  Such patterns are often referred to as "gliders".  How many different types of gliders do you see?  Why does this happen?  How do the rules of the CA result in this behavior?

## THINGS TO TRY

Are there any stable shapes that don't move?

Are there any "glider guns" (objects that emit a steady stream of gliders)?

On a small enough grid, usually the CA reaches a steady state where there may be movement but nothing new happens.  In Brian's Brain, a square grid usually reaches a steady state more quickly than a rectangular grid (try it!).  Why?

## EXTENDING THE MODEL

Many other interesting 3-state 2D automata exist.  Experiment with variations on the rules in this model.

## RELATED MODELS

See all of the other models in the Cellular Automata subsection of the Computer Science section of the NetLogo Models Library.

## CREDITS AND REFERENCES

Brian's Brain was invented by Brian Silverman.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (2002).  NetLogo Brian's Brain model.  http://ccl.northwestern.edu/netlogo/models/Brian'sBrain.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2002 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227.

<!-- 2002 -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
setup-random
repeat 67 [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
