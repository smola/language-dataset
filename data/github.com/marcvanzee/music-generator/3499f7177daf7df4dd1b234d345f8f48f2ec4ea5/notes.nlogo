globals [
  notes
  notes_allowed
]

extensions [
  sound 
  array
]

turtles-own [
  turtle_tone    ;; the tone that the turtle represents
  other_tone     ;; the tone that the collapsing turtle represents
  speed
  do_something ;; whether this turtle should play a sound
  id
]

to setup
  clear-all
  setup-notes
end

to setup-notes
  set notes array:from-list ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]
  set-default-shape turtles "music notes 2"
  let i 0 ;; the note that the turtle represents
  let agent_id 0 ;; the id of the agent
  crt number_of_notes [
    set speed (random-float 0.5) + 0.5
    set shape "music notes 2"
    set color blue
    set size 2
    set do_something true
    setxy random-xcor random-ycor
    set heading  random 360
    set id agent_id
    set agent_id (agent_id + 1)
    set label array:item notes i
    ifelse (i = 11) [ set i 0 ] [ set i (i + 1) ]
  ]
end

to go
  ask turtles [
    if count turtles-here > 1 [ 
      collapse 
      set heading random 360
    ]
    fd speed
  ]
  tick
end

to collapse
  ;; only play chords, so only play if the 
  let other_agent turtles-here
  let num_notes array:from-list []
  ;; 0 = C, 1 = C#, 2 = D, 3 = D#, 4 = E, 5 = F, 6 = F#, 7 = G, 8 = G#, 9 = A, 10 = A#, 11 = B, 12 = C
  
  ifelse only_chords 
    [ set num_notes array:from-list [0 4 7 12 16 19 24] ] ; only the notes in a chord
    [ set num_notes array:from-list [0 2 4 5 7 9 11 12 16] ] ; all the tones in a toonladder
  ;; if sadness = 100, there is a 100% change that all the notes that can be sad will be sad.
    if sadness >= 20 [
      ifelse only_chords [ array:set num_notes 1 3 ] 
                         [ array:set num_notes 2 3 ]
    ] if sadness >= 40 [
      ifelse only_chords [ set num_notes array:from-list [0 3 7 10 12 16 19 24] ]
                         [ array:set num_notes 8 15 ]
    ] if sadness >= 60 [
      if only_chords [ array:set num_notes 5 15 ] 
    ] if sadness >= 80 [
      ifelse only_chords [ array:set num_notes 7 22 ] 
                         [ array:set num_notes 6 10]
    ]
  ifelse one_note_at_a_time 
  [ sound:stop-music
    sound:start-note instrument tone + (array:item num_notes (random (array:length num_notes))) 64 ]
  [ sound:play-note instrument tone + (array:item num_notes (random (array:length num_notes))) 64 random-float 1 ]
end

to kill_sounds
   sound:stop-music
end

to test_sound
  ;;foreach [60 64 67 69 70 69 67 64] [ 
  ;; sound:start-note instrument ? 65
  ;;  wait 0.3
  ;;  sound:stop-note instrument ?
  ;;]
  sound:play-note instrument 63 64 1
  sound:play-note instrument 67 64 1
end
@#$#@#$#@
GRAPHICS-WINDOW
271
10
944
574
25
20
13.0
1
15
1
1
1
0
1
1
1
-25
25
-20
20
0
0
1
ticks

BUTTON
9
10
127
61
NIL
setup
NIL
1
T
OBSERVER
NIL
O
NIL
NIL

BUTTON
141
10
258
61
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

INPUTBOX
10
124
257
184
number_of_notes
20
1
0
Number

SWITCH
10
388
253
421
one_note_at_a_time
one_note_at_a_time
1
1
-1000

CHOOSER
11
188
257
233
instrument
instrument
"Trumpet" "Banjo" "Jazz Electric Guitar" "Nylon String Guitar" "Acoustic Bass" "Steel Drums" "Violin" "Tuba" "Bird Tweet" "Acoustic Grand Piano" "Clavi"
9

BUTTON
141
73
258
106
all sounds off
kill_sounds
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
10
72
127
105
test sound
test_sound
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
11
312
256
345
tone
tone
0
100
54
1
1
NIL
HORIZONTAL

SWITCH
9
428
252
461
only_chords
only_chords
1
1
-1000

SLIDER
10
273
256
306
sadness
sadness
0
100
100
1
1
NIL
HORIZONTAL

@#$#@#$#@
WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

music notes 1
false
0
Polygon -7500403 true true 75 210 96 212 118 218 131 229 135 238 135 243 131 251 118 261 96 268 75 270 55 268 33 262 19 251 15 242 15 238 19 229 33 218 54 212
Rectangle -7500403 true true 120 90 135 255
Polygon -7500403 true true 225 165 246 167 268 173 281 184 285 193 285 198 281 206 268 216 246 223 225 225 205 223 183 217 169 206 165 197 165 193 169 184 183 173 204 167
Polygon -7500403 true true 120 60 120 105 285 45 285 0
Rectangle -7500403 true true 270 15 285 195

music notes 2
false
0
Polygon -7500403 true true 135 195 156 197 178 203 191 214 195 223 195 228 191 236 178 246 156 253 135 255 115 253 93 247 79 236 75 227 75 223 79 214 93 203 114 197
Rectangle -7500403 true true 180 30 195 225

music notes 3
false
0
Polygon -7500403 true true 135 195 156 197 178 203 191 214 195 223 195 228 191 236 178 246 156 253 135 255 115 253 93 247 79 236 75 227 75 223 79 214 93 203 114 197
Rectangle -7500403 true true 180 30 195 225
Polygon -7500403 true true 194 66 210 80 242 93 271 94 293 84 301 68 269 69 238 60 213 46 197 34 193 30

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
NetLogo 4.1RC5
@#$#@#$#@
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
