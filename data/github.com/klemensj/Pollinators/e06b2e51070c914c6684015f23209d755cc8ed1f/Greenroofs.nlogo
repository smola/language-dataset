extensions [ gis ]

globals [landuse
         green-percent
         greenroof-percent
         init-green-percent
         ]

patches-own[land]

breed [bugs bug]  ;; turtles representing our species of interest
bugs-own[steps green-steps gray-steps]
breed [targets target]

to load-gis
  ca
  set landuse gis:load-dataset asc-filename
  gis:set-world-envelope-ds gis:envelope-of landuse
  gis:apply-raster landuse land
end

to data-out

     file-open data-filename
     file-print "asc-filename data-filename data-header vision-width vision-distance init-green-proportion green-proportion greenroof-proportion gray-steps green-steps steps paint-radius"
     file-close

end

to setup

  ask patches [
    ifelse land = 0 [ set pcolor cyan  ] [            ;; park
      ifelse land = 1 [ set pcolor green  ] [            ;; trees
        ifelse land = 2 [set pcolor yellow][             ;; grass
              ifelse land = 3 [set pcolor gray][       ;; road
                ifelse land = 4 [set pcolor blue][     ;; water
                  ifelse land = 5 [set pcolor black][  ;; roofs
      set pcolor brown
    ]]]]]]
  ]

set-default-shape bugs "butterfly"
  clear-turtles
  clear-drawing
  while[count bugs < bug-number]   ;; until we have 100 bugs
  [
  create-bugs 1  ;; create the insects
  [
    set color blue
    set size 10
    setxy max-pxcor random-ycor ;; scatter bugs along right edge
    set steps 0
    set green-steps 0
    set gray-steps 0
  ]
  ]

  calculate-percent-roofs-green
  calculate-percent-green
  set init-green-percent green-percent
  reset-ticks

end


;; This is where the functions start



to go
 calculate-percent-roofs-green ;; make sure this number is up to date before writing header to file
 random-greenroof
 data-out
 while [count bugs > 0]
[ move-bug ]
  stop

end

to move-bug
    ask bugs [
    count-steps
    if pxcor = min-pxcor [
      file-open data-filename
      file-type asc-filename
      file-type " "
      file-type data-filename
      file-type " "
      file-type  data-header
      file-write vision-width
      file-write vision-distance
      file-write init-green-percent
      file-write green-percent
      file-write greenroof-percent
      file-write gray-steps
      file-write green-steps
      file-write steps
      file-write paint-radius         ;; added this but only for analyzing blobs
      file-type "\n"
      file-close
      ]

    if pxcor = min-pxcor [die]

    set heading 270
    pen-down
    let green_target nobody
    let perceived_patches patches in-cone vision-distance vision-width
    let patch-under-me patch-here
    set green_target perceived_patches with [ (pcolor = green and self != patch-under-me) or (pcolor = cyan and self != patch-under-me)]
    ifelse  count green_target != 0 [
          let target min-one-of green_target[ distance myself ]
          let target_heading towards target
          move-to patch-at-heading-and-distance target_heading 1 ]
          [ fd 1]

     ]

end

to draw-greenroof ;; This prodcedure allows you to color certain black roofs green

  if mouse-down? [
        ask patch round mouse-xcor round mouse-ycor
            [
            set pcolor green
            ask patches in-radius paint-radius [if pcolor = black [set pcolor green] ]
            calculate-percent-roofs-green
             ]
             ]
end

to calculate-percent-green ;; This calculates green percentage of all patches at setup, before any roof painting
  let all-patches count patches
  let green-patches count patches with [pcolor = green]
  set green-percent green-patches / all-patches
end

to calculate-percent-roofs-green         ;; This procedure calculates the % greenroof to display in the monitor window on button push

  let roofs patches with [land = 5]
  let roof-number count roofs
  let roofs-number count patches with [land = 5]
  let greenroofs-number count roofs with [pcolor = green]
  if roof-number = 0 [set roofs-number 1]                  ;; added this line of code to present divide by 0 error
  set greenroof-percent  greenroofs-number / roofs-number
end

to count-steps
  set steps steps + 1
  ifelse pcolor = green [set green-steps green-steps + 1][set gray-steps gray-steps + 1] ;;Does not currently account for CYAN Park squares

end

to Random-Greenroof
  let radius paint-radius

   while[ greenroof-percent < Greenroof_Target_Proportion]
    [
      ask one-of patches with [ pcolor = black ] [
      set pcolor green
      ask patches in-radius paint-radius [if pcolor = black [set pcolor green] ]
     ]
 calculate-percent-roofs-green
 calculate-percent-green
    ]
end

to paint-guide

set-default-shape targets "target"
ask one-of patches with [ pcolor = black ] [
  sprout-targets 1
   [
    set breed targets
    set color white
    set size 20

  ]]
end

to clear-targets
  ask targets
  [die]
end

;; 0 = Parks
;; 1 = Tree Canopy
;; 2 = Grass/Srhub/Bare Soil
;; 3 = Urban Matrix (Roads, Railroads, Parking Lots, etc.)
;; 4 = Water
;; 5 = Buildings
@#$#@#$#@
GRAPHICS-WINDOW
205
10
1216
1042
500
500
1.0
1
10
1
1
1
0
0
0
1
-500
500
-500
500
0
0
1
ticks
30.0

BUTTON
-1
69
196
102
NIL
load-GIS
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
115
238
181
271
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
35
584
154
617
Run the Model
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
290
182
323
vision-width
vision-width
1
179
120
1
1
NIL
HORIZONTAL

SLIDER
10
324
182
357
vision-distance
vision-distance
0
100
20
1
1
NIL
HORIZONTAL

MONITOR
19
489
174
534
prop roofs green
greenroof-percent
5
1
11

BUTTON
8
633
184
666
Draw greenroofs manually
draw-greenroof
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
19
359
174
392
paint-radius
paint-radius
1
104
15
1
1
NIL
HORIZONTAL

INPUTBOX
-1
10
196
70
asc-filename
AOI_rand_185.asc
1
0
String

INPUTBOX
-2
106
199
166
data-filename
AOI_115to185
1
0
String

INPUTBOX
-2
166
195
226
data-header
BSpace
1
0
String

BUTTON
8
667
107
700
click for guide
paint-guide
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
108
667
184
700
clear guides
clear-targets
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
19
394
174
454
Greenroof_Target_Proportion
0.5
1
0
Number

BUTTON
19
455
174
488
Set Random Greenroofs
Random-Greenroof
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
9
228
94
288
bug-number
200
1
0
Number

MONITOR
19
534
174
579
Prop patches green
green-percent
5
1
11

@#$#@#$#@
## WHAT IS IT?
This model explores how does the geometry of urban environments affect the movement patterns of hypothetical organisms.

WHYYYYY????


## HOW IT WORKS
There are two main components to this model. The first component focuses on the movement and behavior of hypothetical organisms. Butterflies represent organisms migrating through an urban area. Movement rules for the butterflies were coded as follows; the butterflies are to go west unless they see a green patch. The vision width and the vision distance can be varfied depending on the experiment. If two patches are equidistant, the butterflies choose at random.  When they spot a favorable patch in their cone of vision they are to change their heading to the closest green patch. Each butterfly records WRITES TO A FILEthe number of steps it is taking on favorable (green) vs unfavorable (every other color) as it moves through the area of interest. The data file is created and saved as a text file which can be used for analysis.

The model was one of simple favorability designation. Tree and shrub cover in the model represented favorable habitat and were coded green. All other land cover types were assumed to be unfavorable habitat. We started with a landuse and building footprint map of Philadelphia at a 1 foot scale.

The second component revolves around the manipulation of these areas of interest (AOI). You are able to set the paint brush size and overall percentage of green to add to the buildings which are represented by black in the AOI in order to montior how they change the number of steps these organisms take in favorable vs. unfavorable habitats.

## HOW TO USE IT

1) Load GIS
- type the name of your GIS date file into the "asc-filename" box in the top. All GIS file names should be in "filename.asc" format. Then hit load-GIS button.

2) Create data file name.
- Type data file name in to the "data-filename" box. This is the name of the file where all of the step data will be sent for each run.

3) Create data header
-Type the data header name into the "data-header" box. This is important to help differentiate runs from one another for analysis.

4) Number of bugs
-Change the number of bugs you are running through the simulaiton. Type the desired number of bug into the "bug number" box.

5) Hit setup
- Designated GIS file will load on the screen.

6) Change variables and Set Random Greenroofs
- Here you can change vision width, vision distance, paint radius and target green roof percent with the sliders and boxes depending on what you are trying to study.

7) Run the model
- The moment you have been waiting for. Hit the run the model button to start the simulation. Once it ends ss

* Data file will be save din the same folder as the this netlogo model. GIS AOIs need to be in the same folder as the netlogo model

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRYs

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

Making different patch categories on the landscape have different behaviors, e.g. animals don't move through buildings.

Adding building height to footprint.

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

Takes in GIS file in .asc format to create landscape.

Writes to file for each turtle

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)

GITHUB ADDRESS FOR THE MODEL
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="blobs" repetitions="5" runMetricsEveryStep="true">
    <setup>load-GIS
setup
Random-Greenroof</setup>
    <go>go</go>
    <enumeratedValueSet variable="Greenroof_Target_Proportion">
      <value value="0.11"/>
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bug-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-header">
      <value value="&quot;Blob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asc-filename">
      <value value="&quot;Black.asc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-width">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-radius">
      <value value="104"/>
      <value value="84"/>
      <value value="56"/>
      <value value="28"/>
      <value value="20"/>
      <value value="14"/>
      <value value="10"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-filename">
      <value value="&quot;rvss_blobs&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="blobs2" repetitions="5" runMetricsEveryStep="true">
    <setup>setup
Random-Greenroof</setup>
    <go>go</go>
    <enumeratedValueSet variable="Greenroof_Target_Proportion">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bug-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-header">
      <value value="&quot;Blob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asc-filename">
      <value value="&quot;Black.asc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-width">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-radius">
      <value value="10"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-filename">
      <value value="&quot;rvss_blobs&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="blobs17" repetitions="5" runMetricsEveryStep="true">
    <setup>setup
Random-Greenroof</setup>
    <go>go</go>
    <enumeratedValueSet variable="Greenroof_Target_Proportion">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bug-number">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-header">
      <value value="&quot;Blob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asc-filename">
      <value value="&quot;Black.asc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-width">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-radius">
      <value value="104"/>
      <value value="56"/>
      <value value="28"/>
      <value value="20"/>
      <value value="14"/>
      <value value="10"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-filename">
      <value value="&quot;rvss_blobs&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sweep_greenroof_percent_vision_distance_" repetitions="1" runMetricsEveryStep="true">
    <setup>load-GIS
setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="vision-width">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-distance">
      <value value="1"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Greenroof_Target_Proportion">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-radius">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-filename">
      <value value="&quot;AOI_115to185&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bug-number">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asc-filename">
      <value value="&quot;AOI_rand_115.asc&quot;"/>
      <value value="&quot;AOI_rand_125.asc&quot;"/>
      <value value="&quot;AOI_rand_127.asc&quot;"/>
      <value value="&quot;AOI_rand_129.asc&quot;"/>
      <value value="&quot;AOI_rand_130.asc&quot;"/>
      <value value="&quot;AOI_rand_153.asc&quot;"/>
      <value value="&quot;AOI_rand_156.asc&quot;"/>
      <value value="&quot;AOI_rand_172.asc&quot;"/>
      <value value="&quot;AOI_rand_173.asc&quot;"/>
      <value value="&quot;AOI_rand_185.asc&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data-header">
      <value value="&quot;BSpace&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
