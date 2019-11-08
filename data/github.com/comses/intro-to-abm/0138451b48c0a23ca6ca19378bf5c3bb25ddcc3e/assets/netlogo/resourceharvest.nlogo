globals
[
  max-resource    ; maximum amount any patch can hold
]

patches-own
[
  resource-here      ; the current amount of resource on this patch
  max-resource-here  ; the maximum amount of resource this patch can hold
]

turtles-own
[
  wealth           ; the amount of resource a turtle has
  vision           ; how many patches ahead a turtle can see
  norm-min-resource ; minimum amount of resources on patch before agent will harvest
  copied-norm
  harvestedlevel
]

;;;
;;; SETUP AND HELPERS
;;;

to setup
  clear-all
  ;; set global variables to appropriate values
  set max-resource 50
  ;; call other procedures to set up various parts of the world
  setup-patches
  setup-turtles
  reset-ticks
end

;; set up the initial amounts of resource each patch has
to setup-patches
  ;; give some patches the highest amount of resource possible --
  ;; these patches are the "best land"
  ask patches
    [ set max-resource-here 0
      if (random-float 100.0) <= percent-best-land
        [ set max-resource-here max-resource
          set resource-here max-resource-here ] ]
  ;; spread that resource around the window a little and put a little back
  ;; into the patches that are the "best land" found above
  repeat 5
    [ ask patches with [max-resource-here != 0]
        [ set resource-here max-resource-here ]
      diffuse resource-here 0.25 ]
  repeat 10
    [ diffuse resource-here 0.25 ]          ;; spread the resource around some more
  ask patches
    [ set resource-here floor resource-here    ;; round resource levels to whole numbers
      set max-resource-here resource-here      ;; initial resource level is also maximum
      recolor-patch ]
end

to recolor-patch  ;; patch procedure -- use color to indicate resource level
  set pcolor scale-color yellow resource-here 0 max-resource
end

;; set up the initial values for the turtle variables
to setup-turtles
  set-default-shape turtles "person"
  crt num-people
    [ move-to one-of patches  ;; put turtles on patch centers
      set size 1.5  ;; easier to see
      set color red
      set norm-min-resource min-resource
      set-initial-turtle-vars ]
end

to set-initial-turtle-vars
  face one-of neighbors4
  set wealth 0
  set vision 1 + random max-vision
end

;;;
;;; GO AND HELPERS
;;;

to go
  ask turtles [ turn-towards-resource ]  ;; choose direction holding most resource within the turtle's vision
  ask turtles [ fd 1]
  harvest
  ask turtles [ set wealth wealth * discount]
  monitor
  if imitate? [imitate]
  ask turtles [
    if wealth < 0 [set wealth 0 set norm-min-resource [norm-min-resource] of one-of turtles with [who != self]]]
  ask patches [ grow-resource ]
  ask turtles [set size 0.5 + 2 * norm-min-resource]
  tick
end

;; determine the direction which is most profitable for each turtle in
;; the surrounding patches within the turtles' vision
to turn-towards-resource  ;; turtle procedure
  set heading 0
  let best-direction [0]
  let best-amount resource-ahead
  set heading 90
  if (resource-ahead = best-amount) [
    set best-direction lput 90 best-direction]
  if (resource-ahead > best-amount)
    [ set best-direction [90]
      set best-amount resource-ahead ]
  set heading 180
  if (resource-ahead = best-amount) [
    set best-direction lput 180 best-direction]
  if (resource-ahead > best-amount)
    [ set best-direction [180]
      set best-amount resource-ahead ]
  set heading 270
  if (resource-ahead = best-amount) [
    set best-direction lput 270 best-direction]
  if (resource-ahead > best-amount)
    [ set best-direction [270]
      set best-amount resource-ahead ]
  ifelse best-amount > norm-min-resource [
    set heading one-of best-direction
  ][
    set heading one-of [0 90 180 270]
  ]
end

to-report resource-ahead  ;; turtle procedure
  let total 0
  let how-far 1
  repeat vision
    [ set total total + [resource-here] of patch-ahead how-far
      set how-far how-far + 1 ]
  report total
end

to grow-resource  ;; patch procedure
  if max-resource-here > 0 [
    set resource-here resource-here + regrowth-rate * resource-here * (1 - resource-here / max-resource-here)]
  recolor-patch
end

;; each turtle harvests the resource on its patch.  
to harvest
  ask turtles [set harvestedlevel 10]
  ask turtles [ if resource-here >= 1 and resource-here >= (norm-min-resource * max-resource-here) [
      set harvestedlevel resource-here / max-resource-here 
      set resource-here resource-here - 1 
      set wealth wealth + 1]
  ]
  ask turtles [recolor-patch ]
end

to imitate
  ask turtles [set copied-norm norm-min-resource]
  ask turtles [
    let otheragent one-of other turtles-here
    if otheragent != nobody [
      let ratio 0
      if [wealth] of otheragent > 0 [set ratio wealth / [wealth] of otheragent]
      let probability 1 - ratio
      if probability > 1 [set probability 1]
      if probability < 0 [set probability 0]
      if random-float 1 < probability [
        set copied-norm [norm-min-resource] of otheragent + random-normal 0 stdeverror
        if copied-norm > 1 [set copied-norm 1]
        if copied-norm < 0 [set copied-norm 0]
      ]
    ]
  ]
  ask turtles [set norm-min-resource copied-norm]
end

to monitor
  ask turtles [
    if wealth > costpunish [
      let otheragent min-one-of turtles in-radius radius [harvestedlevel] 
    
      if otheragent != nobody [
        if [harvestedlevel] of otheragent < norm-min-resource [
          set wealth wealth - costpunish
          ask otheragent [set wealth wealth - costpunished]
        ]
      ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
184
10
558
405
25
25
7.14
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
9
189
85
222
setup
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
100
189
176
222
go
go
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
9
45
177
78
max-vision
max-vision
1
15
15
1
1
NIL
HORIZONTAL

SLIDER
8
226
175
259
regrowth-rate
regrowth-rate
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
9
10
177
43
num-people
num-people
0
1000
250
1
1
NIL
HORIZONTAL

SLIDER
9
152
177
185
percent-best-land
percent-best-land
5
25
10
1
1
%
HORIZONTAL

PLOT
561
11
818
170
Resource
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [resource-here] of patches"

SLIDER
9
80
177
113
min-resource
min-resource
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
9
116
177
149
discount
discount
0
1
0.95
0.01
1
NIL
HORIZONTAL

PLOT
562
339
818
496
Wealth
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [wealth] of turtles"

SWITCH
8
262
113
295
imitate?
imitate?
0
1
-1000

SLIDER
8
299
174
332
stdeverror
stdeverror
0
0.1
0.01
0.01
1
NIL
HORIZONTAL

PLOT
562
174
818
333
Norm
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [norm-min-resource] of turtles"

SLIDER
8
335
174
368
costpunish
costpunish
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
8
371
174
404
costpunished
costpunished
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
8
407
174
440
radius
radius
0
10
10
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model simulates the distribution of wealth.  "The rich get richer and the poor get poorer" is a familiar saying that expresses inequity in the distribution of wealth.  In this simulation, we see Pareto's law, in which there are a large number of "poor" or red people, fewer "middle class" or green people, and many fewer "rich" or blue people.

## HOW IT WORKS

This model is adapted from Epstein & Axtell's "Sugarscape" model. It uses grain instead of sugar.  Each patch has an amount of grain and a grain capacity (the amount of grain it can grow).  People collect grain from the patches, and eat the grain to survive.  How much grain each person accumulates is his or her wealth.

The model begins with a roughly equal wealth distribution.  The people then wander around the landscape gathering as much grain as they can.  Each person attempts to move in the direction where the most grain lies.  Each time tick, each person eats a certain amount of grain.  This amount is called their metabolism.  People also have a life expectancy.  When their lifespan runs out, or they run out of grain, they die and produce a single offspring.  The offspring has a random metabolism and a random amount of grain, ranging from the poorest person's amount of grain to the richest person's amount of grain.  (There is no inheritance of wealth.)

To observe the equity (or the inequity) of the distribution of wealth, a graphical tool called the Lorenz curve is utilized.  We rank the population by their wealth and then plot the percentage of the population that owns each percentage of the wealth (e.g. 30% of the wealth is owned by 50% of the population).  Hence the ranges on both axes are from 0% to 100%.

Another way to understand the Lorenz curve is to imagine that there are 100 dollars of wealth available in a society of 100 people.  Each individual is 1% of the population and each dollar is 1% of the wealth.  Rank the individuals in order of their wealth from greatest to least: the poorest individual would have the lowest ranking of 1 and so forth.  Then plot the proportion of the rank of an individual on the y-axis and the portion of wealth owned by this particular individual and all the individuals with lower rankings on the x-axis.  For example, individual Y with a ranking of 20 (20th poorest in society) would have a percentage ranking of 20% in a society of 100 people (or 100 rankings) --- this is the point on the y-axis.  The corresponding plot on the x-axis is the proportion of the wealth that this individual with ranking 20 owns along with the wealth owned by the all the individuals with lower rankings (from rankings 1 to 19).  A straight line with a 45 degree incline at the origin (or slope of 1) is a Lorenz curve that represents perfect equality --- everyone holds an equal part of the available wealth.  On the other hand, should only one family or one individual hold all of the wealth in the population (i.e. perfect inequity), then the Lorenz curve will be a backwards "L" where 100% of the wealth is owned by the last percentage proportion of the population.  In practice, the Lorenz curve actually falls somewhere between the straight 45 degree line and the backwards "L".

For a numerical measurement of the inequity in the distribution of wealth, the Gini index (or Gini coefficient) is derived from the Lorenz curve.  To calculate the Gini index, find the area between the 45 degree line of perfect equality and the Lorenz curve.  Divide this quantity by the total area under the 45 degree line of perfect equality (this number is always 0.5 --- the area of 45-45-90 triangle with sides of length 1).  If the Lorenz curve is the 45 degree line then the Gini index would be 0; there is no area between the Lorenz curve and the 45 degree line.  If, however, the Lorenz curve is a backwards "L", then the Gini-Index would be 1 --- the area between the Lorenz curve and the 45 degree line is 0.5; this quantity divided by 0.5 is 1.  Hence, equality in the distribution of wealth is measured on a scale of 0 to 1 --- more inequity as one travels up the scale.  Another way to understand (and equivalently compute) the Gini index, without reference to the Lorenz curve, is to think of it as the mean difference in wealth between all possible pairs of people in the population, expressed as a proportion of the average wealth (see Deltas, 2003 for more).

## HOW TO USE IT

The PERCENT-BEST-LAND slider determines the initial density of patches that are seeded with the maximum amount of grain.  This maximum is adjustable via the MAX-GRAIN variable in the SETUP procedure in the procedures window.  The GRAIN-GROWTH-INTERVAL slider determines how often grain grows.  The NUM-GRAIN-GROWN slider sets how much grain is grown each time GRAIN-GROWTH-INTERVAL allows grain to be grown.

The NUM-PEOPLE slider determines the initial number of people.  LIFE-EXPECTANCY-MIN is the shortest number of ticks that a person can possibly live.  LIFE-EXPECTANCY-MAX is the longest number of ticks that a person can possibly live.  The METABOLISM-MAX slider sets the highest possible amount of grain that a person could eat per clock tick.  The MAX-VISION slider is the furthest possible distance that any person could see.

GO starts the simulation.  The TIME ELAPSED monitor shows the total number of clock ticks since the last setup.  The CLASS PLOT shows a line plot of the number of people in each class over time.  The CLASS HISTOGRAM shows the same information in the form of a histogram.  The LORENZ CURVE plot shows the Lorenz curve of the population at a particular time as well as the 45 degree line of equality.  The GINI-INDEX V. TIME plot shows the Gini index at the time that the Lorenz curve is drawn.  The LORENZ CURVE and the GINI-INDEX V. TIME plots are updated every 5 passes through the GO procedure.

## THINGS TO NOTICE

Notice the distribution of wealth.  Are the classes equal?

This model usually demonstrates Pareto's Law, in which most of the people are poor, fewer are middle class, and very few are rich.  Why does this happen?

Do poor families seem to stay poor?  What about the rich and the middle class people?

Watch the CLASS PLOT to see how long it takes for the classes to reach stable values.

As time passes, does the distribution get more equalized or more skewed?  (Hint: observe the Gini index plot.)

Try to find resources from the U.S. Government Census Bureau for the U.S.'s Gini coefficient.  Are the Gini coefficients that you calculate from the model comparable to those of the Census Bureau?  Why or why not?

Is there a trend in the plotting of the Gini index with respect to time?  Does the plot oscillate?  Or does it stabilize to a certain number?

## THINGS TO TRY

Are there any settings that do not result in a demonstration of Pareto's Law?

Play with the NUM-GRAIN-GROWN slider, and see how this affects the distribution of wealth.

How much does the LIFE-EXPECTANCY-MAX matter?

Change the value of the MAX-GRAIN variable (in the `setup` procedure in the Code tab).  Do outcomes differ?

Experiment with the PERCENT-BEST-LAND and NUM-PEOPLE sliders.  How do these affect the outcome of the distribution of wealth?

Try having all the people start in one location. See what happens.

Try setting everyone's initial wealth as being equal.  Does the initial endowment of an individual still arrive at an unequal distribution in wealth?  Is it less so when setting random initial wealth for each individual?

Try setting all the individual's wealth and vision to being equal.  Do you still arrive at an unequal distribution of wealth?  Is it more equal in the measure of the Gini index than with random endowments of vision?

## EXTENDING THE MODEL

Have each newborn inherit a percentage of the wealth of its parent.

Add a switch or slider which has the patches grow back all or a percentage of their grain capacity, rather than just one unit of grain.

Allow the grain to give an advantage or disadvantage to its carrier, such as every time some grain is eaten or harvested, pollution is created.

Would this model be the same if the wealth were randomly distributed (as opposed to a gradient)?  Try different landscapes, making SETUP buttons for each new landscape.

Try allowing metabolism or vision or another characteristic to be inherited.  Will we see any sort of evolution?  Will the "fittest" survive?

Try adding in seasons into the model.  That is to say have the grain grow better in a section of the landscape during certain times and worse at others.

How could you change the model to achieve wealth equality?

The way the procedures are set up now, one person will sometimes follow another.  You can see this by setting the number of people relatively low, such as 50 or 100, and having a long life expectancy.  Why does this phenomenon happen?  Try adding code to prevent this from occurring.  (Hint: When and how do people check to see which direction they should move in?)

## NETLOGO FEATURES

Examine how the landscape of color is created --- note the use of the `scale-color` reporter.  Each patch is given a value, and `scale-color` reports a color for each patch that is scaled according to its value.

Note the use of lists in drawing the Lorenz Curve and computing the Gini index.

## CREDITS AND REFERENCES

This model is based on a model described in Epstein, J. & Axtell R. (1996). Growing Artificial Societies: Social Science from the Bottom Up. Washington, DC: Brookings Institution Press.

For an explanation of Pareto's Law, see http://www.xrefer.com/entry/445978.

For more on the calculation of the Gini index see:

 * Deltas, George (2003).  The Small-Sample Bias of the Gini Coefficient:  Results and Implications for Empirical Research.  The Review of Economics and Statistics, February 2003, 85(1): 226-234.

In particular, note that if one is calculating the Gini index of a sample for the purpose of estimating the value for a larger population, a small correction factor to the method used here may be needed for small samples.


## HOW TO CITE

If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:  
- Wilensky, U. (1998).  NetLogo Wealth Distribution model.  http://ccl.northwestern.edu/netlogo/models/WealthDistribution.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  
- Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  

## COPYRIGHT AND LICENSE

Copyright 1998 Uri Wilensky.

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2001.
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
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

@#$#@#$#@
0
@#$#@#$#@
