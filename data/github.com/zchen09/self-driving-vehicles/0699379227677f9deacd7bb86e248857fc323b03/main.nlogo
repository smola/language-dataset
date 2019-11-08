
globals
[
                           ;; in future models we plan to have separate acceleration for AV and HV
  acceleration             ;; the constant that controls how much a car speeds up or slows down by if
  phase                    ;; keeps track of the phase

  num-parking-spots        ;; tracks total number of parking spots

  ;; lists that hold parking spots for each location
  list-parking-house
  list-parking-work
  list-parking-school
  list-parking-shop
  list-parking-spot

  ;; number of people using AVs at each location
  AV-people-count-house
  AV-people-count-work
  AV-people-count-school
  AV-people-count-shop

  ;; variables for agents
  total-num-cars
  num-HVs
  num-AVs
  num-HV-people
  num-AV-people

  ;; amount of HVs that one AV can replace
  replacement-rate

  ;; amount of goals successfully met
  num-goals-met

  ;; patch agentsets
  intersections ;; agentset containing the patches that are intersections
  roads         ;; agentset containing the patches that are roads
  work          ;; the place/parkings to go to or leave from
  house         ;; the place/parkings to go to or leave from
  school        ;; the place/parkings to go to or leave from
  shop          ;; the place/parkings to go to or leave from
  goal-candidates ;; agentset containing the patches of 4 locations drop off and pickup spot
]

;AVs and HVs are both breeds of turtle
breed [ AVs AV ]  ;automous vehicle
breed [ HVs HV ]  ;human-operated vehicle

turtles-own
[
  speed     ;; the speed of the turtle
  up-car?   ;; true if the turtle moves downwards, false if it turns right/left
  wait-time ;; the amount of time since the last time a turtle has moved
  goal      ;; where am I currently headed
  count-down ;; the time it spends in the parking spot
  tick-here
  from       ;; inital spot (for AVs: initial pick-up location; for HVs: the location they leave parking spot from)
  goto       ;; destination (goals)
  on?        ;; if the car is running
  passenger? ;; true if there is passenger in the AV
]

patches-own
[
  intersection?   ;; true if the patch is at the intersection of two roads
  green-light-up? ;; true if the green light is above the intersection,, otherwise, false.
  my-phase        ;; the phase for the intersection. -1 for non-intersection patches.
  auto?           ;; whether or not this intersection will switch automatically.

  ;; for parking lot
  house?          ;; true is this is a parking spot for house
  work?           ;; true is this is a parking spot for work
  school?         ;; true is this is a parking spot for school
  shop?           ;; true is this is a parking spot for shop
  occupied?       ;; whether the parking spot is occupied
]

;;;;;;;;;;;;;;;;;;;;;;
;; Setup Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Initialize the display by giving the global and patch variables initial values.
;; Create num-cars of turtles if there are enough road patches for one turtle to
;; be created per road patch.

to setup
  clear-all
  setup-globals
  setup-patches  ;; ask the patches to draw themselves and set up a few variables

  ;; Make an agentset of all patches where there can be one of the four places
  ;; those patches are house, work, shop and school
  set goal-candidates (patch-set house work shop school)

  ;; warning if there are too many cars for the road
  if (num-AVs + num-HVs > count roads) [
    user-message (word
      "There are too many cars for the amount of "
      "road.  Either increase the amount of roads "
      "by increasing the GRID-SIZE-X or "
      "GRID-SIZE-Y sliders, or decrease the "
      "number of cars by lowering the NUM-CAR slider.\n"
      "The setup has stopped.")
    stop
  ]

  ;; create AVs
  create-AVs num-AVs
  [
    set shape  "car"
    set color blue
    set size 1.1
    set up-car? false
    setup-cars
    ;; choose at random a location for pickup
    set from one-of goal-candidates
    ;; choose at random a location for drop-offs
    set goto one-of goal-candidates with [ self != [ from ] of myself ]
    set goal goto
    set passenger? true ;in the initial set up all AVs will start off with a passenger
  ]

  ;; create HVs
  create-HVs num-HVs
  [
    set shape "car"
    set color yellow
    set size 1.1
    set up-car? false
    setup-cars
    ;; choose at random a location to depart
    set from one-of goal-candidates
    ;; choose at random a location to go
    set goto one-of goal-candidates with [ self != [ from ] of myself ]
    set goal goto
  ]

  ;; give the turtles an initial speed
  ask turtles [ set-car-speed ]
  reset-ticks
end

;; sets up the number of parking spots based on other parameters
to setup-parking-spots

  ask patches [

    ;; set the spots next to the intersections to be green to avoid double parking issues
    if (abs pxcor = 1 and abs pycor = 1)
    [set pcolor brown]

    if count patches with [pcolor = orange] < (parking-ratio * .01 * num-HVs) [
      if (any? neighbors with [pcolor = grey]) and pcolor != grey and pcolor != red and pcolor != green and pcolor != brown
      [set pcolor orange
       set occupied? false]
    ]
    ;; This sets up the parking spots for each location
    if (abs pxcor = 1 and pycor > 0 and pcolor = orange)
    [set shop? true]
    if (pxcor < 0 and abs pycor = 1 and pcolor = orange)
    [set house? true]
    if (pxcor > 0 and abs pycor = 1 and pcolor = orange)
    [set work? true]
    if (abs pxcor = 1 and pycor < 0 and pcolor = orange)
    [set school? true]
  ]

  set num-parking-spots count patches with [pcolor = orange]

  ;; creates lists for parking spots of each location
  set list-parking-house patches with [house? = true]
  set list-parking-shop patches with [shop? = true]
  set list-parking-school patches with [school? = true]
  set list-parking-work patches with [work? = true]
  set list-parking-spot patches with [pcolor = orange]
end

;; set up our destinations (draw + set goal patches)
to setup-places
  ask patches [
  if (pxcor = 10 and pycor = 3)
   [set pcolor black
    set plabel "work"]
  if (pxcor = -10 and pycor = -3)
    [set pcolor yellow
    set plabel "house"]
  if (pxcor = 3 and pycor = -10)
    [set pcolor 86
    set plabel "school"]
  if (pxcor = -3 and pycor = 10)
    [set pcolor 26
    set plabel "shop"]
  ]
  ;; we are setting our goal patches for drop-offs and pick-up (AVs) / look for parking (HVs)
  set work patches with [pxcor = 10 and pycor = 0]
  set house patches with [pxcor = -10 and pycor = 0]
  set school patches with [pxcor = 0 and pycor = -10]
  set shop patches with [pxcor = 0 and pycor = 10]
end

;; Initialize the global variables to appropriate values
to setup-globals
  ;; set current-intersection nobody ;; just for now, since there are no intersections yet
  set phase 0
  set num-goals-met 0
  set replacement-rate 10
  set total-num-cars floor(num-people / ((replacement-rate * percentage-of-AVs / 100) + 1))
  set num-AVs floor(total-num-cars * percentage-of-AVs / 100)
  set num-AV-people num-AVs * replacement-rate
  set num-HVs total-num-cars - num-AVs
  set num-HV-people num-HVs

  ;; number of AV people that exceeds number of AVs - these people will need to start out at locations
  let extra-AV-people num-AV-people - num-AVs

  ;; randomly disperses the extra AV passengers amongst our different locations
  set AV-people-count-house random extra-AV-people
  set AV-people-count-work random (extra-AV-people - AV-people-count-house)
  set AV-people-count-school random (extra-AV-people - ( AV-people-count-house + AV-people-count-work))
  set AV-people-count-shop extra-AV-people - ( AV-people-count-house + AV-people-count-work + AV-people-count-school)

  ;; don't make acceleration 0.1 since we could get a rounding error and end up on a patch boundary
  set acceleration 0.099
end

;; Make the patches have appropriate colors, set up the roads and intersections agentsets,
;; and initialize the traffic lights to one setting
to setup-patches
  ;; initialize the patch-owned variables and color the patches to a base-color
  ask patches [
    set intersection? false
    set auto? false
    set green-light-up? true
    set my-phase -1
    set pcolor green - 0.5 - random-float 0.5
  ]
  setup-places

    ;; set up new roads
  set roads patches with [
    abs pxcor = 0 or abs pycor = 0
  ]
    set intersections roads with [
    (pxcor = 0) and (pycor = 0)
  ]

  ask roads [ set pcolor grey ]
  setup-intersections
  setup-parking-spots
end

;; Give the intersections appropriate values
;; Make all the traffic lights start off so that the lights are red
;; horizontally and green vertically.
to setup-intersections
  ask intersections [
    set intersection? true
    set green-light-up? true
    set my-phase 0
    set auto? true
    set-signal-colors
  ]
end

;; Initialize the turtle variables to appropriate values and place the turtle on an empty road patch.
to setup-cars  ;; turtle procedure
  set speed 0
  set wait-time 0
  set on? true
  put-on-empty-road ;; places cars on empty spot on road
  ifelse intersection? [
    let temp random 2
    ifelse temp = 0
      [ set up-car? true ] ;; randomly sets some of cars to be going vertically and some to go horizontally
      [ set up-car? false ]

  ]
  [
    ; if the turtle is on a vertical road (rather than a horizontal one)
    ifelse pxcor = 0
      [ set up-car? true ]
      [ set up-car? false ]
  ]

  ; sets the cars direction (north or east)
  ifelse up-car?
    [ set heading 0 ]
    [ set heading 90 ]

end

;; Find a road patch without any turtles on it and place the turtle there.
to put-on-empty-road  ;; turtle procedure
  move-to one-of roads with [ not any? turtles-on self ]
end


;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Run the simulation
to go

  ;; sets up traffic lights
  set-signals

  ;; specifc tasks for HVs
  ask-concurrent HVs [

  ifelse reachDestination = false
  ;if this is not destination
  [
      set on? true
      set-car-dir
      set-safety-distance-HV
      fd speed
      record-data     ;; record data for plotting
  ]
 ;if this is destination
    [
    ; check if the car is parked in lot or not
     ;if it is parked
     ifelse on? = false
    [
      ;if the parking time is up
      ifelse count-down <= 0
      [ move-back-to-road ]
      ;if parking time not up
      [decrement-counter]

    ]
    ; the car is not parked
    [
       ;; create temporary variable for current location and look for parking spot next to current location
      let cur-spot find-spot
      let x [pxcor] of cur-spot
      let y [pycor] of cur-spot

        ;; first, a hv looks for parking spots, and report whether it found one or not
      ifelse x != 0 and y != 0
      ;the hv found a spot
      [
        move-to-spot x y ; hv will move to open parking spot
        set-counter ;; sets parking count down
      ]
      ;the car does not have a parking spot
      [
        go-around ;; keep driving around until spot opens up
      ]
    ]
    ]
  ]

  ;; specific tasks for AVs
  ask AVs [
    ; if reached an AV destination
    ifelse destination-AVs?
    [
      if passenger?
      ;if there is passenger, drop them off
      [
        drop-off-passengers
      ]
      ;pick up passengers
      pick-up-passengers

      ;set new destination for AV
      set goto one-of goal-candidates with [ self != [ goto ] of myself ]
    ]
    ;if this is not destination, keep driving
    [
    set on? true
    set-car-dir
    set-safety-distance-AV
    fd speed
    record-data      ;; record data for plotting
    ]
  ]
  next-phase ;; update the phase and the global clock
  tick
end

;; have the traffic lights change color if phase equals each intersections' my-phase
to set-signals
  ask intersections with [ auto? and phase = floor ((my-phase * ticks-per-cycle) / 100) ] [
    set green-light-up? (not green-light-up?)
    set-signal-colors
  ]
end

;; This procedure checks the variable green-light-up? at each intersection and sets the
;; traffic lights to have the green light up or the green light to the left.
to set-signal-colors  ;; intersection (patch) procedure
    ifelse green-light-up? [
      ask patch-at -1 0 [ set pcolor red ]
      ask patch-at 0 -1 [ set pcolor green ]
    ]
    [
      ask patch-at -1 0 [ set pcolor green ]
      ask patch-at 0 -1 [ set pcolor red ]
    ]
end

to set-speed [ delta-x delta-y ] ;; turtle procedure
  ;; get the turtles on the patch in front of the turtle
  let turtles-ahead turtles-at delta-x delta-y

  ;; if there are turtles in front of the turtle, slow down
  ;; otherwise, speed up
  ifelse any? turtles-ahead [
    ifelse any? (turtles-ahead with [ up-car? != [ up-car? ] of myself ]) [
      set speed 0
    ]
    [
      set speed [speed] of one-of turtles-ahead
      slow-down
    ]
  ]
  [ speed-up ]
end

;; set the turtles' speed based on whether they are at a red traffic light or the speed of the
;; turtle (if any) on the patch in front of them
to set-car-speed  ;; turtle procedure
  ; if at red light, stop
  ifelse pcolor = red [
    set speed 0
  ]
  [
    ; if not at red light, go
   ifelse up-car?
    [set-speed 0 -1 ]
    [set-speed 1 0 ]
  ]
end

;; sets direction of car
to set-car-dir

  ; if at an intertersection
  ifelse intersection? [
    ; if driving north and at intersection, will randomly decide if wants to turn right
    ifelse up-car?
    [
      let temp random 2
      ifelse temp = 0
      [ set up-car? true ]
      [ set up-car? false ] ; turn right
    ]
    [; if driving east and at intersection, will randomly decide if wants to turn left
      let temp random 2
      ifelse temp = 0
      [ set up-car? true ] ; turn left
      [ set up-car? false ]
    ]
  ]

  [ ; if the turtle is on a vertical road (rather than a horizontal one)
    ifelse pxcor = 0 and pycor != 0
      [ set up-car? true ]
      [ set up-car? false ]
  ]
  ifelse up-car?
    [ set heading 0 ]
    [ set heading 90 ]
end

;; sets a distance that HV needs to keep from the cars in front - plan to modify this as build model
to set-safety-distance-HV
  let distance-car 1

  let one-patch (not any? turtles-on patch-ahead 1) ;;and (not any? turtles-on patch-ahead 2)
  ifelse one-patch
  [set distance-car distance-car + 1]
  [set distance-car distance-car + 0]

  let two-patches (one-patch and not any? turtles-on patch-ahead 2)
  ifelse two-patches
  [set distance-car distance-car + 1]
  [set distance-car distance-car + 0]

  let three-patches (two-patches and not any? turtles-on patch-ahead 3)
  ifelse three-patches
  [set distance-car distance-car + 1]
  [set distance-car distance-car + 0]

  ;; for HV make sure the distance in front is three
  ifelse distance-car > 3 and pcolor != red
  [speed-up]
  [ifelse distance-car < 3
    [slow-down]
    []
  ]
  ifelse distance-car = 1
  [set speed 0]
  []
end

;; checking distance of car in front for AVs - plan to modify this as build model
to set-safety-distance-AV

  let distance-car 1

  let one-patch (not any? turtles-on patch-ahead 1) ;;and (not any? turtles-on patch-ahead 2)
  ifelse one-patch
  [set distance-car distance-car + 1]
  [set distance-car distance-car + 0]

  ; AV will always drive one patch directly behind car in front (without violating speed limit)
  ifelse distance-car > 1 and pcolor != red
  [speed-up]
  [set speed 0]

end

;; decrease the speed of the car - can eventually modify this for AV and HV separately
to slow-down  ;; turtle procedure
  ifelse speed <= 0
    [ set speed 0 ]
    [ set speed speed - acceleration ]
end

;; increase the speed of the car - can eventually modify this for AV and HV separately
to speed-up  ;; turtle procedure
  ifelse speed > speed-limit
    [ set speed speed-limit ]
    [ set speed speed + acceleration ]
end

;; keep track of the amount of time a car has been stopped
;; if its speed is 0 and it is not parked
to record-data  ;; turtle procedure
  ifelse speed = 0 and on? = true [
    set wait-time wait-time + 1
  ]
  [ set wait-time 0 ]
end

;; cycles phase to the next appropriate value
to next-phase
  ;; The phase cycles from 0 to ticks-per-cycle, then starts over.
  set phase phase + 1
  if phase mod ticks-per-cycle = 0 [ set phase 0 ]
end

;; method to check if AV has reached its destination
to-report destination-AVs?
  let reach? false

  ; four cases for house, work, school, and shop

  if goto = one-of house [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = -10 and y = 0 [
      set reach? true
    ]
  ]
  if goto = one-of work [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 10 and y = 0 [
      set reach? true
    ]
  ]
  if goto = one-of school [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 0 and y = -10 [
      set reach? true
    ]
  ]
  if goto = one-of shop [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 0 and y = 10 [
      set reach? true
    ]
  ]
  report reach?
end

;; method to drop off AV passengers if reached destination
to drop-off-passengers

  if goto = one-of house [
    set AV-people-count-house AV-people-count-house + 1
    ;print "dropped off from house"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of work [
    set AV-people-count-work AV-people-count-work + 1
    ;print "dropped off from work"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of school [
    set AV-people-count-school AV-people-count-school + 1
    ;print "dropped off from school"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of shop [
    set AV-people-count-shop AV-people-count-shop + 1
    ;print "dropped off from shop"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
end

;; method to pick-up new AV passenger
to pick-up-passengers
  if goto = one-of house [
    ; checks if there is an AV passenger at current location
    if AV-people-count-house > 1 [
      set AV-people-count-house AV-people-count-house - 1
      ;print "picked up from house"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of work [
    if AV-people-count-work > 1
    [
      set AV-people-count-work AV-people-count-work - 1
      ;print "picked up from work"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of school [
    if AV-people-count-school > 1
    [
      set AV-people-count-school AV-people-count-school - 1
      ;print "picked up from school"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of shop [
    if AV-people-count-shop > 1
    [
      set AV-people-count-shop AV-people-count-shop - 1
      ;print "picked up from shop"
      set passenger? true
      ;print passenger?
    ]
  ]
end

;; method to see if HV has reached destination
to-report reachDestination
  let reach? false
  ; 4 cases for each destination
  ; case 1
  if goto = one-of house [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    ;; checks for range of road where an HV can park for their destination
    if x >= -18 and x <= -1 [
      set reach? true
    ]
  ]
  ;case 2
  if goto = one-of work [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x <= 18 and x >= 1 [
      set reach? true
    ]
  ]
  ;case 3
  if goto = one-of school [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= -18 and y <= -1 [
      set reach? true

    ]
  ]
  ;case 4
  if goto = one-of shop [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= 1 and y <= 18 [
      set reach? true
    ]
  ]
  report reach?
end

;; if hv has found an open spot, move to that spot to park
to move-to-spot [x y]
    setxy x y
    set on? false ;; turns off car while parked

  ;; changes the patch of the parking spot to be occupied
    ask patches with [pxcor = x and pycor = y] [
      set occupied? true
    ]
  ; updates number of goals met
    set num-goals-met num-goals-met + 1

end

;; reports if a parking spot has been found
to-report find-spot
  let found false
  let spot one-of patches with [pxcor = 0 and pycor = 0]

    ; case 1: checks the two patches between the road for house parking spots
    if goto = one-of house [
      let spot1 one-of neighbors4 with [ pycor > 0 ]
      let spot2 one-of neighbors4 with [ pycor < 0 ]
    ;; checks for parking spot of first neighbor
        if member? spot1 list-parking-house [
          set spot spot1
          set found not [occupied?] of spot1 ; checks if parking spot is occupied, i fnot occupied sets found spot to true
        ]
        print "my patch"
        print patch-here
    ;; checks for parking spot of second neighbor
        if member? spot2 list-parking-house and not found [
          set spot spot2
          set found not [occupied?] of spot2
        ]
      ]
   ;case 2: checks for open parking spots for work parking
   if goto = one-of work [
      let spot1 one-of neighbors4 with [ pycor > 0 ]
      let spot2 one-of neighbors4 with [ pycor < 0 ]

      if member? spot1 list-parking-work [
          set spot spot1
          set found not [occupied?] of spot1
        ]
      if member? spot2 list-parking-work and not found [
          set spot spot2
          set found not [occupied?] of spot2
        ]
    ]
  ; case 3: checks for open parking spots for shop parking
   if goto = one-of shop [
      let spot1 one-of neighbors4 with [ pxcor > 0 ]
      let spot2 one-of neighbors4 with [ pxcor < 0 ]

      if member? spot1 list-parking-shop [
          set spot spot1
          set found not [occupied?] of spot1
        ]
      if member? spot2 list-parking-shop and not found [
          set spot spot2
          set found not [occupied?] of spot2
        ]
    ]
  ; case 4: checks for open parking spots for school parking
   if goto = one-of school [
      let spot1 one-of neighbors4 with [ pxcor > 0 ]
      let spot2 one-of neighbors4 with [ pxcor < 0 ]

      if member? spot1 list-parking-school [
          set spot spot1
          set found not [occupied?] of spot1
        ]
        print "my patch"
        print patch-here
      if member? spot2 list-parking-school and not found [
          set spot spot2
          set found not [occupied?] of spot2
        ]
    ]
; if found a parking spot that is not occupied, it will report that spot
; if occupied it will return x = 0 and y = 0
  ifelse found [report spot]
  [report one-of patches with [pxcor = 0 and pycor = 0]]
end

;; if the HV didn't find a spot, keep driving until find one (may modify this later to take into account our safety distances)
to go-around
  if count [turtles-here] of patch-ahead 1 = 0 [fd 1]
end

;; if parking time is up, move back to the road
to move-back-to-road
  let canMove false

  ;; checks all parking lists to find which destination it is parked at
    if member? patch-here list-parking-shop
    [
      let x 0
      let y [pycor] of patch-here
    ;; first check if there is an opening in road for the car to enter
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false] ;; parking spot is now available
        setxy x y
        set heading 0
        set canMove true
      ]
    ]

    if member? patch-here list-parking-work
    [
      let x [pxcor] of patch-here
      let y 0
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y
        print patch-here
        set heading 90
        set canMove true
      ]
    ]

    if member? patch-here list-parking-house
    [
      let x [pxcor] of patch-here
      let y 0
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y
        set heading 90
        set canMove true
      ]
    ]

    if member? patch-here list-parking-school
    [
      let x 0
      let y [pycor] of patch-here
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y
        set heading 0
        set canMove true
      ]
    ]

  ;; sets a new goal destination
  if canMove [
    set goto one-of goal-candidates with [ self != [ goto ] of myself ]
    set on? true
  ]
end

;; sets up parking count down
to set-counter
    set count-down 1000 ;; set parking time
end

to decrement-counter
    set count-down count-down - 1
end


; Copyright 2017 Zhiyang Chen, Bari Gordon, Meiqing Li.
; See Info tab for documentation.
@#$#@#$#@
GRAPHICS-WINDOW
15
15
578
579
-1
-1
15.0
1
20
1
1
1
0
1
1
1
-18
18
-18
18
1
1
1
ticks
30.0

PLOT
1040
405
1258
580
Average Wait Time of Cars
Time
Average Wait
0.0
100.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [wait-time] of turtles"

PLOT
820
405
1036
580
Average Speed of Cars
Time
Average Speed
0.0
100.0
0.0
1.0
true
false
"set-plot-y-range 0 speed-limit" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [speed] of turtles"

SLIDER
700
15
880
48
num-people
num-people
1
400
88.0
1
1
NIL
HORIZONTAL

PLOT
600
405
814
580
Stopped Cars
Time
Stopped Cars
0.0
100.0
0.0
100.0
true
false
"set-plot-y-range 0 total-num-cars" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [speed = 0]"

BUTTON
600
75
685
120
Go
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
600
15
684
60
Setup
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

SLIDER
890
50
1035
83
speed-limit
speed-limit
0.1
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
890
15
1035
48
ticks-per-cycle
ticks-per-cycle
1
100
95.0
1
1
NIL
HORIZONTAL

SLIDER
700
50
880
83
percentage-of-AVs
percentage-of-AVs
0
100
28.0
1
1
%
HORIZONTAL

MONITOR
1040
125
1260
170
Total Number of Cars (AV and HV)
total-num-cars
0
1
11

SLIDER
700
85
880
118
parking-ratio
parking-ratio
0
100
33.0
1
1
%
HORIZONTAL

MONITOR
1040
350
1260
395
Number of Parking Spots at All Locations
num-parking-spots
0
1
11

MONITOR
1150
235
1260
280
HV user
num-HV-people
0
1
11

MONITOR
1040
180
1140
225
Number of AVs
num-AVs
0
1
11

MONITOR
1040
235
1140
280
AV user
num-AV-people
0
1
11

MONITOR
1150
180
1260
225
Number of HVs
num-HVs
0
1
11

MONITOR
1040
295
1260
340
Occupied AVs
count AVs with [passenger? = true]
0
1
11

SLIDER
890
85
1035
118
current-phase
current-phase
0
99
1.0
1
1
%
HORIZONTAL

PLOT
600
125
1035
395
Number of Goals Met
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
"default" 1.0 0 -16777216 true "" "plot num-goals-met"

@#$#@#$#@
## ACKNOWLEDGMENT

This model is partially based on the example from Chapter Five of the book "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## WHAT IS IT?

The Autonomous Vehicle, Traffic Congestion, and Parking model simulates traffic moving in a simplified city grid with two perpendicular one-way streets that forms an intersection. It allows you to control global variables, such as the AV/HV ratio and the number of people, and explore traffic dynamics, particularly traffic congestion measured by average speed and average waiting time for parking, which add to the total utility.

This model gives the cars destinations (house, shop, school, work). The agents in this model use goal-based and adaptive cognition.

## HOW IT WORKS

Each time step, the cars (both AVs and HVs) face the next destination they are trying to get to (work , house, shop or school) and attempt to move forward at their current speed and keep a safety speed dependent on whether the car directly in front is an AV or HV. If their current distance to the car directly in front of them is less than their safety distance (AV or HV), they decelerate. If the distance is larger than safety distance, they accelerate. If there is a red light or a stopped car in front of them, they stop. We also make sure that no two cars are on the same patch. 

In each cycle, each car will be assigned one destination. Traffic lights at the intersection will automatically change at the beginning of each cycle.

## HOW TO USE IT

Change any setting that you would like to change. Press the SETUP button.

Start the simulation by pressing the GO button. 

### Buttons

SETUP --  sets up patches and parking spots for each destination. Parking spots are randomly allocated based on parking ratio. This button also places AVs and HVs on the street.
GO -- runs the simulation indefinitely. Cars travel from their homes to their work and back.

### Sliders

SPEED-LIMIT -- sets the maximum speed for the cars.

NUM-PEOPLE -- sets the number of cars in the simulation.

PERCENTAGE-OF-AVS -- percentage of AVs in total number of cars

PARKING-RATIO -- percentage of HVs that have a parking spot

TICKS-PER-CYCLE -- sets the number of ticks that will elapse for each cycle. This has no effect on manual lights. This allows you to increase or decrease the granularity with which lights can automatically change.

CURRENT-PHASE -- controls when the current light changes, if it is in auto mode. The slider value represents the percentage of the way through each cycle at which the light should change. So, if the TICKS-PER-CYCLE is 20 and CURRENT-PHASE is 75%, the current light will switch at tick 15 of each cycle.

### Monitors

A number of monitors display a number of variables including total number of cars, AVs, HVs, number of AV users, HV users as well as number of occupied AVs (AV passengers in cars) and total number of parking spots at all four locations.

### Plots

NUMBER OF GOALS MET -- displays the cumulative number of goals met by AVs and HVs. For AVs the goal is to drop off passengers, while HVs aim to find a spot and park once they reach destination.

STOPPED CARS -- displays the number of stopped cars over time.

AVERAGE SPEED OF CARS -- displays the average speed of cars over time (excluding parked cars).

AVERAGE WAIT TIME OF CARS -- displays the average time cars are stopped over time (excluding parked cars).
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
true
0
Polygon -7500403 true true 180 15 164 21 144 39 135 60 132 74 106 87 84 97 63 115 50 141 50 165 60 225 150 285 165 285 225 285 225 15 180 15
Circle -16777216 true false 180 30 90
Circle -16777216 true false 180 180 90
Polygon -16777216 true false 80 138 78 168 135 166 135 91 105 106 96 111 89 120
Circle -7500403 true true 195 195 58
Circle -7500403 true true 195 47 58

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
NetLogo 6.0.2
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
1
@#$#@#$#@

