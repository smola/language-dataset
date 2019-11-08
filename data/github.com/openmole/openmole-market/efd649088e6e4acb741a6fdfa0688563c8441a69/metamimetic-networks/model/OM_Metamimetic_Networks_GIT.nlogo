extensions [ nw ]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Declare Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 globals [ 
;; Globals that come from the widget 
Topology                    ;type of graph
Num-Agents                  ;number of turtles
Connection-Probability      ;for random network
Initial-Neighbours          ;for small world network
Rewiring-Probability        
Scale-Free-Exponent         ;for scale-free network
Initial-Random-Types?       ;Agents' types initialization
Initial-Maxi-% 
Initial-Mini-% 
Initial-Conf-% 
Initial-Anti-% 
Strength-of-Dilemma         ;prisoner's dilemma
inicoop
replacement?
cultural-constant
load-topology?


FileIn
FileOut

;stopping condition
lAvg
condition
cooperation-list
avg1 
avg2  
var1 
var2 

;NETWORK CONSTRUCTION
;for scale-free
graphical?
sequence
p_k
Z
uniform
;for connectivity
success?

;;appearance 
sizeT
radius

;;OUTPUTS
  cooperation-rate
  satisfaction-rate
  satisfaction-rate2

life-expectation
mortality-rate
prob-to-die 
prob-die-imitation
infinity


;;IN THE NETWORK CONTEXT
;  clustering-coefficient               ;; average of clustering coefficients of all turtles
;  average-path-length                  ;; average path length of the network
;  network-density
;  diameter
;  n-links
;  clustering-coefficient-2


;TURTLE POPULATION
  maxi
  mini
  conf
  anti

  turtles-maxi
  turtles-mini
  turtles-conf
  turtles-anti
;perception 
]


turtles-own [
  cooperate       ;; patch will cooperate
  my.rule             ;; patch will have one of four rules: 1=Maxi 2=mini 3=conformist 4=anticonformist  
  score            ;; score resulting from interaction of neighboring patches. It is dictated by the PD payoffs and the discount factor
  last.score
  inst.score
;  satisfaction
  satisfaction2
  age
  chances.imitations  

  rule?
  behavior?

;for network computations
;%%%%%
;distance-from-other-turtles
degree
free-stubs
;node-clustering-coefficient
;betweenness-centrality
;eigenvector-centrality
;page-rank
;closeness-centrality
;longest-path
;mean-path

;time since last changes 

rule.history
behavior.history
ticks.at.death.list
satisfaction.history
scores.history
best.history

;time.rule
;time.behavior
;n.changes
;n.changes.behavior
;n.changes.list 
;time.rule.list 
;time.behavior.list
;rule.at.death.list
;age.at.death.list
;rule.history
;time.history
;ticks.history
;counter.age 
;counter.ticks
;age.history 

;neighbors' type
n.maxi
n.mini
n.anti
n.conf

;neighbors' type
n.maxi.list
n.mini.list
n.anti.list
n.conf.list

;for outputs
;  theta_1
;  theta_2
;  weighting-history
;  copy-error-rule
;  copy-error-behavior
 
run.info 
]

links-own[
; for small-world 
 rewired?
  ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
clear-all

;always need to be setup
set Num-Agents *-Num-Agents
set Topology *-Topology
set Strength-of-Dilemma *-strength-of-dilemma
set inicoop *-inicoop
set replacement? *-replacement?
set cultural-constant *-cultural-constant
set load-topology? Load-Topology
set FileIn *-fileIn
let seed random 2000000000
random-seed seed



;only setup if RN
if Topology = "Random" [set Connection-Probability *-Connection-Probability]

;only setup if SW
if Topology = "Small-World" [
                               set Initial-Neighbours  *-Initial-Neighbours 
                               set Rewiring-Probability *-Rewiring-Probability
                               ]

;if Topology = "Scale-Free" [set Scale-Free-Exponent *-Scale-Free-Exponent]
     
 
set Initial-Random-Types? *-Initial-Random-Types?
ifelse not Initial-Random-Types?
      [
      set Initial-Maxi-% *-Initial-Maxi-%
      set Initial-Mini-% *-Initial-Mini-%
      set Initial-Conf-% *-Initial-Conf-%
      set Initial-Anti-% (100 - Initial-Conf-% - Initial-Mini-% - Initial-Maxi-%)
      ]
      [
      set Initial-Maxi-% (random-float 1) * 100
      set Initial-Mini-% (random-float (1 - Initial-Maxi-%)) * 100 
      set Initial-Conf-% (random-float (1 - Initial-Maxi-% - Initial-Mini-%)) * 100
      set Initial-Anti-% (100 - Initial-Conf-% - Initial-Mini-% - Initial-Maxi-%)
      ]

;set Transcription-error 1
;set PER *-p-Error-Copy-Rule
;set PEB *-p-Error-Copy-Behavior

set radius ( ( min (list world-width world-height) ) / 2 - 1)  
set FileOut (word Rewiring-Probability  "_" inicoop "_" seed "_" Strength-of-Dilemma "_" cultural-constant "_" Initial-Neighbours "_" replacement? )
common-setup
end

to common-setup
set Initial-Anti-% (100 - Initial-Conf-% - Initial-Mini-% - Initial-Maxi-%)
set infinity Num-Agents * 100
set success? false
set condition false
set cooperation-list []
set avg1 0
set avg2 0 
set var1 0
set var2 0
set lAvg 50

ifelse not load-topology? [setup-Topology] 
;[nw:load-matrix FileName turtles links
;; ask links [set color gray]
;]
[
 nw:load-graphml FileIn 
 nw:set-context turtles links
]

set Num-Agents count turtles

setup-init-turtles

if Topology != "Lattice" [ask turtles [set size 3]]
set sizeT [size] of one-of turtles 

set-life-distribution-USA2010
;if replacement? [
;                 init-age-USA2010
;                ]

ask turtles [set degree count link-neighbors]
reset-ticks

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open Mole Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to run-to-grid [tmax]
common-setup
while [not condition and ticks < tmax ][go]
;set-last-measures
reset-ticks
end 

to run-Logo [tmax]  
while [not condition and ticks < tmax] [go]
;set-last-measures
end


to-report go-stop?
let res true
let a1 max (list avg1 avg2)
let a2 min (list avg1 avg2)
let v1 max (list var1 var2)
let v2 min (list var1 var2)

let c1 (a1 - a2)   
let c2 (v1 - v2) 

ifelse  c1 < .03 and c2 < .005 [set res true][set res false]
if  c2 = 0 and c2 = 0 [set res true ] 
if  majority != 3 and replacement? [set res false] 

report res
end

to-report majority
let lista (list maxi mini conf anti)
let lista2 sort lista
report (( position (item  3 lista2 ) lista ) + 1)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Measures & EXPORT;;;;;;;;;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to calculate-Neighbors
ask turtles [
set n.maxi count link-neighbors with  [my.rule = 1]  / degree
set n.mini count link-neighbors with  [my.rule = 2]  / degree
set n.conf count link-neighbors with  [my.rule = 3]  / degree
set n.anti count link-neighbors with  [my.rule = 4]  / degree
set n.maxi.list lput n.maxi n.maxi.list
set n.mini.list lput n.mini n.mini.list
set n.anti.list lput n.anti n.anti.list
set n.conf.list lput n.conf n.conf.list
]
end

to set-outputs
  ;populations
set turtles-maxi turtles with [ my.rule  = 1]
set turtles-mini turtles with [ my.rule  = 2]
set turtles-conf turtles with [ my.rule  = 3]
set turtles-anti turtles with [ my.rule  = 4]


set maxi count turtles-maxi  
set mini count turtles-mini  
set conf count turtles-conf  
set anti count turtles-anti   

set cooperation-rate count turtles with [cooperate] / Num-Agents
;set satisfaction-rate count turtles with [shape = "face happy"] / Num-Agents
set satisfaction-rate2  mean [satisfaction2] of turtles

;;;##FILL STOP CONDITION MEASURES
ifelse length cooperation-list < lAvg
[set cooperation-list fput cooperation-rate cooperation-list]
[
 set cooperation-list fput cooperation-rate cooperation-list
 set cooperation-list remove-item (lAvg - 1) cooperation-list
if ticks = 150 [set avg1 mean cooperation-list
                set var1 sqrt variance cooperation-list
               ]

if ticks = 200 [
               set avg2 mean cooperation-list
               set var2 sqrt variance cooperation-list
               set condition go-stop?
               ]
if ticks mod 50 = 0 and ticks > 200 [
               set avg1 avg2
               set var1 var2
               set avg2 mean cooperation-list
               set var2 sqrt variance cooperation-list
               set condition go-stop?
               ]
]  
end

to export-graphL
let name (word FileOut "_graph.graphml")
nw:save-graphml name
end 
to export-coopL
let name (word FileOut "_coop.csv")
export-plot "Cooperation and Satisfaction" name 
end
to export-propL
let name (word FileOut "_popul.csv")
export-plot "Population" name
end
to export-agesL
let name (word FileOut "_ages.csv")
export-plot "Age Plot" name 
end

to export-graph
nw:save-graphml "graph.graphml"
end 

to export-coop
export-plot "Cooperation and Satisfaction" "coop.csv" 
end

to export-prop
export-plot "Population" "popul.csv"
end

to export-ages
export-plot "Age Plot" "ages.csv" 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Turtles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-init-turtles

if-else Initial-Random-Types? [ask turtles [set my.rule (random 4) + 1]]      
  [
   ask n-of (floor (Initial-Maxi-% * Num-Agents  / 100 )) turtles [set my.rule 1]
   ask n-of floor ((Initial-Mini-% * Num-Agents / 100 )) turtles with [my.rule != 1] [set my.rule 2]
   ask n-of floor ((Initial-Conf-% * Num-Agents / 100 )) turtles with [my.rule != 1 and my.rule != 2] [set my.rule 3]
   ask turtles with [my.rule != 1 and my.rule != 2 and my.rule != 3] [set my.rule 4]
  ]
 
ask turtles [      
     set shape "face sad"
     set size sizeT
     set age 0
     set satisfaction2 1
     ifelse random-float 1.0 < (inicoop / 100)
        [set cooperate true]
        [set cooperate false]
     establish-color
     set score 0.0
     set rule? false
     set behavior? false


set chances.imitations 0
;set perception 0 

set run.info (word Topology  "*" Strength-of-Dilemma "*" inicoop "*" replacement? "*" cultural-constant "*" Rewiring-Probability "*" Initial-Neighbours "*" Connection-Probability "*" Scale-Free-Exponent "*" Initial-Random-Types? "*" load-topology? "*" Num-Agents )

set rule.history []
set behavior.history []
set ticks.at.death.list []
set satisfaction.history []
set scores.history []
set best.history []

set n.maxi.list []
set n.mini.list []
set n.anti.list []
set n.conf.list []


]
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To Go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
;;;;;;;;;;;;;;;;;;;;;
;uncomment to change dynamically on widget
;set Strength-of-Dilemma *-strength-of-dilemma
;set inicoop *-inicoop
;set replacement? *-replacement?
;set cultural-constant *-cultural-constant
;if replacement? [set-life-distribution-USA2010]
;;;;;;;;;;;;;;;;;;;;;

ask turtles [interact]
decision-stage
learn-stage

ask turtles [fill.state.info]

;;;;;;;;;;;;;;;;;;;;;;;
;uncomment to view changes widget
;ask turtles [establish-color]
;ask turtles [set-faces]
;;;;;;;;;;;;;;;;;;;;;;;

ask turtles [set satisfaction2 satisfaction-2]

set-outputs            
calculate-Neighbors
my-update-plots
reset-change

ask turtles [set chances.imitations chances.imitations + 1
             
             ifelse replacement? 
             [
             if  chances.imitations >= cultural-constant   [set age age + 1
                                                      set chances.imitations 0
                                                     ]
             ][set age age + 1]
            ]
if replacement? [replacement]
tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Go Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to fill.state.info
set rule.history lput my.rule rule.history
set behavior.history lput cooperate behavior.history 
set satisfaction.history lput satisfaction2 satisfaction.history 
set scores.history lput score scores.history
set best.history lput am-i-the-best? best.history 
end

to reset-change
ask turtles   [
              set rule? false
              set behavior? false
              ]
end

to set-faces
ifelse am-i-the-best?   [set shape "face happy"]
                        [set shape "face sad"]                          
end

to interact  ;; calculates the agents' payoffs for Prisioner's Dilema.
let total-cooperators count link-neighbors with [cooperate]
set inst.score 0
ifelse cooperate
    [set inst.score (total-cooperators * (1 - Strength-of-Dilemma) / degree)]
    [set inst.score ((total-cooperators + (degree - total-cooperators) * Strength-of-Dilemma ) / degree)]  
set last.score score
set score inst.score
end


to-report am-i-the-best? ;; reports true if the agent is the best in its neighborhood
 let test false  
 if ( my.rule  = 1) and  (score >= [score] of max-one-of link-neighbors [score]  * 0.99)  [set test true]
 if ( my.rule  = 2) and  (score <= [score] of min-one-of link-neighbors [score] * 1.01)     [set test true]
 if ( my.rule  = 3) and  (member? my.rule majority-rules)                                      [set test true]
 if ( my.rule  = 4) and  (member? my.rule minority-rules) and not all? (link-neighbors) [ my.rule  = 4]  [set test true]    
 report test
end

to decision-stage
ask turtles [
  
  let satisfaction  am-i-the-best? ;  
;  if error_on_satisfaction
;  [
;  if random-float 1 <= copy-error-rule ;
;     [set satisfaction not am-i-the-best?]
;  ]

  
if not satisfaction
[
;if random-float 1 <= theta ;only some agents will be allowed to change rule
;      [
      set rule? true 
      set behavior? true
;      ]
       
; if random-float 1 <= theta  
;      [set behavior? true]
]

if age < 15 
    [set rule? false]

]
end

to learn-stage
ask turtles 
[
if rule? [select-rule]
if behavior? [select-behavior]
]
end


to-report is-my-rule-the-best? ;; reports true if the agent's my.rule is used by any of the best valuated agents in its neighborhood (according with its rule) and false otherwise
  let test false
  ifelse am-i-the-best? [set test true][
  if member? my.rule [my.rule] of best-elements [set test true] 
  ]
  report test
end

; choose strategy if your rule is not the best, if innovate? choose rule if you are unsatisfied
to select-rule              
  ;ifelse not is-my-rule-the-best?   
   if not is-my-rule-the-best?   
   [copy-strategy (one-of best-elements)]         
   
   ;[if not am-i-the-best? and member? rule [rule] of best-elements and innovate? ;stuck agent will innovate with probability error-copy
   ;    [set rule (random 4 + 1)]
   ;]
end



to copy-strategy [temp-agent]
;;;RULE STEP
;ifelse random-float 1.0 > copy-error-rule ; some agents do the right thing
;       [

       set my.rule [my.rule] of temp-agent

;      set theta_1 [theta_1] of temp-agent       
;         set theta_2 [theta_2] of temp-agent 
;         if Copy-Thetas? [
;         set theta_1 add-noise "theta_1" Transcription-Error
;         set theta_2 add-noise "theta_2" Transcription-Error
;                         ]
;       ]     
;       [set my.rule random 4 + 1 ] ;do a random thing
;       
       set rule? false
end



to select-behavior 
;ifelse random-float 1 > copy-error-behavior ;only some agents do the right thing 
;       [ 
       if ( my.rule  = 1) or ( my.rule  = 2) [set cooperate [cooperate] of one-of best-elements ]
       if  my.rule = 3                 [set cooperate majority-behavior]         
       if my.rule = 4                 [set cooperate not majority-behavior]
;       ]
;      [
;      ifelse random-float 1.0 < .5  [set cooperate true] [set cooperate false] ;choose random behaviour
;      ]
set behavior? false
end 


to-report majority-behavior
  let mylist [cooperate] of (turtle-set link-neighbors self)
  report one-of modes mylist
end

to-report satisfaction-2
let sat2 0
let myset (turtle-set link-neighbors self)
 
if my.rule = 1 [
            ifelse abs(max [score] of myset - min [score] of myset) = 0
            [set sat2 1]
            [set sat2  ( (score  - min [score] of myset ) / (max [score] of myset - min [score] of myset))] 
            ]                          

if my.rule = 2 [
            ifelse   (max [score] of myset - min [score] of myset) = 0
            [set sat2 1] 
            [set sat2  ( ( max [score] of myset - score  ) / ( max [score] of myset - min [score] of myset ))] 
            ]              

if my.rule = 3 [
            let my-frequency ( count link-neighbors with [ my.rule  = 3] + 1 ) / (degree + 1)
            ifelse abs(min-frequency - max-frequency) = 0 
                                                         [set sat2 1]
                                                         [set sat2  (my-frequency - min-frequency) / (max-frequency - min-frequency)]
            ]

if my.rule = 4 [
            let my-frequency (count link-neighbors with [ my.rule  = 4]  + 1) / ( degree + 1)
            ifelse abs( max-frequency - min-frequency ) = 0 
                                                       [set sat2 1]
                                                       [set sat2  ( max-frequency - my-frequency ) / (  max-frequency - min-frequency)]
            ]

report sat2
end


to-report majority-rules  ;; reports a set with the number of the most frequent rules in agent's neighborhood (agent included)
                          ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist [ my.rule ] of (turtle-set link-neighbors self)
  set mylist modes mylist
  report mylist
end

to-report minority-rules ;; reports a set with the number of the less frequent rules in agent's neighborhood (agent included)
                         ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist_1 [ my.rule ] of (turtle-set link-neighbors self)
  let mylist []
  let j 1
  while [empty? mylist] [
  let i 1
  repeat 4 [
    if length filter [? = i] mylist_1 = j  [set mylist lput i mylist] 
    set i i + 1
    ]
  set j j + 1
  ] 
  report mylist
end


to-report best-elements ;; report a list with the agents with the best performance according to agents  
  let myset (turtle-set link-neighbors self)
  if my.rule = 1 [set myset myset with [score >= [score] of max-one-of myset [score] * 0.99]]
  
  if my.rule = 2 [set myset myset with [score <= [score] of min-one-of myset [score] * 1.01]]
  
  if my.rule = 3 [
              let rules-list majority-rules
              set myset myset with [member? my.rule rules-list]
              ] 
  if my.rule = 4 [
              let rules-list minority-rules
              if not empty? rules-list [
                                        set myset myset with [member? my.rule rules-list]
                                       ]  
              ]
  
  report myset
end  

to-report add-noise [value noise-std]
      let epsilon random-normal 0.0 noise-std
      if ( epsilon <= -100 )
      [ set epsilon -99] 
      let noisy-value runresult value * 100 / ( 100 + epsilon )
      if (noisy-value > 1) [set noisy-value 1]
      if (noisy-value < 0) [set noisy-value 0]     
      report noisy-value
end


to-report min-frequency
let l item 0 minority-rules
report count (turtle-set link-neighbors self) with [ my.rule  = l] / (degree + 1)
end

to-report max-frequency
let l item 0 majority-rules
report count (turtle-set link-neighbors self) with [ my.rule  = l] / (degree + 1)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Life Distributions  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;census-dist : fractions of people with age 0, 5, ...100
;life-expect   : expected life at ages 0, 5, ...100 (life expectancy = 1/M_age)

;expected rate at which people die per unit time at each age* = M_age* 
;probability of dying at age* = 1-exp(-M_age*) :: 

;F_age*= fraction of population of age* that dies within one year :: M_age*=-ln(1-F_age*)

;if M is done in intervals (as we have here), M is the value at age A+5


to init-age-USA2010 ;;Population fraction for ages according data colected
                                 ;By Lindsay M. Howden and Julie A. Meyer in
                                 ;Age and Sex Composition: 2010 Census briefs
                                 ;Reported fractions have an interval of 5 years starting from 0 until 100 years
  let census-dist (list 0.0654 0.0659 0.0670 0.0714 0.0699 0.0683 0.0647 0.0654 0.0677 0.0735 0.0722 0.0637 0.0545 0.0403 0.0301 0.0237 0.0186 0.0117 0.0047 0.0012 0.0002)
  
  ask turtles [
    
    let index floor random 21
    while [random-float 1 > item index census-dist]
          [
          set index floor random 21
          ]
    set age   ( (index) * 5 + random 5)
              ]
end



to set-life-distribution-USA2010 ;;Life expectation for ages according data colected by the Centers for Disease Control
                                 ;and Prevention’s National Center for Health Statistics (NCHS) USA 2010
                                 ;Murphy, Xu, and Kochanek 'Deaths: preliminary data 2010' National Vital Stat. Reports 60-4
                                 ;Reported ages have an interval of 5 years starting from 0 until 100 years

set life-expectation (list 78.7 74.3 69.3 64.4 59.5 54.8 50.0 45.3 40.6 36.0 31.5 27.2 23.1 19.2 15.5 12.2 9.2 6.6 4.7 3.3 2.4) 
set mortality-rate map [1 / ?] life-expectation
set prob-to-die map [ (1 - exp ( ( - 1 ) * ? )  )] mortality-rate 
set prob-die-imitation map [( 1 - exp (  ( ln ( 1 - ? )) / cultural-constant )) ] prob-to-die
end

to-report prob-die
let index ceiling ( floor ( age   / 5 )) 
if index > 20 [set index 20]
if index < 0  [set index 0]

let p.die item (index) prob-die-imitation
report p.die 

;let mortality item index mortality-rate 
;let prob-to-die ( 1 - exp ( (- 1 ) *  mortality ) )
;let prob-die-imitation ( 1 - exp (  (ln ( 1 - prob-to-die )) / cultural-constant ))
;report prob-die-imitation 
end


to replacement
  ask turtles [    
  if      random-float 1  < prob-die 
             [
             set ticks.at.death.list lput ticks ticks.at.death.list
             replace
             set shape "target"
             ]
       ]
end   


to replace  
    set age 0
    set rule? false
    set behavior? false
    set my.rule (random 4) + 1 
    set shape "face sad"
    set size sizeT
    set satisfaction2 1
    ifelse random-float 1.0 < .5 ;(inicoop / 100)
        [set cooperate true]
        [set cooperate false]
    establish-color
    set score 0.0
    set rule? false
    set behavior? false
set chances.imitations 0
;set shuffled? false
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layout  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Layout code citation
;Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
;Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt degree ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end


to layout
  ;; the number here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 10 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
 ;  layout-spring (turtles) links 0.4 6 1

;    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Topologies  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup-Topology
  if-else Topology = "Lattice"       [Create-Lattice]
    [
    if-else Topology = "Random"        [Create-Random-Network]
      [if-else Topology = "Small-World" [Create-Small-World][Create-Barabasi]                                                   
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lattice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Lattice 


let rows  floor (sqrt Num-Agents )
let columns ( floor (Num-Agents / rows ) ) 

set rows ( rows / 2 ) 
set columns ( columns / 2 ) 

let myPatches patches with [ abs (pxcor ) <= rows and abs (pycor ) <= columns ]

ask myPatches [sprout 1] ;Num-Agents
ask turtles [create-links-with turtles-on neighbors]

;wrap world 
let maxxcor max [xcor] of turtles
let minxcor min [xcor] of turtles
let maxycor max [ycor] of turtles
let minycor min [ycor] of turtles

let rightcorner patches with   [pxcor = maxxcor]
let leftcorner patches  with   [pxcor = minxcor]
let bottomcorner patches with  [pycor = maxycor]
let topcorner patches with     [pycor = minycor] 

set rightcorner turtles-on rightcorner
set leftcorner turtles-on leftcorner
set bottomcorner turtles-on bottomcorner
set topcorner turtles-on topcorner

ask rightcorner [create-links-with leftcorner with [ pycor = [pycor] of myself ]]
ask rightcorner [create-links-with leftcorner with [ pycor = [pycor + 1] of myself]]
ask rightcorner [create-links-with leftcorner with [ pycor = [pycor - 1] of myself ]]

ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor] of myself ]]
ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor + 1 ] of myself ]]
ask bottomcorner [create-links-with topcorner with [ pxcor = [pxcor - 1] of myself  ]]


let corners (patch-set  patch minxcor minycor patch minxcor maxycor patch maxxcor maxycor patch maxxcor minycor)
ask turtles-on corners [create-links-with other turtles-on corners] 


set success? true
spread-turtles
set sizeT [size] of one-of turtles 


end

to spread-turtles
let num-rows  length remove-duplicates sort [ycor] of turtles
let num-cols  length remove-duplicates sort [xcor] of turtles 

let horizontal-spacing (world-width / num-cols) 
let vertical-spacing   (world-height / num-rows) 


ask turtles [ 
     let x ( xcor * horizontal-spacing)
     let y ( ycor * vertical-spacing) 
     setxy (x) (y) 
] 

;ask turtles [set size ceiling sqrt distance  (  one-of link-neighbors ) * 1.5]
ask turtles [set size min (list horizontal-spacing vertical-spacing)]
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random Network ;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Random-Network 
  ;; Make a circle of turtles
  create-turtles Num-Agents
  layout-circle sort turtles  (radius )

while [not success?]
[  ;; create links
  ask links [die]
  ask turtles [
    ;; each pair of turtles is only considered once
  create-links-with turtles with [self > myself and
                                  random-float 1.0 < Connection-Probability]
              ]

;  nw:generate-random turtles links Num-Agents Connection-Probability
;  nw:set-context turtles links
  set success? do-calculations
]
 ;ask links [set color gray]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Barabasi ;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Barabasi 
set success? true
  ;; Make a circle of turtles
create-turtles Num-Agents
layout-circle sort turtles  (radius )

while [not success?]
[  ;; create links
ask links [die]
nw:generate-preferential-attachment turtles links Num-Agents
nw:set-context turtles links
set success? do-calculations
]

;ask links [set color gray]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Small World  ;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to Create-Small-World ;; taken from the netlogo library
create-turtles Num-Agents
layout-circle sort turtles (radius )
wire-rewire-them
end

to wire-them
ask links [die]
 let n 0
  while [n < Num-Agents]
  [;; make edges with the next "Initial-Neighbours"
  let i 1
  while [i <= ceiling ( Initial-Neighbours / 2 ) ]
  [
    ask turtle n [create-link-with turtle ((n + i) mod Num-Agents)
                  [set rewired? false]
                  ]
    set i i + 1
  ]
 set n (n + 1)
 ]
end

to wire-rewire-them

  ;; set up a variable to see if the network is connected
  set success? false

  ;; if we end up with a disconnected network, keep trying
  while [not success?] [
    ;; kill the old lattice, reset neighbors, and create new lattice    
    wire-them
    ask links [
      ;; whether to rewire it or not?
      if (random-float 1) < Rewiring-Probability
      [
        ;; "a" remains the same
        let node1 end1
        ;; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < ( Num-Agents - 1)
        [
          ;; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ;; wire the new edge
          ask node1 [ create-link-with node2 [ set rewired? true ] ]
        set rewired? true
        ]
      ]
      
      ;; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]
    ;; check to see if the new network is connected and calculate path length and clustering
    ;; coefficient at the same time
    nw:set-context turtles links
    set success? do-calculations
  
  ]
;ask links [set color gray]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scale Free  ;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;to Create-Scale-Free
;set graphical? false
;setup-sf-parameters
;set success? false
;let connected? false 
;
;create-turtles Num-Agents [
;  set xcor random-xcor
;  set ycor random-ycor
;  set degree infinity * (-1)
;  ;layout-circle sort turtles (max-pxcor - 1)
;  ]
;let trials 1 
;while [not success?] ;tells us if created network is connected
;[
;show "constructing"
;while [not connected?] ;tells us if sequence has the potential to create a connected network 
;[
;while [not graphical?] ; verify if created sequence is graphical
;[
;create-degree-sequence
;set graphical? verify-graphicality sequence Num-Agents
;show "sequence"
;]
;
;if-else sum sequence >= 2 * (Num-Agents - 1) [set connected? true]
;                                             [set graphical? false]
;]
;configuration-model ;construct network
;;havel-hakimi
;nw:set-context turtles links
;set success? do-calculations ;is it connected?
;if success? = false [set trials trials + 1 ] ;try again to make it connected
;
;if trials > 30      [set success? false ;if sufficient trials, start again
;                     set graphical? false  
;                     set connected? false                              
;                     set trials 1
;                    ]
;;ask links [set color gray]
;]
;end
;
;to assign-degrees-to-turtles [degree-sequence]
;let i 0
;foreach degree-sequence
;[
;ask item i (sort turtles)
; [
;   set degree ?
;   set free-stubs ?
; ]
;set i ( i + 1)
;]
;end
;
;to Havel-Hakimi ;;Havel Hakimi procedure constructs a graph from degree sequence, only produces a subset of all possible graphs with given sequence
;let free-turtles turtles with [free-stubs > 0]
;while [any? free-turtles]
;[
;set free-turtles turtles with [free-stubs > 0]
;let current-turtle max-one-of free-turtles [free-stubs]
;let forbiden-turtles [link-neighbors] of current-turtle
;ask current-turtle [set free-turtles other free-turtles with [not (member? self forbiden-turtles)]]
;
;while[ [free-stubs] of current-turtle > 0]
;[
;let link-turtle max-one-of free-turtles [free-stubs]
;ask current-turtle [create-link-with link-turtle]
;ask (turtle-set current-turtle link-turtle) [set free-stubs (free-stubs - 1)]
;ask link-turtle [set free-turtles other free-turtles]
;]
;]
;layout-circle turtles radius
;end
;
;to configuration-model ;;star constrained graphicality configuration model more efficient, 
;                       ;;produces greater variety of graphs. The probability of producing a certain graph can be calculated for ensemble values
;
;assign-degrees-to-turtles sequence
;clear-links
;
;let value -1 
;let id -1
;
;let residual-sequence sequence
;let possible []
;
;let current-turtle turtle (position (max residual-sequence) residual-sequence)
;let free-turtles turtles with [free-stubs > 0]
;let forbiden-turtles [link-neighbors] of current-turtle
;let link-turtle one-of turtles
;
;let contradiction? false 
;
;while [any? free-turtles and not contradiction? ]
;[
;set current-turtle turtle (position (max residual-sequence) residual-sequence)
;set forbiden-turtles [link-neighbors] of current-turtle
;set free-turtles turtles with [free-stubs > 0]
;ask current-turtle [set free-turtles other free-turtles with [ not (member? self forbiden-turtles)]]
;
;while [ [free-stubs] of current-turtle > 0]
;[
;
;ifelse (length filter [? > 0] residual-sequence) = 2 
;        and 
;        (sum residual-sequence = 2)[set link-turtle one-of free-turtles                                   
;                                    ]
;                                 
;                                   [
;                                   set possible (possible-connections current-turtle free-turtles residual-sequence)
;                                   if-else empty? possible
;                                   [set contradiction? true];start construction again. 
;                                   [set link-turtle turtle (one-of possible)]
;                                   ]
;
;ask current-turtle [create-link-with link-turtle]
;ask (turtle-set link-turtle current-turtle) [set free-stubs (free-stubs - 1)]
;ask link-turtle [set free-turtles other free-turtles]
;
;set id [who] of current-turtle
;set value (item id residual-sequence)
;set residual-sequence replace-item id residual-sequence (value - 1) 
;
;set id [who] of link-turtle
;set value (item id residual-sequence)
;set residual-sequence replace-item id residual-sequence (value - 1) 
;]
;]
;end
;
;
;to-report possible-connections [current-turtle free-turtles residual-sequence]
;let final-list n-values Num-Agents [false]
;let candidates []
;let subsequence residual-sequence
;let value -1
;let id -1
;
;foreach sort free-turtles
;[
;set subsequence residual-sequence
;
;set id [who] of current-turtle
;set value (item id residual-sequence)
;set subsequence replace-item id subsequence (value - 1)
;
;set id [who] of ?
;set value (item id residual-sequence)
;set subsequence replace-item id subsequence (value - 1)
;
;set subsequence reverse sort subsequence
;set subsequence filter [? > 0 ] subsequence
;
;ifelse length subsequence = 2 and sum subsequence = 2 [set final-list replace-item ([who] of ?) final-list true] 
;[
; ifelse item 0 subsequence >= length(subsequence) [set final-list replace-item ([who] of ?) final-list false] 
; [
;   set final-list replace-item ([who] of ?) final-list (verify-graphicality subsequence length subsequence)
; ]
;]
;
;]
;
;set candidates (positions final-list true)
;report candidates
;end
;
;to-report positions [a-list value] 
;  let ids n-values (length a-list) [?] 
;  let ids-values map [list ? item ? a-list] ids 
;  report map [first ?] filter [item 1 ? = value] ids-values 
;end 
;
;
;to-report verify-graphicality [seq length-sequence]
;; k goes from 0 to length sequence - 1
;let x_k n-values (length-sequence) [false]
;let seqqx n-values (length-sequence) [?]
;set seqqx  map [? + 1] seqqx 
;
;let k_star 0
;let flag false 
;
;let i (-1)
;
;if-else max seq > (length-sequence - 1) [set flag false]
;[if-else (sum seq) mod 2 != 0 [set flag false]
;[
;
;;set x_k replace-item 0 x_k length-sequence
;while [not flag]
;[
;;;find position of min index i such that sequence < seqqx(i)
;let pos item (i + 1) seqqx
;set pos max-position pos seq
;set x_k replace-item (i + 1) x_k pos
;if item (i + 1) x_k < (i + 1)[set flag true]
;set i (i + 1)
;]
;
;let j 0
;set flag false
;while [j < (length x_k ) and not flag]
;[
;if item j (x_k) < (j + 1) 
;[
;set k_star j
;set flag true 
;]
;set j (j + 1)
;]
;
;
;let k 0
;let L_k item k seq
;let R_k (length-sequence - 1)
;set flag false 
;set k 1
;
;while [not flag and k <= (length-sequence - 1)]
;[
;;;;show k
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;set L_k (L_k + item k seq)
;
;if-else k < k_star
;[set R_k (R_k + (item k x_k) - 1)]
;[set R_k (R_k + 2 * k - (item k seq))]
;
;if L_k > R_k [set flag true]
;set k (k + 1)
;]
;
;set flag (not flag)
;]]
;
;report flag
;end
;
;
;to-report max-position [number seq]
;let count-positions 0
;let max-iter (length seq)
;let flag false 
;
;while [ count-positions <  max-iter and not flag ]
;[
;;show count-positions
;if-else (item count-positions seq) < number
;[set flag true]
;[set count-positions (count-positions + 1)]
;]
;report (count-positions) 
;end
;
;
;
;
;to  create-degree-sequence  ;create sequence with sum of degrees even
;let parity 1
;while [parity mod 2 != 0]
;[
;;create uniform
;set uniform n-values (Num-Agents) [random-float 1]
;set sequence []
;foreach uniform [set sequence lput (create-one-degree ?) sequence]
;set parity sum sequence
;]
;set sequence (sort sequence)
;set sequence (reverse sequence)
;end
;
;
;to-report create-one-degree [R]
;let x 0
;let S (item x p_k)/ Z
;while [S < R]
;[
;set x (x + 1)
;set S (S + ((item x p_k) / Z))
;]
;report (x + 1)
;end
;
;to setup-sf-parameters ;set seq p_k (1, nodes-1,1)^(-g)
;set p_k n-values (Num-Agents - 1 ) [?]
;set p_k map [? + 1] p_k 
;set p_k map [? ^ (- Scale-Free-Exponent)] p_k
;set Z sum p_k
;end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network Computations ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connected Network? and Clustering 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; do-calculations reports true if the network is connected, finds path lengths 

to-report do-calculations
  
  let connected? true  
  
  ;let num-connected-pairs sum [length remove false (remove 0 distance-from-other-turtles)] of turtles
  ;;; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ;;; and none of those distances should be infinity.
  ;ifelse ( num-connected-pairs != (Num-Agents * (Num-Agents - 1) ))
  ;[set average-path-length infinity
  ; set connected? false
  ;]
  ;[
  let mean-path-length nw:mean-path-length
  ;]
  ;;find the clustering coefficient and add to the aggregate for all iterations  
  ;; report whether the network is connected or not
if not is-number? mean-path-length [set connected? false]
report connected?
end






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Outputs and Plots ;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
to my-update-plots  
  set-current-plot "Cooperation and Satisfaction"
  set-current-plot-pen "Cooperation" 
  plot cooperation-rate
;  set-current-plot-pen "satisfaction"
;  plot satisfaction-rate
  set-current-plot-pen "Satisfaction"
  plot satisfaction-rate2
;  set-current-plot-pen "Happy and Cooperating"
;  plot count turtles with [shape = "face happy" and cooperate] / Num-Agents
 
 
  set-current-plot "Population"
  set-current-plot-pen "Maxi"
  plot maxi / Num-Agents
  set-current-plot-pen "Mini"
  plot mini / Num-Agents
  set-current-plot-pen "Conf"
  plot conf / Num-Agents
  set-current-plot-pen "Anti"
  plot anti / Num-Agents
  

  set-current-plot "Age Plot"
  set-current-plot-pen "maxi"
  ifelse maxi > 0 [plot mean [age] of turtles with [ my.rule  = 1 ]][plot -1]
  set-current-plot-pen "mini"
  ifelse mini > 0 [plot mean [age] of turtles with [ my.rule  = 2 ]][plot -1]
  set-current-plot-pen "conf"
  ifelse conf > 0 [plot mean [age] of turtles with [ my.rule  = 3 ]][plot -1]
  set-current-plot-pen "anti"
  ifelse anti > 0 [plot mean [age] of turtles with [ my.rule  = 4 ]][plot -1]
  set-current-plot-pen "all"
  plot mean [age] of turtles
 
 
 
 ;;;UNCOMMENT TO SEE PLOTS ON WIDGET 
;  let maxi-t turtles-maxi with [ cooperate = TRUE]
;  let maxi-f turtles-maxi with [ cooperate = FALSE]
;  let maxi-h turtles-maxi with [ last best.history  = TRUE]
;  let maxi-s turtles-maxi with [ last best.history  = FALSE]
;  let maxi-sc turtles-maxi with [ last best.history  = FALSE and cooperate = TRUE]
;  let maxi-hc turtles-maxi with [ last best.history  = TRUE and cooperate = TRUE]
;  let maxi-sd turtles-maxi with [ last best.history  = FALSE and cooperate = FALSE]
;  let maxi-hd turtles-maxi with [ last best.history  = TRUE and cooperate = FALSE]
;  
;  
;  
;  let mini-t turtles-mini with [ cooperate = TRUE]
;  let mini-f turtles-mini with [ cooperate = FALSE]
;  let mini-h turtles-mini with [ last best.history  = TRUE]
;  let mini-s turtles-mini with [ last best.history  = FALSE]
; 
; 
;  let conf-t turtles-conf with [ cooperate = TRUE]
;  let conf-f turtles-conf with [ cooperate = FALSE]
;  let conf-h turtles-conf with [ last best.history  = TRUE]
;  let conf-s turtles-conf with [ last best.history  = FALSE]
; 
; 
;  let anti-t turtles-anti with [ cooperate = TRUE]
;  let anti-f turtles-anti with [ cooperate = FALSE]
;  let anti-h turtles-anti with [ last best.history  = TRUE]
;  let anti-s turtles-anti with [ last best.history  = FALSE]
;  
;  let mini-sc turtles-mini with [ last best.history  = FALSE and cooperate = TRUE]
;  let conf-sc turtles-conf with [ last best.history  = FALSE and cooperate = TRUE]
;  let anti-sc turtles-anti with [ last best.history  = FALSE and cooperate = TRUE]
;  
;  let mini-hc turtles-mini with [ last best.history  = TRUE and cooperate = TRUE]
;  let conf-hc turtles-conf with [ last best.history  = TRUE and cooperate = TRUE]
;  let anti-hc turtles-anti with [ last best.history  = TRUE and cooperate = TRUE]
;  
;  
;  let mini-sd turtles-mini with [ last best.history  = FALSE and cooperate = FALSE]
;  let conf-sd turtles-conf with [ last best.history  = FALSE and cooperate = FALSE]
;  let anti-sd turtles-anti with [ last best.history  = FALSE and cooperate = FALSE]
;
;  
;  let mini-hd turtles-mini with [ last best.history  = TRUE and cooperate = FALSE]
;  let conf-hd turtles-conf with [ last best.history  = TRUE and cooperate = FALSE]
;  let anti-hd turtles-anti with [ last best.history  = TRUE and cooperate = FALSE]
; 
;
;  set-current-plot "Scores Coop"
;  set-current-plot-pen "mini"
;  ifelse count mini-t > 0 [plot mean [score] of mini-t ][plot -1]
;
;  set-current-plot "Scores Coop"
;  set-current-plot-pen "conf"
;  ifelse count conf-t > 0 [plot mean [score] of conf-t ][plot -1]  
;
;  set-current-plot "Scores Coop"
;  set-current-plot-pen "anti"
;  ifelse count anti-t > 0 [plot mean [score] of anti-t ][plot -1]
;
;  set-current-plot "Scores Defecting"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-f > 0 [plot mean [score] of maxi-f ][plot -1]
;
;  set-current-plot "Scores Defecting"
;  set-current-plot-pen "mini"
;  ifelse count mini-f > 0 [plot mean [score] of mini-f ][plot -1]
;
;  set-current-plot "Scores Defecting"
;  set-current-plot-pen "conf"
;  ifelse count conf-f > 0 [plot mean [score] of conf-f ][plot -1]  
;
;  set-current-plot "Scores Defecting"
;  set-current-plot-pen "anti"
;  ifelse count anti-f > 0 [plot mean [score] of anti-f ][plot -1]
;
;  set-current-plot "Scores Happy"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-h > 0 [plot mean [score] of maxi-h ][plot -1]
;  set-current-plot "Scores Happy"
;  set-current-plot-pen "mini"
;  ifelse count mini-h > 0 [plot mean [score] of mini-h ][plot -1]
;  set-current-plot "Scores Happy"
;  set-current-plot-pen "conf"
;  ifelse count conf-h > 0 [plot mean [score] of conf-h ][plot -1]  
;  set-current-plot "Scores Happy"
;  set-current-plot-pen "anti"
;  ifelse count anti-h > 0 [plot mean [score] of anti-h ][plot -1]
;
;
;  set-current-plot "Scores UnHappy"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-s > 0 [plot mean [score] of maxi-s ][plot -1]
;  set-current-plot "Scores UnHappy"
;  set-current-plot-pen "mini"
;  ifelse count mini-s > 0 [plot mean [score] of mini-s ][plot -1]
;  set-current-plot "Scores UnHappy"
;  set-current-plot-pen "conf"
;  ifelse count conf-s > 0 [plot mean [score] of conf-s ][plot -1]  
;  set-current-plot "Scores UnHappy"
;  set-current-plot-pen "anti"
;  ifelse count anti-s > 0 [plot mean [score] of anti-s ][plot -1]
;
;
;  set-current-plot "S&C"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-sc > 0[plot mean [score] of maxi-sc][plot -1]
;  set-current-plot "S&C"
;  set-current-plot-pen "mini"
;  ifelse count mini-sc > 0[plot mean [score] of mini-sc][plot -1]
;  set-current-plot "S&C"
;  set-current-plot-pen "conf"
;  ifelse count conf-sc > 0[plot mean [score] of conf-sc][plot -1]
;  set-current-plot "S&C"
;  set-current-plot-pen "anti"
;  ifelse count anti-sc > 0[plot mean [score] of anti-sc][plot -1]
;
;
;set-current-plot "H&C"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-hc > 0[plot mean [score] of maxi-hc][plot -1]
;set-current-plot "H&C"
;  set-current-plot-pen "mini"
;  ifelse count mini-hc > 0[plot mean [score] of mini-hc][plot -1]
;set-current-plot "H&C"
;  set-current-plot-pen "conf"
;  ifelse count conf-hc > 0 [plot mean [score] of conf-hc][plot -1]
;set-current-plot "H&C"
;  set-current-plot-pen "anti"
;  ifelse count anti-hc > 0 [plot mean [score] of anti-hc][plot -1]
;
;set-current-plot "S&D"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-sd > 0[plot mean [score] of maxi-sd][plot -1]
;set-current-plot "S&D"
;  set-current-plot-pen "mini"
;  ifelse count mini-sd > 0[plot mean [score] of mini-sd][plot -1]
;set-current-plot "S&D"
;  set-current-plot-pen "conf"
;  ifelse count conf-sd > 0[plot mean [score] of conf-sd][plot -1]
;set-current-plot "S&D"
;  set-current-plot-pen "anti"
;  ifelse count anti-sd > 0[plot mean [score] of anti-sd][plot -1]
;
;set-current-plot "H&D"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-hd > 0[plot mean [score] of maxi-hd][plot -1]
;set-current-plot "H&D"
;  set-current-plot-pen "mini"
;  ifelse count mini-hd > 0[plot mean [score] of mini-hd][plot -1]
;set-current-plot "H&D"
;  set-current-plot-pen "conf"
;  ifelse count conf-hd > 0[plot mean [score] of conf-hd][plot -1]
;set-current-plot "H&D"
;  set-current-plot-pen "anti"
;  ifelse count anti-hd > 0[plot mean [score] of anti-hd][plot -1]
;
;
;
;
;
;
;
;set-current-plot "Sat S&C"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-sc > 0[plot mean [satisfaction2] of maxi-sc][plot -1]
;set-current-plot "Sat S&C"
;  set-current-plot-pen "mini"
;  ifelse count mini-sc > 0[plot mean [satisfaction2] of mini-sc][plot -1]
;set-current-plot "Sat S&C"
;  set-current-plot-pen "conf"
;  ifelse count conf-sc > 0[plot mean [satisfaction2] of conf-sc][plot -1]
;set-current-plot "Sat S&C"
;  set-current-plot-pen "anti"
;  ifelse count anti-sc > 0[plot mean [satisfaction2] of anti-sc][plot -1]
;
;
;set-current-plot "Sat H&C"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-hc > 0[plot mean [satisfaction2] of maxi-hc][plot -1]
;set-current-plot "Sat H&C"
;  set-current-plot-pen "mini"
;  ifelse count mini-hc > 0[plot mean [satisfaction2] of mini-hc][plot -1]
;set-current-plot "Sat H&C"
;  set-current-plot-pen "conf"
;  ifelse count conf-hc > 0 [plot mean [satisfaction2] of conf-hc][plot -1]
;set-current-plot "Sat H&C"
;  set-current-plot-pen "anti"
;  ifelse count anti-hc > 0 [plot mean [satisfaction2] of anti-hc][plot -1]
;
;set-current-plot "Sat S&D"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-sd > 0[plot mean [satisfaction2] of maxi-sd][plot -1]
;set-current-plot "Sat S&D"
;  set-current-plot-pen "mini"
;  ifelse count mini-sd > 0[plot mean [satisfaction2] of mini-sd][plot -1]
;set-current-plot "Sat S&D"
;  set-current-plot-pen "conf"
;  ifelse count conf-sd > 0[plot mean [satisfaction2] of conf-sd][plot -1]
;set-current-plot "Sat S&D"
;  set-current-plot-pen "anti"
;  ifelse count anti-sd > 0[plot mean [satisfaction2] of anti-sd][plot -1]
;
;set-current-plot "Sat H&D"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-hd > 0[plot mean [satisfaction2] of maxi-hd][plot -1]
;set-current-plot "Sat H&D"
;  set-current-plot-pen "mini"
;  ifelse count mini-hd > 0[plot mean [satisfaction2] of mini-hd][plot -1]
;set-current-plot "Sat H&D"
;  set-current-plot-pen "conf"
;  ifelse count conf-hd > 0[plot mean [satisfaction2] of conf-hd][plot -1]
;set-current-plot "Sat H&D"
;  set-current-plot-pen "anti"
;  ifelse count anti-hd > 0[plot mean [satisfaction2] of anti-hd][plot -1]
;
;set-current-plot "Sat Coop"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-t > 0 [plot mean [satisfaction2] of maxi-t ][plot -1]
;set-current-plot "Sat Coop"
;  set-current-plot-pen "mini"
;  ifelse count mini-t > 0 [plot mean [satisfaction2] of mini-t ][plot -1]
;set-current-plot "Sat Coop"
;  set-current-plot-pen "conf"
;  ifelse count conf-t > 0 [plot mean [satisfaction2] of conf-t ][plot -1]  
;set-current-plot "Sat Coop"
;  set-current-plot-pen "anti"
;  ifelse count anti-t > 0 [plot mean [satisfaction2] of anti-t ][plot -1]
;
;  set-current-plot "Sat Defecting"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-f > 0 [plot mean [satisfaction2] of maxi-f ][plot -1]
;  set-current-plot "Sat Defecting"
;  set-current-plot-pen "mini"
;  ifelse count mini-f > 0 [plot mean [satisfaction2] of mini-f ][plot -1]
;  set-current-plot "Sat Defecting"
;  set-current-plot-pen "conf"
;  ifelse count conf-f > 0 [plot mean [satisfaction2] of conf-f ][plot -1]  
;  set-current-plot "Sat Defecting"
;  set-current-plot-pen "anti"
;  ifelse count anti-f > 0 [plot mean [satisfaction2] of anti-f ][plot -1]
;
;  set-current-plot "Sat Happy"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-h > 0 [plot mean [satisfaction2] of maxi-h ][plot -1]
;  set-current-plot "Sat Happy"
;  set-current-plot-pen "mini"
;  ifelse count mini-h > 0 [plot mean [satisfaction2] of mini-h ][plot -1]
;  set-current-plot "Sat Happy"
;  set-current-plot-pen "conf"
;  ifelse count conf-h > 0 [plot mean [satisfaction2] of conf-h ][plot -1]  
;  set-current-plot "Sat Happy"
;  set-current-plot-pen "anti"
;  ifelse count anti-h > 0 [plot mean [satisfaction2] of anti-h ][plot -1]
;
;
;  set-current-plot "Sat UnHappy"
;  set-current-plot-pen "maxi"
;  ifelse count maxi-s > 0 [plot mean [satisfaction2] of maxi-s ][plot -1]
;  set-current-plot "Sat UnHappy"
;  set-current-plot-pen "mini"
;  ifelse count mini-s > 0 [plot mean [satisfaction2] of mini-s ][plot -1]
;  set-current-plot "Sat UnHappy"
;  set-current-plot-pen "conf"
;  ifelse count conf-s > 0 [plot mean [satisfaction2] of conf-s ][plot -1]  
;  set-current-plot "Sat UnHappy"
;  set-current-plot-pen "anti"
;  ifelse count anti-s > 0 [plot mean [satisfaction2] of anti-s ][plot -1]
;
;
;  set-current-plot "Maxi Neighbors"
;  set-current-plot-pen "maxi"
;  ifelse  maxi > 0 [plot   mean [n.maxi]  of turtles-maxi][plot -1]
;  set-current-plot "Maxi Neighbors"
;  set-current-plot-pen "mini"
;  ifelse  mini > 0 [plot mean [n.mini]  of turtles-maxi][plot -1]
;  set-current-plot "Maxi Neighbors"
;  set-current-plot-pen "conf"
;  ifelse  conf > 0 [plot mean [n.conf]  of turtles-maxi][plot -1]  
;  set-current-plot "Maxi Neighbors"
;  set-current-plot-pen "anti"
;  ifelse  anti > 0 [plot mean [n.anti]  of turtles-maxi ][plot -1]
;
;  set-current-plot "Mini Neighbors"
;  set-current-plot-pen "maxi"
;  ifelse  maxi > 0 [plot   mean [n.maxi]  of turtles-mini][plot -1]
;  set-current-plot "Mini Neighbors"
;  set-current-plot-pen "mini"
;  ifelse  mini > 0 [plot mean [n.mini]  of turtles-mini][plot -1]
;  set-current-plot "Mini Neighbors"
;  set-current-plot-pen "conf"
;  ifelse  conf > 0 [plot mean [n.conf]  of turtles-mini][plot -1]  
;  set-current-plot "Mini Neighbors"
;  set-current-plot-pen "anti"
;  ifelse  anti > 0 [plot mean [n.anti]  of turtles-mini ][plot -1]
;
;  set-current-plot "Conf Neighbors"
;  set-current-plot-pen "maxi"
;  ifelse  maxi > 0 [plot   mean [n.maxi]  of turtles-conf][plot -1]
;  set-current-plot "Conf Neighbors"
;  set-current-plot-pen "mini"
;  ifelse  mini > 0 [plot mean [n.mini]  of turtles-conf][plot -1]
;  set-current-plot "Conf Neighbors"
;  set-current-plot-pen "conf"
;  ifelse  conf > 0 [plot mean [n.conf]  of turtles-conf][plot -1]  
;  set-current-plot "Conf Neighbors"
;  set-current-plot-pen "anti"
;  ifelse  anti > 0 [plot mean [n.anti]  of turtles-conf ][plot -1]
;
;  set-current-plot "Anti Neighbors"
;  set-current-plot-pen "maxi"
;  ifelse  maxi > 0 [plot   mean [n.maxi]  of turtles-anti][plot -1]
;  set-current-plot "Anti Neighbors"
;  set-current-plot-pen "mini"
;  ifelse  mini > 0 [plot mean [n.mini]  of turtles-anti][plot -1]
;  set-current-plot "Anti Neighbors"
;  set-current-plot-pen "conf"
;  ifelse  conf > 0 [plot mean [n.conf]  of turtles-anti][plot -1]  
;  set-current-plot "Anti Neighbors"
;  set-current-plot-pen "anti"
;  ifelse  anti > 0 [plot mean [n.anti]  of turtles-anti ][plot -1]
; 
;;  set-current-plot "Degrees Plot"
;;  set-current-plot-pen "maxi"
;;  ifelse maxi > 0 [plot mean [degree] of turtles with [ my.rule  = 1]][plot -1]
;;  set-current-plot-pen "mini"
;;  ifelse mini > 0 [plot mean [degree] of turtles with [ my.rule  = 2]][plot -1]
;;  set-current-plot-pen "conf"
;;  ifelse conf > 0 [plot mean [degree] of turtles with [ my.rule  = 3]][plot -1]
;;  set-current-plot-pen "anti"
;;  ifelse anti > 0 [plot mean [degree] of turtles with [ my.rule  = 4]][plot -1]
;  
;;  
;;  set-current-plot "Clustering Coefficient Plot"
;;  set-current-plot-pen "maxi"
;;  ifelse maxi > 0 [plot mean [node-clustering-coefficient] of turtles with [ my.rule  = 1]][plot -1]
;;  set-current-plot-pen "mini"
;;  ifelse mini > 0 [plot mean [node-clustering-coefficient] of turtles with [ my.rule  = 2]][plot -1]
;;  set-current-plot-pen "conf"
;;  ifelse conf > 0 [plot mean [node-clustering-coefficient] of turtles with [ my.rule  = 3]][plot -1]
;;  set-current-plot-pen "anti"
;;  ifelse anti > 0 [plot mean [node-clustering-coefficient] of turtles with [ my.rule  = 4]][plot -1]
;;  
;;  set-current-plot "Page Rank Plot"
;;  set-current-plot-pen "maxi"
;;  ifelse maxi > 0 [plot mean [page-rank] of turtles with [ my.rule  = 1]][plot -1]
;;  set-current-plot-pen "mini"
;;  ifelse mini > 0 [plot mean [page-rank] of turtles with [ my.rule  = 2]][plot -1]
;;  set-current-plot-pen "conf"
;;  ifelse conf > 0 [plot mean [page-rank] of turtles with [ my.rule  = 3]][plot -1]
;;  set-current-plot-pen "anti"
;;  ifelse anti > 0 [plot mean [page-rank] of turtles with [ my.rule  = 4]][plot -1]
; 

   
  end



to establish-color  ;; agent procedure
if-else Colormap-View = "Strategies"
[  if-else my.rule = 1        [set color red]
    [if-else my.rule = 2      [set color green]
      [if-else my.rule = 3    [set color blue]
                           [set color white]]]
]
[
  if-else cooperate [set color blue] [set color orange]
]
end

@#$#@#$#@
GRAPHICS-WINDOW
32
27
333
349
-1
-1
2.91
1
10
1
1
1
0
0
0
1
-50
49
-50
49
1
1
1
ticks
30.0

BUTTON
161
371
242
404
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
256
371
333
404
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
1

TEXTBOX
25
461
163
547
*Strategies colormap\n\nRed        Maxi\nGreen     Mini\nBlue     Conformist\nWhite      Anti-conf\n                      \n                       
9
0.0
0

SLIDER
402
32
583
65
*-strength-of-dilemma
*-strength-of-dilemma
0
0.5
0.2
0.01
1
NIL
HORIZONTAL

PLOT
827
40
1135
160
Cooperation and Satisfaction
time
NIL
0.0
500.0
0.0
1.0
true
true
"" ""
PENS
"Cooperation" 1.0 0 -2674135 true "" ""
"Satisfaction" 1.0 0 -13345367 true "" ""

PLOT
828
162
1135
288
Population
time
fraction
0.0
500.0
0.0
1.0
true
true
"" ""
PENS
"Maxi" 1.0 0 -2674135 true "" ""
"Mini" 1.0 0 -13840069 true "" ""
"Conf" 1.0 0 -13345367 true "" ""
"Anti" 1.0 0 -16777216 true "" ""

SLIDER
404
70
579
103
*-inicoop
*-inicoop
0
100
80
1
1
NIL
HORIZONTAL

SLIDER
388
361
589
394
*-Connection-Probability
*-Connection-Probability
0.0
1
0.076
.001
1
NIL
HORIZONTAL

INPUTBOX
489
287
565
347
*-Num-Agents
500
1
0
Number

TEXTBOX
389
271
563
289
*Choose Topology\n
10
15.0
1

CHOOSER
394
299
486
344
*-Topology
*-Topology
"Random" "Small-World" "Lattice"
2

TEXTBOX
391
349
601
367
Random Network Connection Probability
9
0.0
1

TEXTBOX
392
399
542
417
Small World Parameters
9
0.0
1

SLIDER
389
446
587
479
*-Rewiring-Probability
*-Rewiring-Probability
0
1
0
.001
1
NIL
HORIZONTAL

TEXTBOX
147
464
255
540
*Behaviour Colormap\n\nBlue   Cooperate\nOrange Defect
9
0.0
1

TEXTBOX
380
16
555
34
*Prisoner's Dilemma Parameters
9
15.0
1

SLIDER
390
410
587
443
*-Initial-Neighbours
*-Initial-Neighbours
1
*-Num-Agents - 1
8
1
1
NIL
HORIZONTAL

CHOOSER
30
372
145
417
Colormap-View
Colormap-View
"Strategies" "Behaviours"
0

MONITOR
683
153
741
198
Maxi %
count turtles with [ my.rule = 1 ] * 100 / count turtles
2
1
11

MONITOR
744
154
802
199
Mini %
count turtles with [my.rule = 2 ] * 100 / count turtles
2
1
11

MONITOR
683
202
743
247
Conf %
count turtles with [my.rule = 3 ]  * 100 / count turtles
2
1
11

MONITOR
744
202
803
247
Anti %
count turtles with [my.rule = 4 ] * 100 / count turtles
2
1
11

TEXTBOX
47
419
197
437
*Network Properties
9
0.0
1

TEXTBOX
381
109
590
133
*Add noise by replacing the population?
9
15.0
1

SWITCH
620
325
795
358
*-Initial-Random-Types?
*-Initial-Random-Types?
0
1
-1000

TEXTBOX
622
310
792
329
*Random Assignation of Types?
9
0.0
1

INPUTBOX
619
374
704
434
*-Initial-Maxi-%
100
1
0
Number

INPUTBOX
706
374
789
434
*-Initial-Mini-%
0
1
0
Number

INPUTBOX
620
434
703
494
*-Initial-Conf-%
0
1
0
Number

MONITOR
709
436
793
481
Initial-Anti-%
100 - *-Initial-Maxi-% - *-Initial-Mini-% - *-Initial-Conf-%
0
1
11

TEXTBOX
620
360
807
380
Otherwise Input % Initial Types
9
0.0
1

TEXTBOX
723
135
823
158
Types %
9
0.0
1

TEXTBOX
391
259
558
279
*Network Parameters
9
0.0
1

MONITOR
708
23
805
68
Cooperation %
count turtles with [cooperate ] * 100 / count turtles
2
1
11

MONITOR
708
73
804
118
Satisfaction %
mean [satisfaction2] of turtles * 100
2
1
11

SWITCH
404
507
571
540
Load-Topology
Load-Topology
1
1
-1000

INPUTBOX
454
543
530
603
*-fileIn
NIL
1
0
String

PLOT
829
290
1134
410
Age Plot
Count
Age
0.0
500.0
45.0
70.0
true
true
"" ""
PENS
"maxi" 1.0 0 -2674135 true "" ""
"mini" 1.0 0 -10899396 true "" ""
"conf" 1.0 0 -13345367 true "" ""
"anti" 1.0 0 -16777216 true "" ""
"all" 1.0 0 -2064490 true "" ""

BUTTON
274
465
347
498
Layout
layout
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
273
500
352
533
resize-nodes
resize-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
377
120
528
153
*-replacement?
*-replacement?
0
1
-1000

MONITOR
630
153
680
198
% New Turtles
count turtles with [shape = \"target\"] * 100 / count turtles
2
1
11

TEXTBOX
639
293
789
311
*Choose types distribution
9
15.0
1

SLIDER
377
154
526
187
*-cultural-constant
*-cultural-constant
.001
20
4.628
.001
1
NIL
HORIZONTAL

MONITOR
629
202
679
247
Mean Age 
mean [age] of turtles
2
1
11

SWITCH
376
188
527
221
*-innovation?
*-innovation?
1
1
-1000

BUTTON
180
405
320
438
NIL
setup-init-turtles
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
259
537
367
572
layout circle
layout-circle turtles radius
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@


## WHAT IS IT?

This is an implementation of a prisonner’s dilemma metamimetic game for different network topologies under netlogo 5.1.0. 


Agents play the prisoner's dilemma game with each one of their neighbours in a torus lattice or a network (small world or random).   

The parameter p corresponds to the strength of dilemma in the widget:



                                 Payoff Matrix
                                 -------------
                                    OPPONENT
          BEHAVIORS   Cooperate            Defect
                        -----------------------------
           Cooperate |(1-p, 1-p)            (0, 1)
      YOU            |
           Defect    |(1, 0)                (p, p)
    
            (x, y) = x: your score, y: your partner's score
            Note: higher the score (amount of the benefit), the better.


The agents can have one of 4 types:

Maxi : The agent tries to maximize the score (payoff)  
Mini : The agent tries to minimize the score  
Conformist: The agent tries to behave as the majority   
Anti-conformist: The agent tries to behave as the minority

Each round agents play they copy the most succesfull agent in their neighborhood according to their own current type: 

A Maxi agent would copy the type and behavior (C or D) of the agent in its neighborhood with highest payoffs. 
A Mini agent would copy the type and behavior (C or D) of the agent in its neighborhood with lower payoffs.
A Conformist would copy the type and behavior (C or D) that the majority of its neighborhood is using. 
An Anti-Conformist would copy the type and behavior (C or D) that the minority of its neighborhood is using.

   
## HOW TO USE IT

Decide the topology structure or load one.
Decide a Number of Agents that will be playing. 
Decide what percentage of agents should cooperate at the initial stage with inicoop.
Decide what is the strenght of the dilemma p.
If you are not loading a topology; choose the parameters for the desired topology. 
Choose to add noise to the model by renovating the population and have some agents die. 

## HOW IT WORKS

At each period: 

Each agent A plays a prisoner's dilemma game pairwise with all of its neighbours. The scores for all the pairwise games played are summed up to become the new payoffs of the agent.  

Each agent looks at the payoffs, decision-making rules and behaviours of other agents in its neighbourhood Gamma_A. 

For any agent A, if according to A's  type (payoffs based or non-materialistic) there is one neighbour B that is more successful than A himself, and if B has a different decision-making rule, then A copies the rule of agent B. In the case of two or more candidates to copy, then A chooses one of the rules at random. 

If according to its new rule of behaviour and its associated utility function, A is still not among the most successful agents in the neighborhood, then A copies the behaviour of the neighbour with the best situation.


## EXAMPLE:

 If agent A had the conformist type and if the majority of its neighbours have turned to maxi since last round, then A will adopt the maxi rule. Here, the imitation rule is used to update itself (reflexivity).
 If same agent A, which is now a maxi agent, played C last round but a D-player did strictly better than all of A’s neighbours (A included), then A will become a D-player. Here, the imitation rule is used to update the behaviour (metacognition).


## THINGS TO NOTICE
How do populations change with the strength of the dilemma?
What is the effect of noise with renovation of the population?
Where are agents located in the network at the attractor? 
What is the effect of the initial disposition towards cooperation? 
Are there differences in the final populations for each topology?

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
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="ASW" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>run-Logo 500</go>
    <final>export-graphL
export-coopL
export-propL
export-agesL</final>
    <exitCondition>condition or ticks &gt; 500</exitCondition>
    <enumeratedValueSet variable="Load-Topology">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.025" last="1"/>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.025" last="0.5"/>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-fileIn">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-cultural-constant">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-replacement?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0.076"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ASW2" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>run-Logo 500</go>
    <final>export-graphL
export-coopL
export-propL
export-agesL</final>
    <exitCondition>condition or ticks &gt; 500</exitCondition>
    <enumeratedValueSet variable="Load-Topology">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="0"/>
      <value value="5"/>
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-fileIn">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-cultural-constant">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-replacement?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0.076"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ASW3" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>run-Logo 500</go>
    <final>export-graphL
export-coopL
export-propL
export-agesL</final>
    <exitCondition>condition or ticks &gt; 500</exitCondition>
    <enumeratedValueSet variable="Load-Topology">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Colormap-View">
      <value value="&quot;Strategies&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Topology">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-Rewiring-Probability" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="*-Initial-Neighbours">
      <value value="12"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-inicoop">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="*-strength-of-dilemma" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="*-Initial-Random-Types?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Num-Agents">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-fileIn">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-cultural-constant">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Scale-Free-Exponent">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Conf-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Mini-%">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Initial-Maxi-%">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-replacement?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="*-Connection-Probability">
      <value value="0.076"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
VIEW
55
40
385
370
0
0
0
1
1
1
1
1
0
1
1
1
-50
49
-50
49

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
