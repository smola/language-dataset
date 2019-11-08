;;Autor : Delay E. (Université de Limoges)
;;7 decembre 2012 modéliser l'article de R.DION 1952  

breed [Districts District] ;les districte sont les quartiers
breed [wineMarkets wineMarket]
breed [foreigns foreign]
undirected-link-breed [linksPrim linkPrim]
undirected-link-breed [secLinks SecLink]

globals [
  ;des gloables qu'on va chercher a analyser en stat descriptive
 fitnessPatches 
 patchbefore
 prodCostGlob
 propPatchMont
 propPatchPlain
 globalQuality
 sumDiffQuality
 meanQuality
 gini-index-reserve
 lorenz-points
 gini-index-patch
 lorenz-points-patch
 meanQualityTotal
 meanQualityMountain
 meanQualityPlain
 maxnbplots
 DiffExtCentral
 nbcentralPlots
 meanPatchByNetwork
 
 ;;variable for gloablSetup
 nbCitys
 nbPLots
 whatWord
 demandF
 priceMaxWineF
 standarDevPop
 downerQ
 coefUpQuality
 logistique
]

patches-own[
  alti
  owner
  ageOfPlot
  plotVineType
  grapProduction
  winrAmount
  quality
  RelaQualityInit
  RelaQuality
  DiffQuality
]

wineMarkets-own [
  popB       ;population locale qui varie autour de 0 par une loi normal
  supplyB    ;la somme des productions de chaque patches
  demandB    ;les besoin locaux en vin
  stockB     ;ce que la ville est capable de stoker (demandB - supplyB)
  myplots    ;agentSet de tout mes patches
  distanceToMarketone ;la distance du marché centrale
  productionCost ;les cout de productions qui sont proportionnel à la somme des éloignement des parcelles
  prodCostNetWork ;les cout de production du réseau
  connection ;FRUE/FALSE estce que la ville est connecté
  networkMarket ;agentSet des marché connecté au marché principale
  distCostPC ;le cout de la distance eu marché principale
  plotsQuality ; la qualitée moyenne de myPlots
  
  supplyW
  demandW
  stockW
]

Districts-own[
  ownerTown
  bourgeois
  worker
  city
]

foreigns-own[
 demandForeign
 supplyForeign
 priceMax
 myWineMarket
 demandRest
]

links-own [
 demandFlux
]





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;   SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to globalSetup ;this setup is not interpreted when the model run on openMole
  ;We need here all interface slider elements and all hard variable
  set nbCitys nbCitys-i
  set nbPLots nbPLots-i
  set whatWord whatWord-i
  set demandF demandF-i
  set priceMaxWineF priceMaxWineF-i
  set standarDevPop standarDevPop-i
  set downerQ downerQ-i
  set coefUpQuality coefUpQuality-i
  set logistique logistique-i
end

to setup
  clear-all
  globalSetup
  commonSetup
end

to commonSetup
;  import-pcolors "montagne1.png" 
  let conterColor 2
  set patchbefore count patches with [owner != -9999]
  
  if whatWord = 1 [
    import-pcolors "montagne5.png"
    ask patches [set alti  pcolor]
  ]
  if whatWord = 0[
   ask patches [set alti 0]
  ]
  if whatWord = 2 [
    import-pcolors "montagne6.png"
    ask patches [set alti  pcolor]
  ]
  
  ask patches [
    set quality (alti + 0.1) * 10
    set owner -9999
    set plotVineType -9999
  ]

  if not any? patches with [quality <= 0][
   set meanQuality mean [quality] of patches  with [owner = -9999]
    ask patches [
      set RelaQualityInit quality / meanQuality * 100
    ]
  ]
  set-default-shape wineMarkets "house"
  set-default-shape Districts "circle"
  set-default-shape foreigns "boat"
;  set-default-shape parcels "plant"
  
  
  create-wineMarkets nbCitys [
    setxy random-pxcor random-pycor
    set size 3
    set label plotsQuality
    set supplyB 0
    set demandB 0
    set stockB 0
    set popB  100
    
    set supplyW 0
    set demandW 0
    set stockW 0
    
    
    set networkMarket (turtle-set)
    
;    pour que chaque marché ait une couleur différente
    set color conterColor
    set conterColor ifelse-value (conterColor < 140) [conterColor + 10] [conterColor - 139]
    set connection 0
  ]
  
  create-foreigns 1 [
;    setxy random-pxcor random-pycor
    setxy -30 0
    set priceMax priceMaxWineF
    set size 5
  ]
    
  ;;creation des parcelles viticole 
  ask wineMarkets [
    set myplots n-of  (random nbPLots + 1) patches in-radius 5 with [owner = -9999]
    ask myplots [
      set pcolor [color] of myself
      set owner [who] of myself
    ]
   ]
  
  ;;recherche de la ville la plus proche pour établir une liaison
  ask foreigns [
    set myWineMarket min-one-of wineMarkets [distance myself]
    ask myWineMarket [
      set connection 1
    ]
    create-linkPrim-with myWineMarket
  ]
  update-lorenz-and-gini
  reset-ticks
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Pour bouger n'importe quelle tortue avant de démarrer
to moveMousse
  if mouse-down? [
    let candidate min-one-of turtles [distancexy mouse-xcor mouse-ycor]
    if [distancexy mouse-xcor mouse-ycor] of candidate < 1 [
      ;; The WATCH primitive puts a "halo" around the watched turtle.
      watch candidate
      while [mouse-down?] [
        ;; If we don't force the view to update, the user won't
        ;; be able to see the turtle moving around.
        display
        ;; The SUBJECT primitive reports the turtle being watched.
        ask subject [ setxy mouse-xcor mouse-ycor ]
      ]
      ;; Undoes the effects of WATCH.  Can be abbreviated RP.
      reset-perspective
    ]
  ]
  clear-links 
  ask foreigns [
    set myWineMarket min-one-of wineMarkets [distance myself]
    ask myWineMarket [
      set connection 1
    ]
    create-linkPrim-with myWineMarket
  ]
end

to seeQuality
  ifelse marketColor = TRUE [
    ask patches [
      set pcolor alti
    ]
    ask wineMarkets [
      ask myPlots [
       set pcolor [color] of myself 
      ]
    ]
  ][
  ask patches [
    set pcolor scale-color red quality 0 100
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  RUN TO THE GRID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to run-to-grid [tmax]
  commonSetup
  while [ticks <= tmax]
  [go]
  reset-ticks
end

to-report go-stop?
;  ifelse nb-viti-non-innov = 0 
;  [report FALSE][report TRUE]
  
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;   GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  ;;pour le calcul du fitness
  set patchbefore count patches with [owner != -9999]
  exportmonde
  
  ask wineMarkets [
    dynPop]
    
  ;;demande n vin pour les étrangés
  ask foreigns [
    marketChoice
    foreignNeed
  ]
  
  ask wineMarkets [
    produce
    upQuality
    ageIncrement
    offer
    colonizePLots
    calculCost
  ]

   downQuality
   calculGlobalQuality
;   updateColor

  
 ask seclinks [
  killlinks 
 ]
 
 altirob
 statSpat
 
  ;;;;mise a jour du graph
  update-fitness2
  update-lorenz-and-gini
  updatePlot
  tick
end

to marketChoice
  ;;;;foreigns context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;calule de la capacité de production du réseau de marché!
  let surfViti 0
  let countNetworkPlots []
   ask myWineMarket [
     ask networkmarket [
       let countMyPlots count myplots
       set countNetworkPlots lput countMyPlots countNetworkPlots
     ]
     set surfViti sum countNetworkPlots + count myPlots
   ]
  let capProd surfViti * 7 
  
  ;;creation des connections
  if [prodCostNetWork] of myWineMarket  > priceMaxWineF [
    ask myWineMarket [
      let firtTestMarket wineMarkets with [connection = 0]
      if any? firtTestMarket [
        let potMark 0
          ;;connection au marchés les plus proches
          set potMark min-n-of 1 firtTestMarket [distance myself]
        
        create-secLinks-with potMark
        ask potMark [
          set connection 1
          set distanceToMarketone distance myself
        ]
        set networkMarket (secLink-neighbors with [connection = 1])
      ]
    ]
  ]
  
  ;;suprimer des connextion en cas de surproduction
  if demandF < capProd[
  ask myWineMarket [
    if any? networkmarket [
    let farewayMarket max-one-of networkmarket [distance myself]
    ask farewayMarket [
      set connection 0
      set distanceToMarketone 0
      ]
    set networkMarket (secLink-neighbors with [connection = 1])
    ]
   ]
  ]
  
  
end

to killlinks
  ;;;;links context
  if any? seclinks[
    ask seclinks with [[connection] of end2 = 0][
      die
     ]
  ]
end

to foreignNeed
   ;;;;foreigns context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ask myWineMarket [
    let countNetwork count networkMarket + 1
    set demandB demandB + (demandF / countNetwork)
    ask networkMarket [
      set demandB demandB + (demandF / countNetwork) ;- (distCostPC * 10)
    ]
  ]
end

to dynPop
  set popB popB + random-normal 0 standarDevPop
  set demandB popB
end

to produce
  ;;WineMarkets context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     ask myPlots [
;      set grapProduction 0
;      ;verification de l'age de la parcelle pour rentrer en production
;      if ageOfPlot > 3 [
;        set grapProduction 1
;      ]
;      if grapProduction = 1 [
;        set winrAmount 7
;      ]
;      
;     ]
ask myPlots [

        set winrAmount 7

     ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;Qualité du terrain;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to upQuality
  ;;;;;;WineMarkets Context
    if logistique = 1 [
        ask myPlots [
;;          ;set quality quality + coefUpQuality * quality * (1 - quality / 100) ;; fonction logistique de croissance de la Qualite
            set quality 100 * (1 / (1 + exp(- coefUpQuality * quality)))
        ]
    ]
    if logistique = 0  [
        ask myPlots [
          set quality quality + (coefUpQuality * quality) ;; fonction linéaire
        ]
    ]
  if any? myPlots [
    set plotsQuality mean [quality] of myPlots
  ]
end

to downQuality
  ask patches with [owner = -9999][
    if quality > 0.1 [
      set quality quality - downerQ
      if alti > 0.1 and quality < alti[
         set quality alti
      ] 
    ]
    if quality <= 0.1 [
      set quality 0.1
    ] 
  ]
end

;to updateColor
;  ask patches [
;   set pcolor quality 
;  ]
;end



to ageIncrement
  ;;;WineMarkets context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ask myPlots [
    set ageOfPlot ageOfPlot + 1
  ]
end

to offer
  ;;;WineMarkets context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  set supplyB sum [winrAmount] of myplots
  set stockB demandB - supplyB
end

to colonizePLots
  ;;;WineMarkets context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if demandB > supplyB and priceMaxWineF > productionCost [
    let potplotfinal (patch-set) ;collection de patchs pot
    ask myplots [
      let potplot patches in-radius 2 with [owner = -9999 ] ;critère de selection, recherche de voisins potentiels
      set potplotfinal (patch-set potplotfinal potplot) ;renseigne la collection
    ]
    if any? potplotfinal [
      let listloop [1 1 1 1 1]
      foreach listloop [
        let newplot min-one-of (potplotfinal with-max[quality])[distance myself]
        ask newplot [
          set pcolor [color] of myself
          set owner [who] of myself
        ]
        set myplots (patch-set myplots newplot)
      ]
    ]
  ]
  if demandB < supplyB or priceMaxWineF < productionCost[
  if any? myplots with-max [distance myself] [
    let frich n-of 1 myplots with-max [distance myself]
    if count frich > 0[
      ask frich [
        set pcolor alti
        set owner -9999
      ]
      set myplots myplots with [owner = [who] of myself]
    ]
  ]
  ]
end

to calculCost
  ;;;;;WineMarkets context;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  let  distcost 0
  ask myplots [
    let distplot distance myself
    set distcost  distcost + ( distplot * 5 )   ;;;5 est le cout de l'acheminement local
    ]
  set productionCost (distcost / 200 ) ;;; on peut ajouter des cout de productions ici
  set prodCostNetWork productionCost / (count Seclinks + 1) ;;; s'il n'y a pas de marché networke le cout de prod myeon sont les meme que les cout de prod


 
;; Si on travail sur un réseau de marché
  if any? networkMarket [
    let sumPrdCost productionCost
    ask networkMarket [
      let distBigMarket distance myself
      set distCostPC distBigMarket
      set sumPrdCost ( sumPrdCost + productionCost + distCostPC)
    ]
    let prodCostMoy sumPrdCost / count networkMarket
    set prodCostNetWork prodCostMoy
  ]

end


to update-fitness2
  if count patches with [owner != -9999] > 0 [
    set fitnessPatches count patches with [owner != -9999] / patchBefore
  ]
end

to altirob ;;pour tracer la courbe du nombre de patches en montagne et en plaine
  let testAlti count patches with [alti > 0]
    if testAlti != 0 [
      set propPatchMont (count patches with [alti >= 0.1 and owner != -9999]) / ( count patches with [alti >= 0.1] ) * 100
    ]
  set propPatchPlain  (count patches with [alti <= 0.1 and owner != -9999]) / ( count patches with [alti <= 0.1] ) * 100
end

to exportmonde
  if export = TRUE [
   if ticks = 0 [
     export-view  "export/quantity0.png"
    set marketColor FALSE
    seeQuality
    export-view  "export/quality0.png"
    set marketColor TRUE
    seeQuality
   ]
   if ticks = 130 [
    export-view  "export/quantity130.png"
    set marketColor FALSE
    seeQuality
    export-view  "export/quality130.png"
    set marketColor TRUE
    seeQuality
   ]
   if ticks = 300 [
     export-view  "export/quantity300.png"
    set marketColor FALSE
    seeQuality
    export-view  "export/quality300.png"
    set marketColor TRUE
    seeQuality
   ]
  ]
end

to statSpat
 set meanQualityTotal mean [quality] of patches  with [owner != -9999]

    ifelse any? patches with [alti >= 0.1 and owner != -9999] [
      set meanQualityMountain mean [quality] of patches with [owner != -9999 and alti >= 0.1]
      set meanQualityPlain mean [quality] of patches with [owner != -9999 and alti < 0.1]
    ][
    set meanQualityMountain 0
    set meanQualityPlain mean [quality] of patches with [owner != -9999]
    ]
    ask foreigns [
     ask  myWineMarket [
       set nbcentralPlots count myPlots
       if nbcentralPlots > maxnbplots [
        set maxnbplots  nbcentralPlots
       ]
       set DiffExtCentral maxnbplots - nbcentralPlots
     ]
    ]
    let listpatch []
    foreach sort wineMarkets with [connection = 1][
      set listpatch lput count [myPlots] of ? listpatch
    ]
    set meanPatchByNetwork mean listpatch
    
end

to calculGlobalQuality
  let tpsGlobalQuality []
  ask wineMarkets [
    set tpsGlobalQuality lput plotsQuality tpsGlobalQuality
    set label (precision plotsQuality 2)
    ]
  set globalQuality tpsGlobalQuality
end

to update-lorenz-and-gini
  
;;; pour avoir un gini les patches sur les patches cultivé 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  let patchOwnered patches with [owner != -9999]
  let countPatchOwner count patchOwnered
  
  let sorted-wealths sort [quality] of patchOwnered
  let total-wealth sum sorted-wealths
  let wealth-sum-so-far 0
  let index 0
  set gini-index-reserve 0
  set lorenz-points [] ;pour stocker un vecteur, lupt rajoute dans la liste
  repeat countPatchOwner [  ; une boucle sur l'ensemble des patches cultivé
    set wealth-sum-so-far (wealth-sum-so-far + item index sorted-wealths)
    set lorenz-points lput ((wealth-sum-so-far / total-wealth) * 100) lorenz-points
    set index (index + 1)
    set gini-index-reserve gini-index-reserve + (index / countPatchOwner) - (wealth-sum-so-far / total-wealth)
  ]
  set gini-index-reserve (gini-index-reserve / countPatchOwner) / 0.5


  ;;;;calcul du gini du nombre de patches
  let sorted-wealths-patch []
  foreach sort wineMarkets [
    set sorted-wealths-patch lput count[myPlots] of ? sorted-wealths-patch
  ]
  let total-wealth-patch sum sorted-wealths-patch
  let wealth-sum-so-far-patch 0
  let index-patch 0
  set gini-index-patch 0
  set lorenz-points-patch [] ;pour stocker un vecteur, lupt rajoute dans la liste
  repeat count wineMarkets [
    set wealth-sum-so-far-patch (wealth-sum-so-far-patch + item index-patch sorted-wealths-patch)
    set lorenz-points-patch lput ((wealth-sum-so-far-patch / total-wealth-patch) * 100) lorenz-points-patch
    set index-patch (index-patch + 1)
    set gini-index-patch gini-index-patch + ((index-patch / count wineMarkets) - (wealth-sum-so-far-patch / total-wealth-patch))

  ]
   set gini-index-patch (gini-index-patch / count wineMarkets) / 0.5
end  


to updatePlot
  
  set-current-plot "demend"
  ask WineMarkets [
    set-plot-pen-color color ;pour renvoyer la couleur du fermier
    plotxy ticks demandB
    ]
  set-current-plot "productionCost_WineMarkets"
  ask WineMarkets [
    set-plot-pen-color color ;pour renvoyer la couleur du fermier
    plotxy ticks productionCost
    ]
  set-current-plot "global-cost"
  ask foreigns [
    ask myWineMarket [
      set-plot-pen-color color
      plotxy ticks prodCostNetWork
;      set prodCostGlob prodCostNetWork
    ]
  ]
  
  set-current-plot "nbconection"
  ask foreigns [
    ask myWineMarket [
      set-plot-pen-color color
      plotxy ticks count networkMarket
    ]
  ]
  
  set-current-plot "meanQualityVineyard"
  ask wineMarkets [
      set-plot-pen-color color
      if count myPlots > 0 [
        plotxy ticks plotsQuality
      ]
    ]
  
  set-current-plot "mountainPlot"
  set-plot-pen-color gray
  plotxy ticks propPatchMont
  set-plot-pen-color red
  plotxy ticks propPatchPlain
  
  set-current-plot "Absolute-landscape-fitness"
  plotxy ticks fitnessPatches
  
  set-current-plot "Quality"
  ;;pour réaliser un histogram on a besoin d'une liste
  set-plot-x-range 0 ( (max globalQuality) + 5)
  histogram globalQuality
  let maxbar modes globalQuality
  let maxrange length filter [ ? = item 0 maxbar ] globalQuality
  set-plot-y-range 0 max list 10 maxrange
  
  
  set-current-plot "LorenzCurve"
  ask wineMarkets [
      plot-pen-reset
      set-plot-pen-interval 100 / (count patches with [owner = -9999])
      plot 0
      foreach lorenz-points plot
    ]
  
end

to photo
  export-view (word "../img/exportview/espace_" ticks ".png")
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
609
430
32
32
5.985
1
10
1
1
1
0
0
0
1
-32
32
-32
32
0
0
1
ticks
30.0

BUTTON
15
25
88
58
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

SLIDER
15
100
187
133
nbCitys-i
nbCitys-i
1
100
10
1
1
NIL
HORIZONTAL

BUTTON
95
25
182
58
go step
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
15
135
187
168
nbPLots-i
nbPLots-i
1
20
7
1
1
NIL
HORIZONTAL

TEXTBOX
640
10
800
46
quantité de vin demandé par le marché étranger : 7000 default
9
0.0
1

TEXTBOX
820
10
970
31
Prix max que les étrangers sont capable de payer le vin
9
0.0
1

PLOT
605
600
770
740
demend
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
"default" 1.0 2 -16777216 true "" ""

BUTTON
120
65
183
98
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

PLOT
5
595
185
745
productionCost_WineMarkets
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
"default" 1.0 2 -16777216 true "" ""
"pen-1" 1.0 0 -2674135 true "" "plot priceMaxWineF"

PLOT
605
445
765
595
nbconection
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
"default" 1.0 0 -16777216 true "" ""
"pen-1" 1.0 0 -7500403 true "" "plot count seclinks"

PLOT
15
175
205
335
meanQualityVineyard
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
"default" 1.0 2 -16777216 true "" ""

SLIDER
640
35
812
68
demandF-i
demandF-i
0
30000
8000
100
1
NIL
HORIZONTAL

SLIDER
815
35
992
68
priceMaxWineF-i
priceMaxWineF-i
0
150
70
10
1
NIL
HORIZONTAL

BUTTON
20
65
112
98
go 500
if ticks <= 500 [go]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
630
230
870
430
Absolute-landscape-fitness
NIL
NIL
0.0
10.0
0.9
1.1
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""
"pen-1" 1.0 0 -5298144 true "" "plot 1"

BUTTON
10
485
132
518
NIL
moveMousse
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
15
525
165
561
Si l'on veux déplacer la vache pour tester le développement d'une zone en particulé
9
0.0
1

PLOT
630
75
885
225
global-cost
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
"default" 1.0 0 -16777216 true "" ""

TEXTBOX
150
565
410
586
evolution du prix pour chaque marché locale
9
0.0
1

PLOT
875
385
1115
585
mountainPlot
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
"default" 1.0 2 -16777216 true "" ""

SWITCH
1140
15
1282
48
marketColor
marketColor
1
1
-1000

BUTTON
1035
15
1137
48
NIL
seeQuality
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
175
440
385
560
Quality
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
"default" 1.0 1 -16777216 true "" ""

SWITCH
10
445
157
478
chooseMode
chooseMode
1
1
-1000

SWITCH
1135
55
1237
88
export
export
1
1
-1000

INPUTBOX
1135
155
1225
215
standarDevPop-i
2
1
0
Number

PLOT
905
75
1105
225
Difference mean Quality Mountain vs Plain
NIL
NIL
0.0
10.0
0.99
1.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -7500403 true "" "plotxy ticks (meanQualityMountain - meanQualityPlain)"
"zero" 1.0 0 -2674135 true "" "plot 0"

PLOT
875
230
1115
380
meanQualityPlots
NIL
NIL
0.0
10.0
0.0
0.2
true
true
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plotxy ticks meanQualityTotal"
"Mountain" 1.0 0 -7500403 true "" "plotxy ticks meanQualityMountain"
"Plaine" 1.0 0 -2674135 true "" "plotxy ticks meanQualityPlain"

MONITOR
1135
95
1227
140
NIL
meanQuality
17
1
11

INPUTBOX
1135
225
1210
285
downerQ-i
0.15
1
0
Number

PLOT
395
445
595
595
LorenzCurve
NIL
NIL
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""
"pen-1" 100.0 0 -7500403 true "plot 0\nplot 100" ""

PLOT
395
595
595
745
Gini-Index-Quality
Gini
Time
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (gini-index-reserve)"

INPUTBOX
1135
305
1225
365
coefUpQuality-i
0.01
1
0
Number

SLIDER
20
345
170
378
whatWord-i
whatWord-i
0
2
0
1
1
NIL
HORIZONTAL

TEXTBOX
20
390
170
426
WhatWord=0 isotrope\n                   1 mountain rigth\n                   2 mountain left
9
0.0
1

PLOT
190
595
390
745
Gini-index-Patch
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (gini-index-patch)"
"pen-1" 1.0 0 -2674135 true "" "plot 0"

TEXTBOX
770
455
865
505
nb de connections entre le marché centrale et les marchés périfériques
8
0.0
1

TEXTBOX
780
645
870
710
Graphe qui permet d'observer quand est ce que la demande extérieur est assouvie
8
0.0
1

SLIDER
1240
55
1273
201
logistique-i
logistique-i
0
1
0
1
1
NIL
VERTICAL

MONITOR
1135
380
1192
425
maxQ
max[quality] of patches
17
1
11

BUTTON
1135
435
1207
468
NIL
photo
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
## AUTEURS 
Etienne DELAY (GEOLAB - université de Limoges) etienne.delay[at]etu.unilim.fr
Marius CHEVALIER (GEOLAB - université de Limoges) marius.chev[at]gmail.com
Cyril PIOU (CIRAD, UMR CBGP, 34398 Montpellier cedex 5, France)  cyril.piou[at]cirad.fr

## CREDITS AND REFERENCES
Ce modèle vise à éclairer la lecture de l'article  DELAY,E., CHEVALIER,M., "Roger Dion toujours vivant",Cybergeo : European Journal of Geography [En ligne], Systèmes, Modélisation, Géostatistiques.
Il a été développé par les auteurs et distribué sous licence GNU-GPL V3 

## WHAT IS IT?

Ce modèle vise à explorer et revisiter les conditions d'émergence et de structuration historique des territoires de production des vins de qualité décrits dans l’œuvre de Roger Dion (Dion, R., 1952. Querelle des anciens et des modernes sur les facteurs de la qualité du vin. geo 61, 417–431.). 
On s'intéresse particulièrement à 2 hypothèses : 

* la qualité du vin est une fonction du travail de la parcelle (donc une fonction temporelle).
* l'émergence d'un territoire viticole est moins question des conditions initiales (caractéristiques pédoclimatiques), que de l'existence d'un marché viticole pour écouler les productions.

## HOW IT WORKS

Pour plus de détails, vous pourrez vous reporter à l'article de cybergéo. Ce qu'il faut néanmoins noter ici, c'est que l'augmentation et la diminution de la qualité suivent des fonctions linéaires. Lorsqu'une parcelle est mise en culture, sa qualité augmente linéairement en fonction du nombre d'itérations du modèle. Quand les simulations sont lancées en condition anisotropique, les formes blanches représentent des coteaux qui sont considérés comme plus qualitatifs. Les parcelles de ces zones commencent donc leur évolution avec une qualité initiale plus élevée.

## HOW TO USE IT

**Les variables globales** : 
_nbCitys_ : le nombre de marchés locaux généré à l'initialisation.
_nbPLots_ : le nombre de parcelles initiales pour chaque marché local à l'initialisation.
_demandF_ : la demande extérieure au territoire symbolisée par le lien entre la vache et le 1er marché.
_priceMaxWineF_ : le consentement à payer par les négociants étrangers. Au-delà de cette limite économique, la production n'est pas vendue.
_standarDevPop_ : la dérivée standard pour l'évolution de la population de chaque marché. Ces populations locales sont consommatrices des marchés qui ne sont pas rattachés au réseau.
_downerQ_ : le coefficient de diminution de la qualité à chaque itération
_coefUpQuality_ : le coefficient d'augmentation de la qualité à chaque itération

**Les graphiques** :
_meanQualityVineyard_ : permet d'observer l'évolution de la qualité moyenne des parcelles pour chaque marché.
_Quality_ : permet sous la forme d'un histogramme d'observer la fréquence de chaque classe de qualité de parcelles
_LorenzCurve_ : quand elle est linéaire et avec un coefficient directeur proche de 1, la courbe de Lorenze signifie que les valeurs étudiées sont réparties équitablement entre les agents. Dans ce cas-là, les agents sont les parcelles, et la valeur observée est la qualité.
_nbconection_ : permet de suivre dans le temps le nombre de connexions établies entre le marché principal et les marchés secondaires.
_productionCost_WineMarkets_ : permet de suivre les coûts de production pour chaque marché
_Gini-index-Patch_ : l'indice de Gini est une valeur comprise entre 0 et 1 permettant d'évaluer l'équitable répartition d'une valeur entre tous les agents. Ici, on s'intéresse au nombre de parcelles par agent. Si la valeur de Gini est proche de 1, le système n'est pas équitable.
_Gini-Index-Quality_ : s'intéresse à la répartition de la qualité entre les marchés
_demend_ : permet de visualiser si la demande extérieure est assouvie (valeurs proches de 0)
_global-cost_ : est la synthèse des valeurs que prennent les coûts de production à l'échelle individuelle des marchés (sommes des coûts individuels).
_Difference mean Quality Mountain vs Plain_ : reporte la différence de qualité entre la montagne et la plaine
_Absolute-landscape-fitness_ : représente les dynamiques du paysage viticole afin de visualiser s'il y a de grandes vagues d'extension du vignoble
_meanQualityPlots_ reporte la qualité moyenne des parcelles (patches)
_mountainPlot_ 

## THINGS TO NOTICE

* La géographie commerciale n’a pas seulement un impact à l'échelle macro, mais également au niveau micro. Si une parcelle est maintenue durablement en culture en raison de sa proximité (faibles coûts de transport) et de sa qualité progressive liée à l’ancienneté, elle peut acquérir une qualité supérieure quelles que sont les conditions pédoclimatiques initiales.
* Au-delà de leur trajectoire individuelle, on peut imaginer que ces parcelles ont une fonction méso-économique stimulant la vente des parcelles voisines par effet de proximité, et ainsi à l’échelle des marchés locaux reconstituer la hiérarchie dans les appellations. 
* Les dynamiques mises en évidence dans ce marché "globalisé" permettent aussi d'interpréter la création de zones de protection de la qualité comme une mesure de sauvegarde des compétences acquises par les exploitants. En effet, il n'est pas acceptable de voir sa production écartée de la commercialisation (réduction de la couronne d'influence autour des marchés) sous prétexte de l'arrivée de vins moins chers. La création des AOC, cette ”arme d’un style nouveau, qu’elle [au viticulteur de qualité] a donné depuis peu la législation sur les appellations d’origine des vins” (Dion R. 1952), peut donc être envisagée comme un moyen, pour ces viticulteurs, de distordre la concurrence à leur profit.

## THINGS TO TRY

* Nous vous encourageons à jouer avec le switch "marketColor" accompagné du bouton "seeQuality" afin de suivre l'évolution de la qualité sur les différentes parcelles tout au long de la simulation.
* Vous pouvez également modifier les inputs "downQ" et "CoefupQuality" pour influencer la vitesse de progression ou de régression de la qualité sur les parcelles.
 

## EXTENDING THE MODEL

* R.DION oppose dans son article les vins bourgeois des vins paysans. Nous nous sommes cantonnés à modéliser les dynamiques issues des vins bourgeois, mais il serait également intéressant d'explorer le comportement du territoire et de la qualité si une concurrence /cohabitation existait entre les vins bourgeois et paysans.
* On pourrait également envisager cette modélisation en considérant que la qualité évolue suivant une fonction logistique et explorer les implications de ce changement de paradigme.
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

boat
false
0
Polygon -7500403 true true 45 180 75 225 225 225 255 180
Line -7500403 true 150 180 150 45
Polygon -7500403 true true 150 45 60 150 150 150
Polygon -7500403 true true 150 45 225 150 150 150

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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
Polygon -7500403 true true 135 285 195 285 270 90 30 90 105 285
Polygon -7500403 true true 270 90 225 15 180 90
Polygon -7500403 true true 30 90 75 15 120 90
Circle -1 true false 183 138 24
Circle -1 true false 93 138 24

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
  <experiment name="exp" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>gini-index-reserve</metric>
    <metric>gini-index-patch</metric>
    <metric>meanQualityTotal</metric>
    <metric>meanQualityMountain</metric>
    <metric>meanQualityPlain</metric>
    <metric>DiffExtCentral</metric>
    <metric>nbcentralPlots</metric>
    <metric>meanPatchByNetwork</metric>
    <metric>sum [quality] of patches with [owner != -9999 and alti &lt; 0.1]</metric>
    <metric>sum [quality] of patches with [owner != -9999 and alti &gt;= 0.1]</metric>
    <enumeratedValueSet variable="export">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downerQ">
      <value value="0.15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="coefUpQuality" first="0.01" step="0.01" last="0.06"/>
    <enumeratedValueSet variable="chooseMode">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demandF">
      <value value="0"/>
      <value value="2000"/>
      <value value="7000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbfrich">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbPLots">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="standarDevPop">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbCitys">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="whatWord">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceMaxWineF">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="choceOnDist">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="marketColor">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="stability" repetitions="50000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>gini-index-reserve</metric>
    <metric>gini-index-patch</metric>
    <metric>meanQualityTotal</metric>
    <metric>meanQualityMountain</metric>
    <metric>meanQualityPlain</metric>
    <metric>DiffExtCentral</metric>
    <metric>nbcentralPlots</metric>
    <metric>meanPatchByNetwork</metric>
    <metric>sum [quality] of patches with [owner != -9999 and alti &lt; 0.1]</metric>
    <metric>sum [quality] of patches with [owner != -9999 and alti &gt;= 0.1]</metric>
    <enumeratedValueSet variable="export">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downerQ">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="coefUpQuality">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chooseMode">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demandF">
      <value value="7000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbPLots">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="standarDevPop">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nbCitys">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="whatWord">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceMaxWineF">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="marketColor">
      <value value="false"/>
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
1
@#$#@#$#@
