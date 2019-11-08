;###############################################################################
;#############################  MAIN MODULE  ###################################
;###############################################################################
; the main module is responsable for launching the meny untill the user exits
; the programm.
(defmodule MAIN (export ?ALL))


(deffacts MAIN::initialFacts
    (module menu)
)

; launch the menu again
(defrule MAIN::PassToModuleMenu
    ?f1<-(module menu)
    =>
    (retract ?f1)
    (printout t "focusing menu" crlf)
    (assert (menu mainMenu))
    (focus MENU)
    (assert (module menu))
)



;###############################################################################
;###########################  MINERALS MODULE  #################################
;###############################################################################
; the minerals module is respondable for storagin the knowledge of the minerals
; for the expert to search betwen them.

(defmodule MINERALS (export ?ALL))

; template of the mineral
(deftemplate MINERALS::mineral
    (slot name (type SYMBOL) (default notDefined))
    (multislot color (type SYMBOL) (default notDefined))
    (slot hardness (type NUMBER) (range -1 10) (default -1))
    (slot density (type NUMBER) (default -1))
    (multislot diaphaneity (allowed-symbols transparent translucent opaque notDefined) (default notDefined))
    (slot streak (type SYMBOL) (default notDefined))
)

; the minerals
(deffacts MINERALS::minerals
    (mineral (name emerald) (color green) (hardness 7.5) (density 2.7) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name diamond) (color yellow brown green blue white colorless) (hardness 10) (density 3.5) (diaphaneity transparent translucent) (streak colorless))
    (mineral (name quartz) (color white pink black colorless red green blue violet) (hardness 7) (density 2.65) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name tourmaline) (color red pink yellow brown green blue white black colorless) (hardness 7) (density 3.1) (diaphaneity translucent opaque) (streak white))
    (mineral (name topaz) (color red pink yellow brown blue violet white colorless) (hardness 8) (density 3.5) (diaphaneity transparent) (streak white))
    (mineral (name zircon) (color yellow brown green violet white colorless) (hardness 7) (density 4.7) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name jade) (color red pink yellow brown green blue violet white black colorless) (hardness 7) (density 3.3) (diaphaneity translucent opaque) (streak white))
    (mineral (name opal) (color red pink yellow brown white black colorless) (hardness 6) (density 2.5) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name turquoise) (color blue) (hardness 5.5) (density 2.5) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name amethyst) (color violet) (hardness 7) (density 2.66) (diaphaneity transparent translucent) (streak white))
    (mineral (name sapphire) (color blue) (hardness 9) (density 3.98) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name ruby) (color red pink) (hardness 9) (density 4.02) (diaphaneity transparent translucent) (streak white))
    (mineral (name apatite) (color white yellow green red blue) (hardness 5) (density 3.19) (diaphaneity transparent translucent) (streak white))
    (mineral (name lazulite) (color blue green) (hardness 5.75) (density 3.1) (diaphaneity translucent opaque) (streak white))
    (mineral (name azurite) (color blue) (hardness 3.75) (density 3.83) (diaphaneity transparent translucent) (streak blue))
    (mineral (name malachite) (color green) (hardness 7.5) (density 3.8) (diaphaneity translucent opaque) (streak green))
    (mineral (name pyrite) (color yellow brown) (hardness 6.25) (density 4.9) (diaphaneity opaque) (streak black))
    (mineral (name magnetite) (color black) (hardness 6) (density 5.15) (diaphaneity opaque) (streak black))
    (mineral (name spinel) (color red pink blue violet green brown black colourless) (hardness 7.8) (density 3.64) (diaphaneity transparent translucent opaque) (streak white))
    (mineral (name alexandrite) (color blue red green yellow pink violet) (hardness 8.5) (density 3.72) (diaphaneity transparent translucent) (streak white))
    (mineral (name jasper) (color red brown yellow green) (hardness 7) (density 2.65) (diaphaneity opaque) (streak white))
    (mineral (name agate) (color white blue red green yellow brown pink violet black) (hardness 6.75) (density 2.62) (diaphaneity translucent) (streak white))
    (mineral (name kryptonite) (color green) (hardness 10) (density 2.41) (diaphaneity transparent) (streak green))
    (mineral (name lapislazuli) (color blue violet) (hardness 5.25) (density 2.4) (diaphaneity translucent opaque) (streak blue))
    (mineral (name peridot) (color green yellow) (hardness 6.75) (density 3.34) (diaphaneity transparent translucent) (streak white))
    (mineral (name garnet) (color red green yellow black brown) (hardness 7) (density 3.7) (diaphaneity transparent translucent) (streak white))
)







;###############################################################################
;#############################  MENU MODULE  ###################################
;###############################################################################
; the menu module is responsable for the user I/O

(defmodule MENU (import MAIN ?ALL) (import MINERALS ?ALL) (export ?ALL))

; the response of the user when is addign a restriction will be stored here
(defglobal ?*response* = nil)

; adds the target mineral, where all the restrictions will be stored
(deffacts MINERALS::addTarget
    (mineral (name target)) 
)

; this function prints a menu acording to his parameter:
; 1 is for the main menu and 2 is for the restriction menu, 0 is exit, so it is
; not contemplated
(deffunction MENU::printMenu (?option)
    (printout t crlf)

    (if (eq ?option 0)  then
        (printout t "   1. Add restriction" crlf)
        (printout t "   2. Filter minerals" crlf)
        (printout t "   0. Exit" crlf)
    )
    (if (eq ?option 1) then
        (printout t "   1. Specify color" crlf)
        (printout t "   2. Specify hardness" crlf)
        (printout t "   3. Specify density" crlf)
        (printout t "   4. Specify diaphaneity" crlf)
        (printout t "   5. Specify streak" crlf)
    )
    (printout t crlf)
    (printout t "your option: ")
)

; this function will print a menu for adding a restriction to the given 
; parameter
(deffunction MENU::printMenuRestriction (?option)
    (printout t crlf)

    (if (eq ?option color) then
        (printout t "Specify the color of the mineral: ")
    )
    (if (eq ?option hardness) then
        (printout t "Specify the hardness of the mineral [0-10]: ")
    )
    (if (eq ?option density) then
        (printout t "Specify the density of the mineral: ")
    )
    (if (eq ?option diaphaneity) then
        (printout t "Specify the diaphaneity of the mineral" crlf)
        (printout t crlf)
        (printout t "   1. transparent" crlf)
        (printout t "   2. translucent" crlf)
        (printout t "   3. opaque" crlf)
        (printout t crlf)
        (printout t "your option: ")
    )
    (if (eq ?option streak) then
        (printout t "Specify the color of the streak: ")
    )
)

;###############################################################################

; this function checks if the parameter is a valid color
(deffunction isColor(?color)
    (if (eq ?color black) then (return true))
    (if (eq ?color blue) then (return true))
    (if (eq ?color brown) then (return true))
    (if (eq ?color colorless) then (return true))
    (if (eq ?color green) then (return true))
    (if (eq ?color pink) then (return true))
    (if (eq ?color red) then (return true))
    (if (eq ?color violet) then (return true))
    (if (eq ?color white) then (return true))
    (if (eq ?color yellow) then (return true))
    (return false)
)

;###############################################################################

; validates and return a valid color from the user
(deffunction MENU::getColorFromUser ()
    (bind ?*response* (read))
    (if (neq (isColor ?*response*) true) then
        (printout t "invalid color, please, type a correct color: ")
        (return (getColorFromUser))
    )
    (return  ?*response*)
)

; validates and return a valid density from the user
(deffunction MENU::getDensityFromUser ()
    (bind ?*response* (read))
    (if (< 0 ?*response*) then
        (return  ?*response*)
    )
    (printout t "invalid density, please, type a correct density: ")
    (return (getDensityFromUser))
)

; validates and return a valid hardness from the user
(deffunction MENU::getHardnessFromUser ()
    (bind ?*response* (read))
    (if (and
            (<= 0 ?*response*)
            (>= 10 ?*response*)
        ) then
        (return  ?*response*)
    )
    (printout t "invalid hardness, please, type a correct hardness [0-10]: ")
    (return (getHardnessFromUser))
)

; validates and return a valid diaphaneity from the user
(deffunction MENU::getDiaphaneityFromUser ()
    (bind ?*response* (read))
    (if (and
            (<= 1 ?*response*)
            (>= 3 ?*response*)
        ) then
        (if (eq ?*response* 1) then (return transparent))
        (if (eq ?*response* 2) then (return translucent))
        (if (eq ?*response* 3) then (return opaque))
    )
    (printout t "invalid option, please, type a correct diaphaneity: ")
    (return (getDiaphaneityFromUser))
)

; validates and return a valid streak from the user
(deffunction MENU::getStreakFromUser ()
    (bind ?*response* (read))
    (if (neq (isColor ?*response*) true) then
        (printout t "invalid color, please, type a correct streak color: ")
        (return (getStreakFromUser))
    )
    (return  ?*response*)
)


;###############################################################################

; prints the main menu and asserts the chosen option as a fact
(defrule MENU::mainMenu
    ?f1<-(menu mainMenu)
    =>
    (retract ?f1)
    (printMenu 0)

    (assert
        (menu option (read))
    )
)

; if the exit option is selected, halt the program
(defrule MENU::exit
    ?f1<-(menu option 0)
    =>
    (retract ?f1)
    (exit)
)

; prints the menu for adding a restriction and asserts a fact with the chosen
; restriction
(defrule MENU::menuAddRestriction
    ?f1<-(menu option 1)
    =>
    (retract ?f1)
    (printMenu 1)
    (assert
        (menu AddRestriction (read))
    )
)

; focus the expert to make an diagnostic with the current restrictions
(defrule MENU::menuExpert
    ?f1<-(menu option 2)
    =>
    (retract ?f1)
    (printout t "focusing expert" crlf)
    (focus EXPERT)
)


;###############################################################################

; add a restriction to the color of the mineral
(defrule MENU::AddRestrictionColor
    ?f1<-(menu AddRestriction 1)
    ?target<-(mineral (name target))
    =>
    (retract ?f1)
    ; prints the menu for adding a restriction to the color
    (printMenuRestriction color)
    ; modify the target with the color chosen by the user
    (modify ?target
        (color (getColorFromUser))
    )
)

; add a restriction to the hardness of the mineral
(defrule MENU::AddRestrictionHardness
    ?f1<-(menu AddRestriction 2)
    ?target<-(mineral (name target))
    =>
    (retract ?f1)
    ; prints the menu for adding a restriction to the hardness
    (printMenuRestriction hardness)
    ; modify the target with the hardness chosen by the user
    (modify ?target
        (hardness (getHardnessFromUser))
    )
)

; add a restriction to the density of the mineral
(defrule MENU::AddRestrictionDensity
    ?f1<-(menu AddRestriction 3)
    ?target<-(mineral (name target))
    =>
    (retract ?f1)
    ; prints the menu for adding a restriction to the density
    (printMenuRestriction density)
    ; modify the target with the density chosen by the user
    (modify ?target
        (density (getDensityFromUser))
    )
)


; add a restriction to the diaphaneity of the mineral
(defrule MENU::AddRestrictionDiaphaneity
    ?f1<-(menu AddRestriction 4)
    ?target<-(mineral (name target))
    =>
    (retract ?f1)
    ; prints the menu for adding a restriction to the diaphaneity
    (printMenuRestriction diaphaneity)
    ; modify the target with the diaphaneity chosen by the user
    (modify ?target
        (diaphaneity (getDiaphaneityFromUser))
    )
)

; add a restriction to the streak of the mineral
(defrule MENU::AddRestrictionStreak
    ?f1<-(menu AddRestriction 5)
    ?target<-(mineral (name target))
    =>
    (retract ?f1)
    ; prints the menu for adding a restriction to the streak
    (printMenuRestriction streak)
    ; modify the target with the streak chosen by the user
    (modify ?target
        (streak (getStreakFromUser))
    )
)



;###############################################################################
;############################  EXPERT MODULE  ##################################
;###############################################################################
(defmodule EXPERT (import MINERALS ?ALL) (export ?ALL))

; error for comparing the hardness and density
(defglobal ?*hardnessError* = 0.3)
(defglobal ?*densityError* = 0.1)

;###############################################################################

; recursive funciton that checks if an element is in a vector
(deffunction EXPERT::isInArray (?element $?vector)
    (if (eq 0 (length$ $?vector)) then
        (return false)
    )
    (if (eq ?element (nth$ 1 $?vector)) then
        (return true)
    )
    (return (isInArray ?element (rest$ $?vector)))
)

;###############################################################################

; checks if the target color is tha same as the color of the mineral
(deffunction EXPERT::isSameColor (?targetColor $?color)
    ; target hardness has its default value, not filtering by this field
    (if (eq ?targetColor notDefined) then (return true))
    (return (isInArray ?targetColor $?color))
)

; checks if the target hardness is tha same as the hardness of the mineral
(deffunction EXPERT::isSameHardness (?targetHardness ?hardness)
    ; target hardness has its default value, not filtering by this field
    (if (eq ?targetHardness -1) then (return true))
    
    (if (> ?*hardnessError* (abs (- ?targetHardness ?hardness)))
        then (return true)
    )
    (return false)
)

; checks if the target density is tha same as the density of the mineral
(deffunction EXPERT::isSameDensity (?targetDensity ?density)
    ; target density has its default value, not filtering by this field
    (if (eq ?targetDensity -1) then (return true))
    (if (> ?*densityError* (abs (- ?targetDensity ?density)))
        then (return true)
    )
    (return false)
)

; checks if the target diaphaneity is tha same as the diaphaneity of the mineral
(deffunction EXPERT::isSameDiaphaneity (?targetDiaphaneity $?diaphaneity)
    ; target diaphaneity has its default value, not filtering by this field
    (if (eq ?targetDiaphaneity notDefined) then (return true))
    (return (isInArray ?targetDiaphaneity $?diaphaneity))
)

; checks if the target streak is tha same as the streak of the mineral
(deffunction EXPERT::isSameStreak (?targetStreak $?streak)
    ; target streak has its default value, not filtering by this field
    (if (eq ?targetStreak notDefined) then (return true))
    (return (isInArray ?targetStreak $?streak))
)

;###############################################################################

; show what the expert is filtering by
(defrule EXPERT::showFilteringBy
    ?target<-(mineral
                (name target)
                (color ?targetColor)
                (hardness ?targetHardness)
                (density ?targetDensity)
                (diaphaneity ?targetDiaphaneity)
                (streak ?targetStreak)
             )
    =>
    (printout t "" crlf)
    (printout t "filtering by:" crlf)
    (if (neq ?targetColor notDefined) then
        (printout t "   color: "?targetColor crlf)
    )
    (if (neq ?targetHardness -1) then
        (printout t "   hardness: "?targetHardness crlf)
    )
    (if (neq ?targetDensity -1) then
        (printout t "   density: "?targetDensity crlf)
    )
    (if (neq ?targetDiaphaneity notDefined) then
        (printout t "   diaphaneity: "?targetDiaphaneity crlf)
    )
    (if (neq ?targetStreak notDefined) then
        (printout t "   streak: "?targetStreak crlf)
    )
    (if (and
            (and
                (eq ?targetColor notDefined)
                (eq ?targetHardness -1)
            )
            (and
                (eq ?targetDensity -1)
                (and
                    (eq ?targetDiaphaneity notDefined)
                    (eq ?targetStreak notDefined)
                )
            )
        )
        then
        (printout t "   nothing."crlf)
    )
    
    (printout t "" crlf)
)



; test if a mineral matches the target
(defrule EXPERT::testMineral
    ?target<-(mineral
                (name target)
                (color ?targetColor)
                (hardness ?targetHardness)
                (density ?targetDensity)
                (diaphaneity ?targetDiaphaneity)
                (streak ?targetStreak)
             )
    ?mineral<-(mineral
                (name ?name)
                (color $?color)
                (hardness ?hardness)
                (density ?density)
                (diaphaneity $?diaphaneity)
                (streak ?streak)
              )
    (test (neq ?name target))
    (test (eq true (isSameColor ?targetColor $?color)))
    (test (eq true (isSameHardness ?targetHardness ?hardness)))
    (test (eq true (isSameDensity ?targetDensity ?density)))
    (test (eq true (isSameDiaphaneity ?targetDiaphaneity $?diaphaneity)))
    (test (eq true (isSameStreak ?targetStreak ?streak)))
    =>
    (printout t "match with "?name crlf)
)
