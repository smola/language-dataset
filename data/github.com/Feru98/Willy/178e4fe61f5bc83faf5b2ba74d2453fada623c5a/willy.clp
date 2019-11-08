;============ Start ============
(deffacts william
	(direccion nombre)	;Se usa para ver en qué posición se ha movido 
	(pos 0 0)			;Contiene la posición actual de Willy una vez se ha realizado el movimiento
	(visited 0 0 ok)	;Guardara las casillas seguras visitadas
	(counter 0)			;Guarda el numero de movimientos que Willy ha realizado
	(nostop)			;Sirve para parar todas las reglas cuando se llega a un numero concreto de movimientos
	(last nombre)		;Sirve actualizar la posición actual
)

;============ Stop ============

(defrule Stop                           ;La funcion hace que tras 700 movimientos willy se pare
	(declare (salience 20) )
	(counter ?c&:(eq ?c 700))
	?h <- (nostop)
	=>
	(retract ?h)
)

;============ Movement ============

(defrule moveWilly						;Mueve a willy de manera aleatoria
	(nostop)
	(directions $? ?direction $?)
	?m<-(counter ?c)
	?s<-(direccion ?x)
	?n<-(last ?x)
	=>
	(moveWilly ?direction)
	(retract ?s)
	(retract ?m)
	(retract ?n)
	(assert (counter (+ ?c 1)))
	(assert (direccion ?direction))
	(assert (last ?direction))
)

(defrule noNorth                        			;La funcion impide que willy vuelva a la posicion anterior en funcion de la direccion
	(declare (salience 5))							;en la que se mueve, la funcion tiene 4 versiones una para cada direccion
	(nostop)
	(direccion ?direction&:(eq ?direction south))
	?h <- (direccion ?y)
	(directions $? ?x&:(neq ?x north) $?)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last ?x))
)

(defrule noSouth
	(declare (salience 5) )
	(nostop)
	(direccion ?direction&:(eq ?direction north))
	?h<-(direccion ?y)
	(directions $? ?x&:(neq ?x south) $?)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(retract ?l)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last ?x))
)

(defrule noEast
	(declare (salience 5) )
	(nostop)
	(direccion ?direction&:(eq ?direction west))
	?h<-(direccion ?y)
	(directions $? ?x&:(neq ?x east) $?)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(retract ?l)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last ?x))
)

(defrule noWest
	(declare (salience 5) )
	(nostop)
	(direccion ?direction&:(eq ?direction east))
	?h<-(direccion ?y)
	(directions $? ?x&:(neq ?x west) $?)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(retract ?l)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last ?x))
)

;============ Not visited ============

(defrule NotVN
	(declare (salience 6) )									;La funcion hace que willy se mueva con preferencia por casillas no visitadas
	(nostop)												;tiene 4 versiones una para cada direccion
	?h<-(direccion ?y)										
	(pos ?a ?b)
	(directions $? ?x&:(eq ?x north) $?)
	(not(visited ?i&:(= (+ ?a 1) ?i) ?j&:(= ?b ?j) ok))
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(retract ?l)
	(assert (last ?x))
)

(defrule NotVS
	(declare (salience 6) )
	(nostop)
	?h<-(direccion ?y)
	(pos ?a ?b)
	(directions $? ?x&:(eq ?x south) $?)
	(not(visited ?i&:(= (- ?a 1) ?i) ?j&:(= ?b ?j) ok))
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(retract ?l)
	(assert (last ?x))
)

(defrule NotVE
	(declare (salience 6) )
	(nostop)
	?h<-(direccion ?y)
	(pos ?a ?b)
	(directions $? ?x&:(eq ?x east) $?)
	(not (visited ?i&:(= ?a ?i) ?j&:(= (+ ?b 1) ?j) ok))
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(retract ?l)
	(assert (last ?x))
)

(defrule NotVW
	(declare (salience 6) )
	(nostop)
	?h<-(direccion ?y)
	(pos ?a ?b)
	(directions $? ?x&:(eq ?x west) $?)
	(not(visited ?i&:(= ?a ?i) ?j&:(= (- ?b 1) ?j) ok))
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly ?x)
	(retract ?h)
	(assert(direccion ?x))
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(retract ?l)
	(assert (last ?x))
)




;============ Position ============

(defrule checknorth
	(declare (salience 15) )							;El hecho last actualiza el hecho pos que es la posicion
	(last ?direction&:(eq ?direction north))			;en la que willy se encuentra
	?n <- (last ?direction)
	?s <- (pos ?x ?y)
	?v <- (visited $?)
	=>
	(retract ?n)
	(retract ?s)
	(assert (pos (+ ?x 1) ?y))
	(assert(last ?x))
	(assert(visited (+ ?x 1) ?y ok))
)

(defrule checksouth
	(declare (salience 15) )
	(last ?direction&:(eq ?direction south))
	?n <- (last ?direction)
	?s <- (pos ?x ?y)
	?v <- (visited $?)
	=>
	(retract ?n)
	(retract ?s)
	(assert (pos (- ?x 1) ?y))
	(assert(last ?x))
	(assert(visited (- ?x 1) ?y ok))
)

(defrule checkeast
	(declare (salience 15) )
	(last ?direction&:(eq ?direction east))
	?n <- (last ?direction)
	?s <- (pos ?x ?y)
	?v <- (visited $?)
	=>
	(retract ?n)
	(retract ?s)
	(assert (pos ?x (+ ?y 1)))
	(assert(last ?x))
	(assert(visited  ?x (+ ?y 1)  ok))
)

(defrule checkwest
	(declare (salience 15) )
	(last ?direction&:(eq ?direction west))
	?n <- (last ?direction)
	?s <- (pos ?x ?y)
	?v <- (visited $?)
	=>
	(retract ?n)
	(retract ?s)
	(assert (pos ?x (- ?y 1)))
	(assert(last ?x))
	(assert(visited ?x (- ?y 1) ok))
)

(defrule checknoise
	(declare (salience 14))						;La funcion guarda las posiciones en el hecho noisepos en las que se ha detectado un noise
	(or (percepts Noise)
	(percepts Noise Pull) 
	(percepts Pull Noise))
	(pos ?z ?k)
	=>
	(assert(noisepos ?z ?k))
)

;============ Fire ============ 

(defrule fireWillywest
   (declare (salience 17) )						;La funcion hace que willy dispare cuando se encuentra un ruido y tenga en la lista de hechos el noisepos
   (hasLaser)									;que se encuentre en el mismo eje que el punto en el que esta y en funcion del otro valor de noisepos 
   (or (percepts Noise)							;sabe en que direccion debe disparar
   (percepts Noise Pull)
   (percepts Pull Noise))
   (pos ?a ?b)
   (noisepos ?a ?k&:(> ?b ?k))
   (not(test (= ?b ?k)))
   =>
   (fireLaser west)
)

(defrule fireWilleast
   (declare (salience 17) )						
   (hasLaser)									
   (or (percepts Noise)							
   (percepts Noise Pull)
   (percepts Pull Noise))
   (pos ?a ?b)
   (noisepos ?a ?k&:(< ?b ?k))
   (not(test (= ?b ?k)))
   =>
   (fireLaser east)
)

(defrule fireWillsouth
   (declare (salience 17) )						
  (hasLaser)									
   (or (percepts Noise)							
   (percepts Noise Pull)
   (percepts Pull Noise))
   (pos ?a ?b)
   (noisepos ?i&:(> ?a ?i) ?b)
   (not(test (= ?a ?i)))
   =>
   (fireLaser south)
)

(defrule fireWillnorth
   (declare (salience 17) )						
   (hasLaser)									
   (or (percepts Noise)							
   (percepts Noise Pull)
   (percepts Pull Noise))
   (pos ?a ?b)
   (noisepos ?i&:(< ?a ?i) ?b)
   (not(test (= ?a ?i)))
   =>
   (fireLaser north)
)




;============ Danger ============

(defrule dangerS									
	(declare (salience 10) )							;La funcion danger hace que willy vuelva a su posicion anterior si detecta un peligro
	(nostop)
	(or(percepts Pull)
	(percepts Noise)
	(percepts Noise Pull) 
	(percepts Pull Noise))
	(direccion ?direction&:(eq ?direction south))
	?h<-(direccion ?y)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly north)
	(assert(direccion north))
	(retract ?h)
	(retract ?l)
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last north))
)

(defrule dangerN
	(declare (salience 10) )
	(nostop)
	(or(percepts Pull)
	(percepts Noise)
	(percepts Noise Pull) 
	(percepts Pull Noise))
	(direccion ?direction&:(eq ?direction north))
	?h<-(direccion ?y)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly south)
	(assert(direccion south))
	(retract ?h)
	(retract ?l)
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last south))
)

(defrule dangerE
	(declare (salience 10) )
	(nostop)
	(or(percepts Pull)
	(percepts Noise)
	(percepts Noise Pull) 
	(percepts Pull Noise))
	(direccion ?direction&:(eq ?direction east))
	?h<-(direccion ?y)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly west)
	(assert(direccion west))
	(retract ?h)
	(retract ?l)
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last west))
)

(defrule dangerW
	(declare (salience 10) )
	(nostop)
	(or(percepts Pull)
	(percepts Noise)
	(percepts Noise Pull) 
	(percepts Pull Noise))
	(direccion ?direction&:(eq ?direction west))
	?h<-(direccion ?y)
	?m <- (counter ?c)
	?l<-(last ?z)
	=>
	(moveWilly east)
	(assert(direccion east))
	(retract ?h)
	(retract ?l)
	(retract ?m )
	(assert (counter (+ ?c 1)))
	(assert (last east))
)