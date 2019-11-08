;UNIVERSIDAD NACIONAL AUTONOMA DE MEXICO.
;FACULTAD DE INGENIERIA.
;MATERIA: SISTEMAS EXPERTOS.
;PROF.: M.I. JAIME ALFONSO REYES CORTES.
;GRUPO: 01.
;PROGRAMA QUE EJEMPLIFICA EL USO DE HECHOS Y REGLAS
(clear)

(defrule mueve_directamente
    ?objetivo <- (mueve ?bloque1 encima_de ?bloque2)
    (block ?bloque1)
    (block ?bloque2)
    (encima_de NADA ?bloque1)
    ?pila1 <- (encima_de ?bloque1 ?bloque3)
    ?pila2 <- (encima_de NADA ?bloque2)
    =>
    (retract ?objetivo)
    (retract ?pila1)
    (retract ?pila2)
    (assert (encima_de ?bloque1 ?bloque2))
    (assert (encima_de NADA ?bloque3))
    (printout t ?bloque1 " se movio encima de " ?bloque2 "." crlf)
    )

(defrule mueve_al_piso
    ?objetivo <- (mueve ?bloque1 encima_de PISO)
    (block ?bloque1)
    (encima_de NADA ?bloque1)
    ?pila <- (encima_de ?bloque1 ?bloque2)
    =>
    (retract ?objetivo ?pila)
    (assert (encima_de ?bloque1 PISO))
    (assert (encima_de NADA ?bloque2))
    (printout t ?bloque1 " se mueve encima del piso " crlf)
    )

(defrule quita_bloque_de_arriba
    (mueve ?bloque1 encima_de ?)
    (block ?bloque1)
    (encima_de ?bloque2 ?bloque1)
    (block ?bloque2)
    =>
    (assert (mueve ?bloque2 encima_de PISO))
    )

(defrule quita_bloque_de_arriba_del_objetivo
    (mueve ? encima_de ?bloque1)
    (block ?bloque1)
    (encima_de ?bloque2 ?bloque1)
    (block ?bloque2)
    =>
    (assert (mueve ?bloque2 encima_de PISO))
    )

(deffacts estado_inicial
    (encima_de A B)
    (encima_de B C)
    (encima_de D E)
    (encima_de E F)
    (encima_de NADA A)
    (encima_de NADA D)
    (encima_de C PISO)
    (encima_de F PISO)
    (block A)
    (block B)
    (block C)
    (block D)
    (block E)
    (block F)
    (mueve F encima_de C)
    )
(reset)
(run)
