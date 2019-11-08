;;; Hechos estaticos;
(deffacts Habitaciones
  (Habitacion Recepcion)    ;;;;  Receptión es una habitación
  (Habitacion Pasillo)
  (Habitacion Oficina1)
  (Habitacion Oficina2)
  (Habitacion Oficina3)
  (Habitacion Oficina4)
  (Habitacion Oficina5)
  (Habitacion OficinaDoble)
  (Habitacion Gerencia)
  (Habitacion Papeleria)
  (Habitacion Aseos)
  (Habitacion AseoHombres)
  (Habitacion AseoMujeres)
  )
  (deffacts Puertas
  (Puerta Recepcion Pasillo)    ;;;; Hay una puerta que comunica Recepción con Pasillo
  (Puerta Pasillo Oficina1)
  (Puerta Pasillo Oficina2)
  (Puerta Pasillo Oficina3)
  (Puerta Pasillo Oficina4)
  (Puerta Pasillo Oficina5)
  (Puerta Pasillo Gerencia)
  (Puerta Pasillo OficinaDoble)
  (Puerta Pasillo Papeleria)
  )
  (deffacts Luces
    (Luz Recepcion ON)    ;;;;  Receptión es una habitación
    (Luz Pasillo ON)
    (Luz Oficina1 OFF)
    (Luz Oficina2 OFF)
    (Luz Oficina3 OFF)
    (Luz Oficina4 OFF)
    (Luz Oficina5 OFF)
    (Luz OficinaDoble OFF)
    (Luz Gerencia OFF)
    (Luz Papeleria OFF)
    (Luz Aseos OFF)
    (Luz AseoHombres OFF)
    (Luz AseoMujeres OFF)
  )

  (deffacts Empleados
  (Empleado G1 Oficina1)          ;;;;; El empleado G1 atiende en la Oficina 1
  (Empleado G2 Oficina2)
  (Empleado G3 Oficina3)
  (Empleado G4 Oficina4)
  (Empleado G5 Oficina5)
  (Empleado E1 OficinaDoble-1)
  (Empleado E2 OficinaDoble-2)
  (Empleado Recepcionista Recepcion)
  (Empleado Director Gerencia)
  )
   (deffacts Codificacion
   (Code TG "Tramites Generales")
   (Code TE "Tramites Especiales")
   )
  (deffacts Tareas
  (Tarea G1 TG)                   ;;;;; El empleado G1 atiende Trámites Generales
  (Tarea G2 TG)
  (Tarea G3 TG)
  (Tarea G4 TG)
  (Tarea G5 TG)
  (Tarea E1 TE)                   ;;;;; El empleado E1 atiende Trámites Especiales
  (Tarea E2 TE)
  )
  (deffacts Inicializacion
  (Personas 0)                    ;;; Inicialmente hay 0 personas en las oficinas
  (Usuarios TG 0)                 ;;; Inicialmente hay 0 Usuarios de trámites generales
  (UltimoUsuarioAtendido TG 0)    ;;; Inicialmente se han atendido 0 usuarios de tramites generales
  (Usuarios TE 0)
  (UltimoUsuarioAtendido TE 0)
  (Empleados 0)                   ;;; Inicialmente hay 0 empleados en las oficinas
  (Ejecutar)
  (EmpleadosAtendiendo TG 0)  ;;; Empleados ateniendo (asignados)
  (EmpleadosAtendiendo TE 0)

  (Empleados TG 0)    ;;; Empleados en la oficina
  (Empleados TE 0)

  ; Estado empleados
  (Estado G1 NoHaLlegado)
  (Estado G2 NoHaLlegado)
  (Estado G3 NoHaLlegado)
  (Estado G4 NoHaLlegado)
  (Estado G5 NoHaLlegado)
  (Estado E1 NoHaLlegado)
  (Estado E2 NoHaLlegado)

  ; Tramites gestionados y tiempo empleado
  (TramitesGestionados G1 0 0) ; total_tramites, total_tiempo
  (TramitesGestionados G2 0 0)
  (TramitesGestionados G3 0 0)
  (TramitesGestionados G4 0 0)
  (TramitesGestionados G5 0 0)
  (TramitesGestionados E1 0 0)
  (TramitesGestionados E2 0 0)

  (TotalEsperas TG 0 0) ; esperas tramite total tiempo_total
  (TotalEsperas TE 0 0)

  (TotalTramites TG 0 0)
  (TotalTramites TE 0 0)

  ; Sirven para calcular la desviacion tipica
  (SumaParcialDesviacionTr 0)
  (SumaParcialDesviacionEs 0)

  ; Hora que se activó por última vez cada sensor de presencia
  (TiempoUltimaVezPasillo 0)
  (TiempoUltimaVez AseoHombres 0)
  (TiempoUltimaVez AseoMujeres 0)

  ; Total de personas en cada Oficina (estimado)
  (TotalPersonas Oficina1 0)
  (TotalPersonas Oficina2 0)
  (TotalPersonas Oficina3 0)
  (TotalPersonas Oficina4 0)
  (TotalPersonas Oficina5 0)
  (TotalPersonas OficinaDoble 0)
  (TotalPersonas Gerencia 0)
  (TotalPersonas Papeleria 0)

  ; Habitaciones que no importa si se quedan vacías
  (NoImportaSiVacia Gerencia)
  (NoImportaSiVacia Papeleria)

  ; Habitaciones individuales
  (Individual Oficina1)
  (Individual Oficina2)
  (Individual Oficina3)
  (Individual Oficina4)
  (Individual Oficina5)
  )

  (defrule cargarconstantes
  (declare (salience 10000))
  =>
  (load-facts Constantes.txt)
  )

  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;; PASO1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;respuestas ante los hechos (Solicitud ?tipotramite) y (Disponible ?empl);;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;; 1A ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defrule EncolarUsuario
  ?g <- (Solicitud ?tipotramite)
  ?f <- (Usuarios ?tipotramite ?n)
  =>
  (bind ?segundos_llegada (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  (assert (Usuario ?tipotramite (+ ?n 1))
          (Usuarios ?tipotramite (+ ?n 1))
          (TiempoLlegadaUsuario ?tipotramite (+ ?n 1) ?segundos_llegada)
  )
  (printout t "Su turno es " ?tipotramite " " (+ ?n 1)  crlf)
  (retract ?f ?g)
  )

  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;; 1B ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defrule AsignarEmpleado
  ?g <- (Disponible ?empl)
  (Tarea ?empl ?tipotramite)
  (Empleado ?empl ?ofic)
  ?f <- (UltimoUsuarioAtendido ?tipotramite ?atendidos)
  ?h <- (EmpleadosAtendiendo ?tipotramite ?total_empleados)
  ;?z <- (Estado ?empl ?x)
  (Usuarios ?tipotramite ?total)
  (test (< ?atendidos ?total))
  =>
  (bind ?segundos_actuales (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  (bind ?a (+ ?atendidos 1))
  (assert (Asignado ?empl ?tipotramite ?a)
          (UltimoUsuarioAtendido ?tipotramite ?a)
          (EmpleadosAtendiendo ?tipotramite (+ ?total_empleados 1))
          (RegistrarEsperaUsuario ?tipotramite ?a ?segundos_actuales)
          (TiempoInicioTramiteUsuario ?empl ?tipotramite ?a ?segundos_actuales)
          (ActualizarEstado ?empl Atendiendo)
  )
  (printout t "Usuario " ?tipotramite ?a ", por favor pase a " ?ofic crlf)
  (retract ?f ?g ?h)
  )

  (defrule ActualizarEstado
    (declare (salience 1000))
    ?a <- (ActualizarEstado ?emp ?estado_nuevo)
    ?b <- (Estado ?emp ?estado)
    =>
    (retract ?a ?b)
    (assert
      (Estado ?emp ?estado_nuevo)
    )
  )
  ; Se registra cuánto ha esperado
  (defrule RegistrarEsperaUsuario
    (declare (salience 1000))
    ?r <- (RegistrarEsperaUsuario ?tipotramite ?usr ?hora_final)
    ?g <- (TiempoLlegadaUsuario ?tipotramite ?usr ?hora_inicial)
    =>
    (assert (EsperaUsuario ?tipotramite (- ?hora_final ?hora_inicial)) )
    (retract ?g ?r)
  )

  (defrule RegistrarCaso
  (declare (salience 10))
  (Disponible ?empl)
  ;?z <- (Estado ?empl ?x)
  ?f <- (Asignado ?empl ?tipotramite ?n)
  ?h <- (EmpleadosAtendiendo ?tipotramite ?total_empleados)
  =>
  (bind ?segundos_actuales (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  (assert (Tramitado ?empl ?tipotramite ?n)
          (EmpleadosAtendiendo ?tipotramite (- ?total_empleados 1))
          (RegistrarTramiteUsuario ?tipotramite ?n ?empl ?segundos_actuales)
          (ActualizarEstado ?empl Disponible)
  )
  (retract ?f ?h)
  )

  ; Se registra cuánto ha durado el trámite
  (defrule RegistrarTramiteUsuario
    (declare (salience 1000))
    ?r <- (RegistrarTramiteUsuario ?tipotramite ?usr ?empl ?hora_final)
    ?g <- (TiempoInicioTramiteUsuario ?empl ?tipotramite ?usr ?hora_inicial)
    ?p <- (TramitesGestionados ?empl ?total_tramites ?total_tiempo)
    =>
    (bind ?tiempo (- ?hora_final ?hora_inicial))
    (assert
      (TramiteUsuario ?tipotramite ?empl ?tiempo)
      (TramitesGestionados ?empl (+ ?total_tramites 1) (+ ?total_tiempo ?tiempo))
    )
    (retract ?g ?r ?p)
  )

  ;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;; 1C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defrule NoposibleEncolarUsuario
  (declare (salience 20))
  ?g <- (Solicitud ?tipotramite)
  (Usuarios ?tipotramite ?n)
  (UltimoUsuarioAtendido ?tipotramite ?atendidos)
  (TiempoMedioGestion ?tipotramite ?m)
  (FinalJornada ?h)
  (test (> (* (- ?n ?atendidos) ?m) (mrest ?h)))
  (Code  ?tipotramite ?texto)
  =>
  (printout t "Lo siento pero por hoy no podremos atender mas " ?texto crlf)
  (bind ?a (- ?n ?atendidos))
  (printout t "Hay ya  " ?a " personas esperando y se cierra a las " ?h "h. No nos dara tiempo a atenderle." crlf)
  (retract ?g)
  )

  ;===============================
  ; Control de calidad de servicio
  ;===============================

  ; Informa si hay menos de N empleados atendiendo TG
  (defrule InformarMenosDeNEmpleadosTG
    (declare (salience 50))
    (EmpleadosAtendiendo TG ?total_empleados)
    (MinimoEmpleadosActivos TG ?n)
    (test(< ?total_empleados ?n))
    =>
    (printout t "Hay menos de " ?n " empleados atendiendo TG" crlf)
  )

  ; Informa si hay 0 empleados atendiendo TE
  (defrule InformarNingunEmpleadoTE
    (declare (salience 50))
    (EmpleadosAtendiendo TE ?total_empleados)
    (test(= ?total_empleados 0))
    =>
    (printout t "No hay ningún empleado atendiendo TE" crlf)
  )

  ; Comprueba continuamente si una espera supera el tiempo máximo
  (defrule ComprobarMaximoTiempoEspera
    (declare(salience 50))
    (TiempoLlegadaUsuario ?tipotramite ?usr ?segundos_llegada)
    (MaximoEsperaParaSerAtendido ?tipotramite ?max_espera)
    (not (AvisoUsuarioEsperando ?tipotramite ?usr))

    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)

    (test(> (- (+ (* ?h 3600) (* ?m 60) ?s) ?segundos_llegada) (* 60 ?max_espera)))
    =>
    (assert (AvisoUsuarioEsperando ?tipotramite ?usr))
  )

  (defrule AvisarTiempoEsperaExcedido
    (declare(salience 1000))
    (AvisoUsuarioEsperando ?tipotramite ?usr)
    =>
    (printout t "El usuario " ?tipotramite "-" ?usr " ha superado tiempo de espera" crlf)
  )

  ; Comprueba continuamente si un trámite supera el tiempo máximo
  (defrule ComprobarMaximoTiempoTramite
    (declare (salience 50))
    (TiempoInicioTramiteUsuario ?empl ?tipotramite ?usr ?segundos_comienzo)
    (MaximoTiempoGestion ?tipotramite ?max_tramite)
    (not (AvisoUsuarioTramite ?tipotramite ?usr))

    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)

    (test(> (- (+ (* ?h 3600) (* ?m 60) ?s) ?segundos_comienzo) (* 60 ?max_tramite)))
    =>
    (assert (AvisoUsuarioTramite ?tipotramite ?usr))
  )

  (defrule AvisarTiempoGestionExcedido
    (declare (salience 1000))
    (AvisoUsuarioTramite ?tipotramite ?usr)
    =>
    (printout t "El usuario " ?tipotramite "-" ?usr " ha superado tiempo de gestión" crlf)
  )

  ;=======================
  ; Control de rendimiento
  ;=======================

  ; Primera vez que ficha el empleado
  (defrule FichaAlEntrar
    ?f <- (Ficha ?emp)
    (not (EmpleadoTrabajando ?emp ?estado ?h))
    (Tarea ?emp ?tipotramite)
    ?e <- (Empleados ?tipotramite ?total)
    ?z <- (Estado ?emp ?x)
    =>
    (bind ?time (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
    (assert
      (EmpleadoTrabajando ?emp 1 ?time) ; EmpleadoTrabajando{ 1 = Dentro Oficina, 0 = Fuera Oficina}
      (ComprobarRetraso ?emp ?time)
      (Empleados ?tipotramite (+ ?total 1))
      (Estado ?emp EnOficina)
    )
    (retract ?f ?e ?z)
  )

  ; Comprueba si ha llegado tarde
  (defrule ComprobarRetraso
    (declare (salience 1000))
    ?a<-(ComprobarRetraso ?emp ?tiempo_llegada)
    (ComienzoJornada ?hora_apertura)
    (TiempoMaximoRetraso ?tmax_retraso)
    =>
    (bind ?segundos_apertura (* ?hora_apertura 3600))
    (bind ?retraso (- ?tiempo_llegada ?segundos_apertura))
    (if (> ?retraso (* 60 ?tmax_retraso))
    then
      (printout t "El empleado " ?emp " llega tarde" crlf)
    else
      (printout t "El empleado " ?emp " llega a tiempo" crlf)
    )
    (retract ?a)
  )

  ; Para el resto de fichadas
  ; Si estado = 1 (en Oficina), pasa a 0 (Fuera) y viceversa
  (defrule Ficha
    ?f <- (Ficha ?emp)
    ?e <- (EmpleadoTrabajando ?emp ?estado ?hora)
    (Tarea ?emp ?tipotramite)
    ?d <- (Empleados ?tipotramite ?total)
    ?z <- (Estado ?emp ?x)

    =>
    (bind ?time (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))

    (if (= ?estado 1)
    then
      (assert
        (EmpleadoTrabajando ?emp 0 ?time)
        (Empleados ?tipotramite (- ?total 1))
        (ComprobacionDescanso ?emp ?time)
        (Estado ?emp Descansando)
      )
    else
      (assert
        (EmpleadoTrabajando ?emp 1 ?time)
        (Empleados ?tipotramite (+ ?total 1))
        (EliminarComprobacionDescanso ?emp)
        (Estado ?emp EnOficina)
      )
    )
    (retract ?f ?e ?d ?z)
  )

  ; Elimina la Comprobación para ver si sobrepasaba el tiempo de descanso
  (defrule EliminarComprobacionDescanso
    (declare (salience 1000))
    ?a <- (EliminarComprobacionDescanso ?emp)
    ?b <- (ComprobacionDescanso ?emp)
    =>
    (retract ?a ?b)
  )

  ; Comprueba si el tiempo de descanso ha sido superado
  (defrule TiempoDescansoSuperado
    (declare (salience 50))
    (ComprobacionDescanso ?emp ?time)
    (not(AvisoDescansoSuperado ?emp))
    (TiempoMaximoDescanso ?tmax_descanso)

    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)

    (test(> (- (+ (* ?h 3600) (* ?m 60) ?s) ?time) (* 60 ?tmax_descanso)))
    =>
    (assert(AvisoDescansoSuperado ?emp))
  )

  (defrule AvisoDescansoSuperado
    (declare (salience 1000))
    (AvisoDescansoSuperado ?emp)
    =>
    (printout t "El empleado " ?emp " ha superado el tiempo de descanso" crlf)
  )

;-------------------------
;==== Fin del Sistema ====
;-------------------------

; La hora actual es superior a la del cierre,
; Compruebo empleados mínimo Trámites y
; Genero Informes

  (defrule acabarSistema
    (declare (salience 5000))
    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)
    (FinalJornada ?hora_final)

    (test(> (+ (* ?h 3600) (* ?m 60) ?s) (* 3600 ?hora_final)))
    =>
    (assert
      (ComprobarMinimoTramites)
      (GenerarInformes)
    )
  )

; Informa de los empleados

  (defrule MinimoTramites
    (declare (salience 5001))
    (ComprobarMinimoTramites)
    (TramitesGestionados ?emp ?total)
    (Tarea ?emp ?tipotramite)
    (MinimoTramitesPorDia ?tipotramite ?minimo)
    (test(< ?total ?minimo))
    =>
    (printout t "El empleado " ?emp " no ha cumplido el mínimo de trámites: " ?total "/" ?minimo crlf)
  )

; Genera los Informes

  (defrule Informes
    (declare (salience 5000))
    (GenerarInformes)
    =>
    (printout t "Generando Informes..." crlf)
    (assert
      (GenerarInformeEsperas)
      (GenerarInformeTramites)
    )
  )

;=======================
;---- INFORME 1 y 2 ----
;=======================

; Dos Reglas para contar el total de esperas
  (defrule contarEsperas
    (declare (salience 5000))
    (GenerarInformeEsperas)
    (EsperaUsuario ?tipotramite ?tiempo_espera)
    =>
    (assert
      (sumarEspera ?tipotramite ?tiempo_espera)
    )
  )

  (defrule sumarEsperas
    (declare (salience 5000))
    ?a <- (sumarEspera ?tipotramite ?tiempo_espera)
    ?b <- (TotalEsperas ?tipotramite ?total_esperas ?tiempo_total)
    =>
    (retract ?a ?b)
    (assert
      (TotalEsperas ?tipotramite (+ 1 ?total_esperas) (+ ?tiempo_total ?tiempo_espera))
    )
  )

; Dos reglas para contar el total de trámites
  (defrule contarTramites
    (declare (salience 5000))
    (GenerarInformeTramites)
    (TramiteUsuario ?tipotramite ?emp ?tiempo_tramite)
    =>
    (assert
      (sumarTramite ?tipotramite ?tiempo_tramite)
    )
  )

  (defrule sumarTramites
    (declare (salience 5000))
    ?a <- (sumarTramite ?tipotramite ?tiempo_tramite)
    ?b <- (TotalTramites ?tipotramite ?total_tramites ?tiempo_total)
    =>
    (retract ?a ?b)
    (assert
      (TotalTramites ?tipotramite (+ 1 ?total_tramites) (+ ?tiempo_total ?tiempo_tramite))
    )
  )

; Calcula la sumatoria de la desviación
  (defrule calcularDesviacionEsperas
    (declare (salience 4999)) ; necesita ejecutarse cuando ya esté calculado TotalTramites
    (GenerarInformeTramites)
    (TotalEsperas ?tipotramite ?total_tramites ?tiempo_total)
    (EsperaUsuario ?tipotramite ?tiempo)
    =>
    (assert
      (sumarDesviacionEs (- ?tiempo (/ ?tiempo_total ?total_tramites)))
    )
  )

  (defrule sumarDesviacionEsperas
    (declare (salience 4999))
    ?a <- (sumarDesviacionEs ?t)
    ?b <- (SumaParcialDesviacionEs ?total)
    =>
    (retract ?a ?b)
    (assert
      (SumaParcialDesviacionEs (+ ?total (* ?t ?t)))
    )
  )

  (defrule calcularDesviacionTramites
    (declare (salience 4999)) ; necesita ejecutarse cuando ya esté calculado TotalTramites
    (GenerarInformeTramites)
    (TotalTramites ?tipotramite ?total_tramites ?tiempo_total)
    (TramiteUsuario ?tipotramite ?emp ?tiempo)
    =>
    (assert
      (sumarDesviacionTr (- ?tiempo (/ ?tiempo_total ?total_tramites)))
    )
  )

  (defrule sumarDesviacionTramites
    (declare (salience 4999))
    ?a <- (sumarDesviacionTr ?t)
    ?b <- (SumaParcialDesviacionTr ?total)
    =>
    (retract ?a ?b)
    (assert
      (SumaParcialDesviacionTr (+ ?total (* ?t ?t)))
    )
  )

  ; Apertura de informes
  (defrule AbrirInforme
    (declare (salience 4500))
    (GenerarInformes)
    =>
    (open "informe1.txt" informe1 "w")
    (open "informe2.txt" informe2 "w")
  )

  ; Escribe el informe 1 (Trámites)
  (defrule EscribirTiemposTotalesTr
    (declare (salience 4499))
    (GenerarInformes)
    (TotalTramites ?tipotramite ?total ?total_tiempo)
    (SumaParcialDesviacionTr ?parcial)
    =>
    (printout informe1 "Total Tramites " ?tipotramite ": " ?total ", media = "(/ ?total_tiempo ?total)
    ", desviacion = " (sqrt (* (/ 1 (- ?total 1)) ?parcial) )crlf)
  )
  ; Escribe el informe 1 (Esperas)
  (defrule EscribirTiemposTotalesEs
    (declare (salience 4499))
    (GenerarInformes)
    (TotalEsperas ?tipotramite ?total ?total_tiempo)
    (SumaParcialDesviacionEs ?parcial)
    =>
    (printout informe1 "Total esperas " ?tipotramite ": " ?total ", media = " (/ ?total_tiempo ?total)
    ", desviacion = " (sqrt (* (/ 1 (- ?total 1)) ?parcial) ) crlf)
  )

  ; Escribe el informe2
  (defrule EscribirEstadisticasEmpleados
    (declare (salience 4499))
    (GenerarInformes)
    (TramitesGestionados ?empl ?total ?total_tiempo)
    (test(> ?total_tiempo 1))
    =>
    (printout informe2 "Estadísticas Empleado " ?empl " : Tramites = " ?total ", tiempo atendiendo = " ?total_tiempo ", media = "(/ ?total_tiempo ?total) crlf)
  )

  ; Cierra los informes abiertos
  (defrule CerrarInforme
    (declare (salience 4498))
    (GenerarInformes)
    =>
    (close informe1)
    (close informe2)
  )

  ;==============================
  ; Control inteligente de luces
  ;==============================

  ; Actualiza la última vez que se activó
  ; El sensor del Pasillo
  (defrule actualizarTiempoPasillo
    (declare (salience 9001))
    ?p <- (Sensor_presencia Pasillo)
    ?g <- (TiempoUltimaVezPasillo ?tiempo)
    =>
    (bind ?nuevo_tiempo (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
    (assert
      (TiempoUltimaVezPasillo ?nuevo_tiempo)
    )
    (printout t "Sensor pasillo activado" crlf)
    (retract ?g ?p)
  )

  ; Enciende la luz de la oficina
  (defrule encenderLuzOficina
    (declare (salience 9000))
    ?s <- (Sensor_puerta ?hab)
    ?l <- (Luz ?hab OFF)
    ?p <- (TotalPersonas ?hab ?n)
    =>
    (assert
      (Luz ?hab ON)
      (TotalPersonas ?hab (+ ?n 1))
    )
    (printout t "Luz " ?hab " encendida" crlf)
    (retract ?s ?l ?p)
  )

  ; Aumenta las personas en una hab
  (defrule aumentaPersonasHabitacion
    (declare (salience 9000))
    ?s <- (Sensor_puerta ?hab)
    (TiempoUltimaVezPasillo ?tiempo)
    (Luz ?hab ON)
    ?p <- (TotalPersonas ?hab ?n)
    ; Si hace menos de 3 segundos que se activó el pasillo
    ; No se sabe si alguien ha entrado o salido
    ; Lo más normal es que entre, pero podía haber otra persona en el pasillo
    (test(>= 3 (- (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*) ?tiempo)))
    =>
    (assert(TotalPersonas ?hab (+ ?n 1)))
    (retract ?p ?s)
  )

  ; Disminuye las personas en una hab
  ; y apaga luz si es necesario

  (defrule saleOficina
    (declare (salience 9000))
    ?s <- (Sensor_puerta ?hab)
    (TiempoUltimaVezPasillo ?tiempo)
    ?l <- (Luz ?hab ON)
    ?p <- (TotalPersonas ?hab ?n)
    ; Si hace más de 3 segundos que se activó el pasillo (no había nadie, ergo ha salido)
    (test(< 3 (- (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*) ?tiempo)))
    =>
    (bind ?n_nuevo (- ?n 1))
    (if (<= ?n_nuevo 0)
      then
      (assert
        (Luz ?hab OFF)
        (TotalPersonas ?hab 0)
      )
      (printout t "LUZ " ?hab " APAGADA" crlf)
      else
      (assert
        (Luz ?hab ON)
        (TotalPersonas ?hab ?n_nuevo)
      )
    )
    (retract ?l ?p ?s)
  )

  ; Enciende luz aseo
  (defrule encenderLuzAseo
    (declare (salience 9000))
    ?s <- (Sensor_presencia ?aseo)
    ?l <- (Luz ?aseo ?estado)
    ?g <- (TiempoUltimaVez ?aseo ?t)
    =>
    (assert
      (Luz ?aseo ON)
      (TiempoUltimaVez ?aseo (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
    )
    (printout t "ENCIENTO/mantengo encendida luz " ?aseo crlf)
    (retract ?l ?g ?s)
  )
  ; Apaga luz aseo
  (defrule apagarLuzAseo
    (declare (salience 9000))
    ?l <- (Luz ?aseo ON)
    ?g <- (TiempoUltimaVez ?aseo ?t)

    (hora_actual ?h)
    (minutos_actual ?m)
    (segundos_actual ?s)

    (test(> (- (+ (* ?h 3600) (* ?m 60) ?s) (* 5 60)) ?t))
    =>
    (assert
      (Luz ?aseo OFF)
      (TiempoUltimaVez ?aseo 100000) ; Introduzco numero grande, asi esta regla no vuelve a saltar hasta que se actualice el sensor
    )
    (printout t "APAGO/mantengo apagada luz " ?aseo crlf)
    (retract ?l ?g)
  )

  ; Fuerza el apagado si el empleado está Descansando
  ; Sólo funciona para oficinas individuales
  (defrule ForzarApagado
    (Estado ?emp Descansando)
    (Empleado ?emp ?hab)
    (Individual ?hab) ; Oficinas individuales
    ?g <- (Luz ?hab ON)
    =>
    (retract ?g)
    (assert
      (Luz ?hab OFF)
    )
  )

; ==============================
; ==============================
;     SITUACIONES ANÓMALAS
; ==============================
; ==============================

; 1 - Oficina vacía sin justificacion
(defrule VuelveDeDesayunar
  (declare (salience 9500))
  (Estado ?emp Descansando)
  (Ficha ?emp)
  (not(HaDesayunado ?emp ?t))
  =>
  (bind ?nuevo_tiempo (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  (assert
    (HaDesayunado ?emp ?nuevo_tiempo)
  )
)

(defrule OficinaVaciaSinJustificacion
  (declare (salience 9500))
  (HaDesayunado ?emp ?tiempo)
  (Empleado ?emp ?hab)
  (Luz ?hab OFF)
  ; Tiempo desde que volvio desayuno mayor 5 minutos
  (test(< (* 60 5) (- (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*) ?tiempo)))
  (not (NoImportaSiVacia ?hab))
  =>
  (printout t "Oficina " ?hab " vacia sin explicacion" crlf)
)

; 2 - Hay más personas de las que deberia en una oficina
(defrule MasPersonasEsperado
  (declare (salience 9500))
  (TotalPersonas ?hab ?n)
  (Empleado ?emp ?hab)
  (Individual ?hab)
  (test (>= ?n 2))
  =>
  (if (> ?n 2)
  then
    (printout t "Mas personas de lo esperado en " ?hab ", hay " ?n crlf)
    (assert
      (PosibleEquivocacion ?hab (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
    )
  else
    (assert(ComprobarAsignado ?emp ?hab))
  )
)
(defrule ComprobarAsignado
  (declare (salience 9500))
  ?g <- (ComprobarAsignado ?emp ?hab)
  (Estado ?emp Asignado)
  =>
  (retract ?g)
)
(defrule ComprobarNoAsignado
  (declare (salience 9499))
  ?g <- (ComprobarAsignado ?emp ?hab)
  (Estado ?emp ?estado)
  =>
  (printout t "Mas personas de lo esperado en " ?hab crlf)
  (assert
  (PosibleEquivocacion ?hab (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  )
  (retract ?g)
)

; Usuario no responde
; Usuario fue asignado, nos disponemos a esperarlo
(defrule UsuarioFueAsignado
  (declare (salience 9500))
  (Asignado ?empl ?tipotramite ?usr)
  =>
  (bind ?tiempo (+ (* ?*hora* 3600) (* ?*minutos* 60) ?*segundos*))
  (assert (EsperandoAUsuario ?empl ?tipotramite ?usr ?tiempo))
)
; Usuario entró, dejamos de esperarlo
(defrule UsuarioEntroEnOficina
  (declare (salience 9500))
  ?g <- (EsperandoAUsuario ?empl ?tipotramite ?usr ?tiempo)
  (Empleado ?empl ?hab)
  (Sensor_puerta ?hab)
  =>
  (retract ?g)
)
; pasan 10 minutos desde que comenzó la espera
(defrule UsuarioNoResponde
  (declare (salience 9500))
  (Asignado ?empl ?tipotramite ?usr)
  (EsperandoAUsuario ?empl ?tipotramite ?usr ?tiempo)
  (not(AvisoNoResponder ?tipotramite ?usr))
  (hora_actual ?h)
  (minutos_actual ?m)
  (segundos_actual ?s)
  ; Si tarda mas de 15 minutos en entrar por la puerta
  (test(> (- (+ (* ?h 3600) (* ?m 60) ?s) ?tiempo) (* 15 60)))
  =>
  (assert
    (AvisoNoResponder ?tipotramite ?usr)
    (ComprobarEquivocacion ?tipotramite ?usr (+ (* ?h 3600) (* ?m 60) ?s) ?tiempo)
  )
)

(defrule NoHaRespondido
  (declare (salience 9500))
  (AvisoNoResponder ?tipotramite ?usr)
  =>
  (printout t "Usuario no responde a la llamada" crlf)
)

; Usuario se ha equivocado
; Si en una habitación había más personas de la cuenta
(defrule ComprobarEquivocacion
  (declare (salience 9500))
  ?a <- (ComprobarEquivocacion ?tipotramite ?usr ?tiempo_max ?tiempo_min)
  ?b <- (PosibleEquivocacion ?hab ?tiempo_med)
  (test(< ?tiempo_med ?tiempo_max))
  (test(> ?tiempo_med ?tiempo_min))
  =>
  (printout t "Posiblemente el usr " ?tipotramite ?usr " haya entrado por equivocacion en la oficina " ?hab crlf)
  (retract ?a ?b)
)

; Usuario ha entrado en generencia u oficina

; quien ha entrado probablemente haya sido un empleado
(defrule EmpleadoEnGerenciaPapeleria
  (declare (salience 9500))
  ; Se activa el sensor de alguna de esas habs
  (Sensor_puerta ?hab)
  (NoImportaSiVacia ?hab)

  (Empleado ?emp ?hab)
  (or
    (Estado ?emp EnOficina)
    (Estado ?emp Atendiendo)
    (Estado ?emp Disponible)
  )
  (TotalPersonas ?hab 0)
  =>
  (assert (EmpleadoHaVisitado ?hab))
)

; No ha sido un empleado
(defrule UsuarioEnGerenciaPapeleria
  (declare (salience 9499))
  (Sensor_puerta ?hab)
  (NoImportaSiVacia ?hab)
  (not (EmpleadoHaVisitado ?hab))
  =>
  (printout t "Usuario posiblemente en " ?hab crlf)
)

; Fue el empleado
(defrule eliminarVisitaEmpleado
  (declare (salience 9498))
  ?g <- (EmpleadoHaVisitado ?hab)
  =>
  (retract ?g)
)

;(defrule SensorPuerta
;  (declare (salience 2000))
;  (Sensor_puerta ?hab)
;  =>
;  (assert
;    (Sensor_puerta ?hab)
;  )
;)
