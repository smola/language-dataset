;##############################################################################
;#                       TEMPLATES QUE UTILIZA EL SISTEMA                     #
;#                                                                            #
;#      En esta seccion se declaran todos los templates de los que hace uso   #
;#          el sistema, ademas de una expliacion de cada uno de los slots     #
;##############################################################################

;-----------------------------------------------
;   Template para los datos de las empresas
;-----------------------------------------------

(deftemplate Empresa
    ; Nombre de la empresa
    (multislot Nombre)
    ; Precio de las acciones
    (slot Precio (type FLOAT))
    ; Porcentaje de variacion del dia
    (slot Var_dia (type FLOAT))
    ; Capitalizacion en miles de euros
    (slot Capitalizacion (type FLOAT))
    ; PER -->  Price Earning Ratio o relacion precio-beneficio
    (slot PER (type FLOAT))
    ; RPD --> Rentabilidad por dividendo
    (slot RPD (type FLOAT))
    ; Tamanio de la empresa --> Pequenio, Mediano, Grande
    (slot Tamanio)
    ; Porcentaje en el IBEX35
    (slot Porcentaje_IBEX (type FLOAT))
    ; Etiqueta asociada al PER --> Alto, Medio, Bajo
    (slot Etiq_PER)
    ; Etiqueta asociada al RPD ---> Alto, Medio, Bajo
    (slot Etiq_RPD)
    ; Sector al que pertenece la empresa
    (slot Sector)
    ; Porcentaje asociado a la variacion del valor durante los ultimos 5 dias
    (slot Porcentaje_Var_5_Dias (type FLOAT))
    ; Flag que se activa si el valor pierde durante 3 dias consecutivos o mas
    (slot Perd_3consec)
    ; Flag que se activa si el valor pierde durante 5 dias consecutivos o mas
    (slot Perd_5consec)
    ; Porcentaje de variacion respecto al sector en los ultimos 5 dias
    (slot Perd_VarRespSector5Dias (type FLOAT))
    ; Variacion respecto al sector los ultimos 5 dias
    (slot VRS5_5)
    ; Variacion mensual del precio 
    (slot Porcentaje_VarMen (type FLOAT))
    ; Variacion trimestral del precio
    (slot Porcentaje_VarTri (type FLOAT))
    ; Variacion del precio durante un semestre
    (slot Porcentaje_VarSem (type FLOAT))
    ; Variacion del precio durante un anio
    (slot Porcentaje_Var12meses (type FLOAT))
)

;-----------------------------------------------
;   Template para los sectores
;-----------------------------------------------

(deftemplate Sector
    ; Nombre de la empresa
    (multislot Nombre)
    ; Porcentaje de variacion del dia
    (slot Var_dia (type FLOAT))
    ; Capitalizacion en miles de euros
    (slot Capitalizacion (type FLOAT))
    ; PER -->  Price Earning Ratio o relacion precio-beneficio
    (slot PER (type FLOAT))
    ; RPD --> Rentabilidad por dividendo
    (slot RPD (type FLOAT))
    ; Porcentaje en el IBEX35
    (slot Porcentaje_IBEX (type FLOAT))
    ; Porcentaje asociado a la variacion del sector durante los ultimos 5 dias
    (slot Porcentaje_Var_5_Dias (type FLOAT))
    ; Flag que se activa si el sector pierde durante 3 dias consecutivos o mas
    (slot Perd_3consec)
    ; Flag que se activa si el sector pierde durante 5 dias consecutivos o mas
    (slot Perd_5consec)
    ; Variacion mensual del precio 
    (slot Porcentaje_VarMen (type FLOAT))
    ; Variacion trimestral del precio
    (slot Porcentaje_VarTri (type FLOAT))
    ; Variacion del precio durante un semestre
    (slot Porcentaje_VarSem (type FLOAT))
    ; Variacion del precio durante un anio
    (slot Porcentaje_Var12meses (type FLOAT))
)

;-----------------------------------------------
;   Template para las noticias
;-----------------------------------------------

(deftemplate Noticia
    ; Indica a que afecta la noticia
    (slot Sobre)
    ; El tipo de noticia --> Buena o Mala
    (slot Tipo)
    ; El numero de dias de antigüedad de la noticia
    (slot Antiguedad (type NUMBER))
)

;-----------------------------------------------
;   Template para los datos pertenecientes a la
;                   cartera
;
;-----------------------------------------------
;       Los datos del dinero en efectivo o
;       DISPONIBLE tendran el mismo valor tanto
;       en "Acciones" como en "ValorActual"
;-----------------------------------------------

(deftemplate Cartera
    ; Nombre de la empresa de la que tenemos acciones
    (slot Nombre)
    ; Numero de acciones que tenemos de esa empresa
    (slot Acciones (type NUMBER))
    ; Valor real de las acciones que tenemos para esa empresa
    (slot ValorActual (type FLOAT))
)

;-----------------------------------------------
;   Template para modelar una propuesta que 
;           genere el sistema experto
;-----------------------------------------------

(deftemplate Propuesta
    ; Almacena el tipo de propuesta que genera
    ;  - Vender empresas peligrosas
    ;  - Vender empresas sobrevaloradas
    ;  - Comprar empresa infravalorada
    ;  - Intercambiar a más rentable
    (slot Tipo)
    ; Nombre de la empresa a la que afecta principalmente
    (slot Nombre1)
    ; Nombre de la segunda empresa, se usa en los intercambios
    (slot Nombre2)
    ; Rendimiento esperado de la propuesta
    (slot RE)
    ; Explicación que genera el sistema 
    (slot Explicacion)
)

;##############################################################################
;#                       MODULO INICIAL DEL PROGRAMA                          #
;#                                                                            #
;#     Cargar el fichero de datos sobre el cierre de la bolsa y dar paso al   #
;#                         resto de modulos del sistema                       #
;##############################################################################

;---------------------------------------------------------------
; Funcion que inicia el sistema
;   - Inserta en la base de hechos IniciarSistema para
;     poder disparar la regla que carga los datos del cierre
;
;   El salience es necesario para obligar a que sea la primera
;   regla que se ejecute 
;---------------------------------------------------------------

(defrule start
    (declare (salience 100))
    => 
    (printout t crlf "¡Bienvenido!" crlf)
    (assert (IniciarSistema))
)

;---------------------------------------------------------------
; Funcion para abrir el fichero de datos del cierre
;---------------------------------------------------------------

(defrule cargarAnalisis
    ?f <- (IniciarSistema)
    =>
    (open "./Analisis.txt" mydata)
    (assert (SeguirLeyendo))
    (retract ?f)
)

;---------------------------------------------------------------
; Funcion que lee los datos y los carga en la base de hechos
;---------------------------------------------------------------

(defrule leerValoresCierreFromFile
    ?f <- (SeguirLeyendo)
    =>
    (bind ?valor (read mydata))
    (retract ?f)
    (if (neq ?valor EOF) then
        (assert (Empresa
                    (Nombre ?valor)
                    (Precio (read mydata))
                    (Var_dia (read mydata))
                    (Capitalizacion (read mydata))
                    (PER (read mydata))
                    (RPD (read mydata))
                    (Tamanio (read mydata))
                    (Porcentaje_IBEX (read mydata))
                    (Etiq_PER (read mydata))
                    (Etiq_RPD (read mydata))
                    (Sector (read mydata))
                    (Porcentaje_Var_5_Dias (read mydata))
                    (Perd_3consec (read mydata))
                    (Perd_5consec (read mydata))
                    (Perd_VarRespSector5Dias (read mydata))
                    (VRS5_5 (read mydata))
                    (Porcentaje_VarMen (read mydata))
                    (Porcentaje_VarTri (read mydata))
                    (Porcentaje_VarSem (read mydata))
                    (Porcentaje_Var12meses (read mydata)))
        )
        (assert (SeguirLeyendo)))
)

;---------------------------------------------------------------
;   Funcion que cierra el fichero sobre el cierre y da paso
;   al modulo que carga los datos sobre los sectores
;---------------------------------------------------------------

(defrule cerrarFicheroValoresCierre
    =>
    (close mydata)
    (assert (leerSectores))
)

;##############################################################################
;#                   MODULO PARA LEER LOS DATOS DE LOS SECTORES               #
;##############################################################################

;---------------------------------------------------------------
; Funcion para abrir el fichero
;---------------------------------------------------------------

(defrule abrirFicheroSectores
    ?f <- (leerSectores)
    =>
    (open "./AnalisisSectores.txt" sectores)
    (assert (continuarConSectores))
    (retract ?f)
)

;---------------------------------------------------------------
; Funcion que lee los datos y los carga en la base de hechos
;---------------------------------------------------------------

(defrule LeerValoresSectoresFromFile
    ?f <- (continuarConSectores)
    =>
    (bind ?valor (read sectores))
    (retract ?f)
    (if (neq ?valor EOF) then
        (assert (Sector
                    (Nombre ?valor)
                    (Var_dia (read sectores))
                    (Capitalizacion (read sectores))
                    (PER (read sectores))
                    (RPD (read sectores))
                    (Porcentaje_IBEX (read sectores))
                    (Porcentaje_Var_5_Dias(read sectores))
                    (Perd_3consec(read sectores))
                    (Perd_5consec(read sectores))
                    (Porcentaje_VarMen (read sectores))
                    (Porcentaje_VarTri (read sectores))
                    (Porcentaje_VarSem (read sectores))
                    (Porcentaje_Var12meses (read sectores)))
        )
        (assert (continuarConSectores))
        else
            (assert (finalizarSectores))
    )
)

;---------------------------------------------------------------
;   Funcion que cierra el fichero sobre los sectores y da paso
;   al modulo que carga las noticias
;---------------------------------------------------------------

(defrule cerrarFicheroSectores
    ?f <- (finalizarSectores)
    =>
    (close sectores)
    (assert (leerNoticias))
)

;##############################################################################
;#                MODULO PARA LEER LOS DATOS DE LA NOTICIAS                   #
;##############################################################################

;---------------------------------------------------------------
; Funcion para abrir el fichero
;---------------------------------------------------------------

(defrule abrirFicheroNoticias
    ?f <- (leerNoticias)
    =>
    (open "./Noticias.txt" noticias)
    (assert (continuarConNoticias))
    (retract ?f)
)

;---------------------------------------------------------------
; Funcion que lee los datos y los carga en la base de hechos
;---------------------------------------------------------------

(defrule LeerNoticiasFromFile
    ?f <- (continuarConNoticias)
    =>
    (bind ?valor (read noticias))
    (retract ?f)
    (if (neq ?valor EOF) then
        (assert (Noticia
                    (Sobre ?valor)
                    (Tipo (read noticias))
                    (Antiguedad (read noticias))
        ))
        (assert (continuarConNoticias))
        else
            (assert (finLeerNoticias))
    )
)

;---------------------------------------------------------------
;   Funcion que cierra el fichero sobre las noticias y da paso
;   al modulo que carga la cartera
;---------------------------------------------------------------

(defrule cerrarFicheroNoticias
    ?f <- (finLeerNoticias)
    =>
    (close noticias)
    (retract ?f)
    (assert (leerCartera))
)

;##############################################################################
;#                MODULO PARA DETECTAR VALORES INESTABLES                     #
;##############################################################################

;---------------------------------------------------------------
;   Regla que inicia el funcionamiento del modulo para
;   detectar valores inestables
;---------------------------------------------------------------
(defrule moduloValoresInestables
    ?f <- (detectarValoresInestables)
    => 
    (retract ?f)
    (printout t "Detectando valores inestables..." crlf)
    (assert (detectarValoresConstruccion))
)

;---------------------------------------------------------------
;   Por definicion, los valores asociados al sector de la 
;   construccion son valores inestables. Esta regla se 
;   encarga de introducir esto en la base de hechos.
;---------------------------------------------------------------

(defrule valoresInestablesConstruccion
    ?f<-(detectarValoresConstruccion)
    (Empresa (Nombre ?nombreEmpresa) (Sector Construccion))
    =>
    
    (assert (Inestable ?nombreEmpresa (str-cat "La empresa " ?nombreEmpresa 
        " pasa a ser inestable porque pertenece al sector de la Construccion")))

    (printout t (str-cat "La empresa " ?nombreEmpresa " pasa a ser "
        "inestable porque pertenece al sector de la Construccion")  crlf)
)

;---------------------------------------------------------------
;   Finaliza la carga de valores inestables en la construccion
;   y da paso a la siguiente regla sobre los valores del sector
;   servicios.
;---------------------------------------------------------------

(defrule finValoresConstruccion
    (declare (salience -10))

    ?f <- (detectarValoresConstruccion)
    =>
    (retract ?f)
    (assert (detectarServiciosInestables))
)


;---------------------------------------------------------------
;   Por definicion, el sector servicios sera inestable si 
;   la economia ("Ibex") esta bajando
;---------------------------------------------------------------

;##############################################################################
;   * Como el experto no lo ha especificado, que la economia este bajando     ;
;     significa que la economia lleva 5 dias consecutivos en perdidas *       ;
;##############################################################################

(defrule sectorServiciosInestablePorEconomiaBajando
    (detectarServiciosInestables)
    (Sector (Nombre Ibex) (Perd_5consec true))
    (Empresa (Nombre ?nombreEmpresa) (Sector Servicios))
    =>
    (printout t "La empresa " ?nombreEmpresa " pasa a ser inestable porque "
        "pertenece al sector Servicios y la economia esta bajando" crlf)
    (assert (Inestable ?nombreEmpresa (str-cat "La empresa " ?nombreEmpresa 
        " pasa a ser inestable porque pertenece al sector 
        Servicios y la economia esta bajando")))
)

;---------------------------------------------------------------
;   Termina la deteccion de los valores inestables por defecto
;   del sector servicios y da paso a las siguientes reglas
;
;---------------------------------------------------------------

(defrule finDeteccionServiciosInestables
    (declare (salience -10))
    ?f <- (detectarServiciosInestables)
    =>
    (retract ?f)
    (printout t "Leyendo noticias..." crlf)
    (assert (detectarNoticias))
)

;---------------------------------------------------------------
;   Al producirse una mala noticia que afecta, si afecta a una 
;   empresa, el valor pasa a ser inestable durante dos dias. En
;   el caso de que le afecte al sector, este se volvera inestable
;   y todos los valores de este sector, se volveran inestables
;---------------------------------------------------------------


;---------------------------------------------------------------
;   Por definicion, el sector servicios sera inestable si 
;   la economia ("Ibex") esta bajando
;---------------------------------------------------------------

;##############################################################################
;   * Como el experto no lo ha especificado, que la economia este bajando     ;
;     significa que la economia lleva 5 dias consecutivos en perdidas *       ;
;##############################################################################

(defrule sectorServiciosInestablePorEconomiaBajando
    (detectarServiciosInestables)
    (Sector (Nombre Ibex) (Perd_5consec true))
    (Empresa (Nombre ?nombreEmpresa) (Sector Servicios))
    =>
    (printout t "La empresa " ?nombreEmpresa " pasa a ser inestable porque "
        "pertenece al sector Servicios y la economia esta bajando" crlf)
    (assert (Inestable ?nombreEmpresa (str-cat "La empresa " ?nombreEmpresa 
        " pasa a ser inestable porque pertenece al sector 
        Servicios y la economia esta bajando")))
)

;---------------------------------------------------------------
;   Termina la deteccion de los valores inestables por defecto
;   del sector servicios y da paso a las siguientes reglas
;
;---------------------------------------------------------------

(defrule finDeteccionServiciosInestables
    (declare (salience -10))
    ?f <- (detectarServiciosInestables)
    =>
    (retract ?f)
    (printout t "Leyendo noticias..." crlf)
    (assert (detectarNoticias))
)

;---------------------------------------------------------------
;   Al producirse una mala noticia que afecta, si afecta a una 
;   empresa, el valor pasa a ser inestable durante dos dias. En
;   el caso de que le afecte al sector, este se volvera inestable
;   y todos los valores de este sector, se volveran inestables
;---------------------------------------------------------------

(defrule detectarNoticiasNegativas
    (detectarNoticias)
    (Noticia
        (Sobre ?s)
        (Tipo Mala)
        (Antiguedad ?ant))
    (or (Empresa (Nombre ?s))
        (Sector (Nombre ?s)))
    (test (<= ?ant 2))
    =>
    (assert (Inestable ?s (str-cat ?s " pasa a ser inestable durante dos dias "
        "porque hay una noticia mala que le afecta")))
    (printout t ?s " pasa a ser inestable durante dos dias porque hay una "
        "noticia mala que le afecta" crlf)
)

;---------------------------------------------------------------
;   Al producirse la mala noticia sobre el sector, esta regla 
;   se encarga de introducir en la base de hechos el 
;   conocimiento de que los valores asociados al sector se 
;   han vuelto inestables
;---------------------------------------------------------------

(defrule sectorInestable
    (detectarNoticias)
    (Noticia
        (Sobre ?s)
        (Tipo Mala)
        (Antiguedad ?ant))
    (Empresa (Nombre ?nombre) (Sector ?s))
    (test (<= ?ant 2))
    =>
    (assert (Inestable ?nombre (str-cat ?nombre " pasa a ser inestable "
        "durante dos dias porque su sector ha pasado a ser inestable")))
    (printout t (str-cat ?nombre " pasa a ser inestable durante dos dias porque "
            "su sector ha pasado a ser inestable") crlf)
)

;---------------------------------------------------------------
;   Cuando se ha producido una noticia que afecta a la economia
;   todos los valores se vuelven inestables, por lo que 
;   se introduce este conocimiento con el hecho 
;               
;                      (Inestable Todo ...)
;---------------------------------------------------------------

(defrule economiaInestable
    (detectarNoticias)
    (Noticia
        (Sobre Ibex)
        (Tipo Mala)
        (Antiguedad ?ant))
    (test (<= ?ant 2))
    =>
    
    (assert (Inestable Todo (str-cat "Toda la economia ha pasado a ser "
        " inestable durante 2 dias al haber una mala noticia sobre el Ibex")))
    
    (printout t "Toda la economia ha pasado a ser inestable durante 2 dias al "
        "haber una mala noticia sobre el Ibex" crlf)
)


;---------------------------------------------------------------
;   Esta regla se encarga de eliminar de la base de hechos los
;   valores inestables al encontrarse una buena noticia sobre
;   estos en la base de hechos, siempre y cuando esta noticia 
;   tenga menos de dos dias de tiempo
;---------------------------------------------------------------

(defrule detectarNoticiasPositivas
    (declare (salience -1))
    (detectarNoticias)
    (Noticia
        (Sobre ?s)
        (Tipo Buena)
        (Antiguedad ?ant))
    (or (Empresa (Nombre ?s))
        (Sector (Nombre ?s)))
    ?f <- (Inestable ?s ?texto)
    (test (<= ?ant 2))
    =>
    (retract ?f)
    (printout t ?s " deja de ser inestable porque existe una noticia 
        buena lo referencia con de hace " ?ant " dias" crlf)
)

;---------------------------------------------------------------
;   Al producirse una buena noticia que afecta a un sector,
;   los valores asociados a este sector dejan de ser 
;   inestables durante dos dias
;---------------------------------------------------------------

(defrule sectorEstable
    (declare (salience -1))
    (detectarNoticias)
    (Noticia
        (Sobre ?s)
        (Tipo Buena)
        (Antiguedad ?ant))
    (Empresa (Nombre ?nombre) (Sector ?s))
    ?f <- (Inestable ?nombre ?texto)
    (test (<= ?ant 2))
    =>
    (retract ?f)
    (printout t ?nombre " deja de ser inestable porque existe 
        una noticia buena sobre su sector" crlf)
)

;---------------------------------------------------------------
;   Esta regla se encarga de dar fin al modulo de deteccion de
;   valores inestables y da paso al modulo de deteccion de 
;   valores peligrosos
;---------------------------------------------------------------

(defrule finModuloInestables
    (declare (salience -10))
    ?f<-(detectarNoticias)
    =>
    (retract ?f)
    (assert (inicioValSobr))
)

;##############################################################################
;#                 MODULO PARA DETECTAR VALORES SOBREVALORADOS                #
;##############################################################################

;---------------------------------------------------------------
;   Regla que inicia el modulo
;---------------------------------------------------------------

(defrule inicioValSobr
    ?f <- (inicioValSobr)
    =>
    (retract ?f)
    (printout t "Detectando valores sobrevalorados..." crlf)
    (assert (detectandoValSobr))

)

; ---------------------------------------------------------------
;   Esta regla se encarga de forma general, de aniadir los
;   valores que esten sobrevalorados debido a un PER alto 
;   y un RPD Bajo
; ---------------------------------------------------------------

(defrule detectarValoresSobrevaloradosGeneral
    (detectandoValSobr)
    (Empresa (Nombre ?N) (Etiq_PER Alto) (Etiq_RPD Bajo))
    =>
    (assert (Sobrevalorado ?N (str-cat ?N " es un valor sobrevalorado ya que "
        " como norma general, los valores con PER Alto y RPD Bajo, estan "
        "sobrevalorados por definicion, como es este caso")))
)

;---------------------------------------------------------------
;   Esta regla es mas especifica que la anterior y se
;   aplica sobre aquellas empresas con un tamanio PEQUENIA
;   que tengan un PER Alto, o un PER Mediano y un RPD Bajo
;---------------------------------------------------------------

(defrule detectarValoresSobrevaloradosEmpresaPequenia
    (detectandoValSobr)
    (or (Empresa (Nombre ?N) 
        (Tamanio PEQUENIA)
        (Etiq_PER Alto)
        (Etiq_RPD Bajo))

        (Empresa (Nombre ?N) 
        (Tamanio PEQUENIA)
        (Etiq_PER Medio)
        (Etiq_RPD Bajo))
    )
    =>
    (assert (Sobrevalorado ?N (str-cat ?N " es un valor de una empresa PEQUENIA"
        " sobrevalorado ya que su PER es Alto, o bien su PER es Medio y "
        "su RPD es Bajo")))
)

;---------------------------------------------------------------
;   Esta regla es similar a la anterior, pero aplicada a las
;   empresas con un tamanio GRANDE, y que tengan un RPD Bajo y 
;   un PER, Medio o Alto; o tengan un RPD Medio y un
;   PER Alto
;---------------------------------------------------------------

(defrule detectarValoresSobrevaloradosEmpresaGrande
    (detectandoValSobr)
    (or (Empresa (Nombre ?N) (Etiq_RPD Bajo) (Etiq_PER Medio) (Tamanio GRANDE))
        (Empresa (Nombre ?N) (Etiq_RPD Bajo) (Etiq_PER Alto) (Tamanio GRANDE))
        (Empresa (Nombre ?N) (Etiq_RPD Medio) (Etiq_PER Alto) (Tamanio GRANDE))
    )
    =>
    (assert (Sobrevalorado ?N (str-cat ?N " es un valor de una empresa GRANDE sobrevalorado ya "
        "que su RPD es Bajo y su PER es Medio o Alto, o bien su RPD es Medio "
        "y su PER es Alto")))

    (printout (str-cat ?N " es un valor de una empresa GRANDE sobrevalorado ya "
        "que su RPD es Bajo y su PER es Medio o Alto, o bien su RPD es Medio "
        "y su PER es Alto"))
)

;---------------------------------------------------------------
;   Regla que da fin a este modulo y empieza con el modulo
;   encargado de detectar valores infravalorados
;---------------------------------------------------------------

(defrule finModValSobr
    (declare (salience -10))
    ?f<-(detectandoValSobr)
    =>
    (retract ?f)
    (assert (inicioValoresInfr))    
)

;##############################################################################
;#                 MODULO PARA DETECTAR VALORES INFRAVALORADOS                #
;##############################################################################

;---------------------------------------------------------------
;   Regla que inicia el modulo
;---------------------------------------------------------------

(defrule inicioValInfr
    ?f <- (inicioValoresInfr)
    =>
    (retract ?f)
    (printout t "Detectando valores infravalorados..." crlf)
    (assert (detectandoValoresInfr))
)

;---------------------------------------------------------------
;   Regla que detecta los valores que tienen un PER Bajo y un
;   RPD alto, que pasan a ser valores infravalorados.
;---------------------------------------------------------------

(defrule infravaloradoGeneral
    (detectandoValoresInfr)
    (Empresa 
        (Nombre ?Nombre)
        (Etiq_PER Bajo)
        (Etiq_RPD Alto))
    =>
    (assert (Infravalorado ?Nombre (str-cat ?Nombre " es una empresa "
            "infravalorada ya su PER es BAJO y su RPD es Alto, por lo que"
            " pasa a ser una empresa infravalorada."))
    )
)

;---------------------------------------------------------------
;   Esta regla detecta los valores que han caido al menos un
;   30% durante los últimos meses, que tienen un PER bajo,
;   y que ademas han subido algo en el ultimo mes, pero no mucho
;---------------------------------------------------------------

(defrule detectarValoresInfravalorados
    (detectandoValoresInfr)
    (Empresa 
        (Nombre ?Nombre) (Porcentaje_VarMen ?mes) (Porcentaje_VarTri ?tri)
        (Porcentaje_VarSem ?semestre) (Porcentaje_Var12meses ?anio) (Etiq_PER Bajo))
    (not (Infravalorado ?Nombre ?$))
    ; Comprobamos si el valor ha caido al menos un 30% en el ultimo trimestre,
    ; semestre o anio y que no ha subido mucho
    (test (or (<= ?tri -30) (<= ?semestre -30) (<= ?anio -30)))
    (test (> ?mes 0))
    (test (< ?mes 10))
    =>
    (assert (Infravalorado ?Nombre (str-cat ?Nombre " es una empresa infravalorada"
        " ya que su PER es Bajo y durante el ultimo trimestre, semestre o anio"
        " ha caido al menos un 30% y ahora esta subiendo, aunque no demasiado")))
)
;---------------------------------------------------------------
;   Esta regla detecta sin una empresa grande esta infravalorada.
;   Si la empresa es grande, el RPD es alto y el PER Mediano, 
;   ademas no esta bajando y se comporta mejor que su sector, 
;   la empresa esta infravalorada
;---------------------------------------------------------------

(defrule detectarValorInfravaloradoGrandes
    (detectandoValoresInfr)
    (Empresa 
        (Nombre ?Nombre)
        (Etiq_RPD Alto)
        (Etiq_PER Medio)
        (Perd_5consec false)
        (VRS5_5 false)
        (Tamanio GRANDE))
    =>
    (assert (Infravalorado ?Nombre (str-cat ?Nombre " es grande, el RPD es alto"
        " y el PER Medio, ademas no esta bajando y se comporta mejor que "
        "su sector, la empresa esta infravalorada")))
)

;---------------------------------------------------------------
;   Regla que da fin al modulo 3 y da paso al modulo
;---------------------------------------------------------------

(defrule finValoresInfr
    (declare (salience -10))
    ?f<-(detectandoValoresInfr)
    =>
    (retract ?f)
    (assert (moduloValoresPeligrosos))
)

;##############################################################################
;#                MODULO PARA DETECTAR VALORES PELIGROSOS                     #
;##############################################################################

;---------------------------------------------------------------
;   Inicia el modulo para detectar valores peligrosos y
;   da paso a las reglas de este modulo
;---------------------------------------------------------------

(defrule inicioModuloValoresPeligrosos
    ?f <- (moduloValoresPeligrosos)
    =>
    (retract ?f)
    (printout t "Detectando valores peligrosos..." crlf)
    (assert (detectandoValPel))
)

;---------------------------------------------------------------
;   Esta regla se encarga de aniadir al conocimiento de la base
;   de hechos que un valor es peligroso si lleva 3 dias
;   consecutivos bajando, es inestable y tenemos dinero 
;   invertido en este valor
;---------------------------------------------------------------

(defrule detectarValorPeligroso3dias
    (detectandoValPel)
    (or (Inestable ?Nombre ?texto)
        (Inestable Todo ?texto))
    (Cartera (Nombre ?Nombre))
    (Empresa (Nombre ?Nombre) (Perd_3consec true))
    =>
    (assert (Peligroso ?Nombre (str-cat ?Nombre " pasa a ser valor peligroso al 
        llevar 3 dias consecutivos en perdidas y hay dinero invertido en " ?Nombre)))
    
    (printout t  ?Nombre " pasa a ser valor peligroso al llevar 3 dias 
        consecutivos en perdidas y hay dinero invertido en " ?Nombre crlf)

)

;---------------------------------------------------------------
;   Esta regla se encarga de aniadir al conocimiento de la base
;   de hechos que un valor es peligroso si lleva 5 dias
;   consecutivos bajando y tenemos dinero invertido 
;   en este valor
;---------------------------------------------------------------

(defrule detectarValorPeligroso5dias
    (detectandoValPel)
    (Cartera (Nombre ?Nombre))
    (Empresa (Nombre ?Nombre) (Sector ?sector) (VRS5_5 true))
    ; (not (Peligroso ?Nombre ?$))
    =>
    
    (assert (Peligroso ?Nombre (str-cat ?Nombre " pasa a ser valor peligroso al 
        llevar 5 dias consecutivos en perdidas, hay dinero invertido en "?Nombre 
        " y la variacion respecto al sector " ?sector " es mayor del 5%")))
    
    (printout t  ?Nombre " pasa a ser valor peligroso al llevar 5 dias 
        consecutivos en perdidas, hay dinero invertido en "?Nombre " y la 
        variacion respecto al sector " ?sector " es mayor del 5%" crlf)
)

;---------------------------------------------------------------
;   Con esta regla finalizamos el modulo y damos paso al 
;   modulo para detectar valores sobrevalorados
;---------------------------------------------------------------

(defrule finValPelig
    (declare (salience -10))
    ?f<-(detectandoValPel)
    =>
    (retract ?f)
    (assert (RealizarPropuestas))
)

;##############################################################################
;#                 MODULO PARA LA REALIZACION DE PROPUESTAS                   #
;##############################################################################

;---------------------------------------------------------------
;   Con esta regla iniciamos el modulo encargado de obtener
;   las propuestas que se mostrarán al usuario
;---------------------------------------------------------------

(defrule inicioRealizacionPropuestas
    ?f<-(RealizarPropuestas)
    =>
    (retract ?f)
    (assert (obtenerPropuestas))
)

;---------------------------------------------------------------
;   Esta regla genera propuestas nuevas debido a que 
;   existe un valor peligroso y se recomienda venderlo
;---------------------------------------------------------------

(defrule propuestaVenderEmpresaPeligrosa
    (obtenerPropuestas)
    ; Existe una empresa
    (Empresa (Nombre ?nombre) (Porcentaje_VarMen ?varMenEmpresa) (Sector ?nombreSector)
        (RPD ?rpd))
    ; Detectada como valor peligroso
    (Peligroso ?nombre ?explicacion)
    (Sector (Nombre ?nombreSector) (Porcentaje_VarMen ?varMenSector))
    ; Comprobamos si esta bajando en el ultimo mes
    (test (< ?varMenEmpresa 0))
    ; Y si la variacion respecto a su sector es menor del 3%
    (test (< (- (abs ?varMenEmpresa) (abs ?varMenSector)) -3))
    =>
    (assert (Propuesta 
        (Tipo Vender_Peligroso) 
        (Nombre1 ?nombre) 
        (Nombre2 NA)
        (RE (- 20 (* ?rpd 100))) 
        (Explicacion (str-cat "La empresa " ?nombre " es peligrosa porque es un"
                    " valor peligroso, y esta entrando en tendencia bajista respecto a"
                    " su sector. Segun mi estimacion, existe una probabilidad no "
                    "despreciable de que pueda caer al cabo del anio un 20%, aunque"
                    " produzca un " ?rpd "% por dividendos perderiamos un " (- 20 ?rpd) "%")))
    )
)
;----------------------------------------------------------------------
;   Con esta regla proponemos invertir en empresas infravaloradas
;   Si una empresa esta infravalorada  y el usuario tiene  dinero para 
;   invertir proponer invertir el dinero en  las acciones de la empresa. 
;                   RE=(PERMedio-PER)*100/(5*PER)+RPD 
;----------------------------------------------------------------------


(defrule propuestaInvertirInfravaloradas
    (obtenerPropuestas)
    (Empresa 
        (Nombre ?nombre)
        (RPD ?RPD)
        (PER ?PER)
        (Sector ?nombreSector))
    (Sector 
        (Nombre ?nombreSector)
        (PER ?PERMedio))
    (Infravalorado ?nombre ?explicacion)
    (Cartera
        (Nombre DISPONIBLE)
        (Acciones ?v))
    (not (Cartera (Nombre ?nombre)))
    (test (> ?v 0))
    =>
    (if (not (eq ?PER 0)) then
        (assert (Propuesta
            (Tipo ComprarInfravalorado)
            (Nombre1 ?nombre)
            (Nombre2 NA)
            (RE (+ (/ (* (- ?PERMedio ?PER) 100) (* 5 ?PER)) (* ?RPD 100)))
            (Explicacion (str-cat ?nombre " esta infravalorada y seguramente el PER tienda"
                        " al PER medio en 5 anios, con lo que se deberia revalorizar un "
                        (/ (* (- ?PERMedio ?PER) 100) (* 5 ?PER)) " anual a lo que" 
                        " habria que sumar el " (* ?RPD 100) "% RPD de beneficios por dividendos"))
                )
        )
    )
)

;----------------------------------------------------------------------
;   En esta regla, se proponen vender valores de empresas sobrevaloradas 
;   Si una empresa de mi cartera esta sobrevalorada y el rendimiento 
;   por anio < 5 + precio dinero, proponer vender las acciones de 
;   esa empresa.
;               RE = -RPD+(PER-PERMedioSector)/(5*PER)
;----------------------------------------------------------------------

(defrule propuestaVenderSobrevaloradas
    (obtenerPropuestas)
    (Sobrevalorado ?nombre ?explicacion)
    (Cartera (Nombre ?nombre) (ValorActual ?v))
    (Empresa 
        (Nombre ?nombre)
        (RPD ?RPD)
        (PER ?PER)
        (Sector ?nombreSector) 
        (Porcentaje_Var12meses ?varAnual)
    )
    (Sector 
        (Nombre ?nombreSector)
        (PER ?PERMedio)
    )
    (test (< (+ ?varAnual (* 100 ?RPD)) (+ ?v 5)))
    =>
    (assert (Propuesta
        (Tipo Vender_Sobrevalorado)
        (Nombre1 ?nombre)
        (Nombre2 NA)
        (RE (+ (* ?RPD -100) (/ (- ?PER ?PERMedio) (* 5 ?PER))))
        (Explicacion (str-cat ?nombre " esta sobrevalorada, es mejor amortizar"
            " lo invertido, ya que seguramente el PER tan alto debera bajar al"
            " PER medio del sector en unos 5 anios, con lo que se deberia "
            "devaluar un "(/ (- ?PER ?PERMedio) (* 5 ?PER))" anual, asi que aunque"
            " se pierda el  RPD% de beneficios por dividendos  saldria rentable")
            )
    ))
)

;----------------------------------------------------------------------
;   Es la regla encargada de proponer cambiar una inversion a valores 
;   mas rentables. Si una empresa (empresa1) no esta sobrevalorada y  
;   su RPD  es mayor que el (revalorizacion por semestre + RPD+1) de 
;   una empresa de mi cartera (empresa 2) que no esta infravalorada, 
;   proponer cambiar las acciones de una empresa por las de la otra 
;       RE= (RPD empresa1 - (rendimiento por anio obtenido  empresa2 
;                    + rdp empresa2 +1)
;----------------------------------------------------------------------

(defrule propuestaCambiarAMasRentable
    (obtenerPropuestas)
    (Empresa (Nombre ?nombre1) (RPD ?rpd1) (Porcentaje_VarSem ?varSem1))
    (not (Sobrevalorado ?nombre1 ?explicacion))
    (not (Cartera (Nombre ?nombre1)))
    (Empresa (Nombre ?nombre2) (RPD ?rpd2) (Porcentaje_VarSem ?varSem2))
    (Cartera (Nombre ?nombre2))
    (not (Infravalorado ?nombre2 ?explicacion))

    (test (< (+ (* ?rpd2 100) ?varSem2 1) (* ?rpd1 100)))
    =>
    (assert (Propuesta
        (Tipo IntercambiarAMasRentable)
        (Nombre1 ?nombre1)
        (Nombre2 ?nombre2)
        (RE (- (* ?rpd1 100) (+ ?varSem2 (* ?rpd2 100) 1)))
        (Explicacion (str-cat ?nombre1 " debe tener una revalorizacion acorde con la evolucion de la bolsa. Por dividendos se espera un " 
            (* ?rpd1 100) "% que es mas de lo que te esta dando " ?nombre2 
            ", por eso te propongo cambiar los valores por los de esta otra " 
            (+ ?varSem2 (* ?rpd2 100)) 
            ". Aunque se pague el 1% del coste del cambio te saldria rentable")
            )
    ))
)

;----------------------------------------------------------------------
;   Finaliza el módulo de obtención de propuestas y da paso al 
;   módulo de presentación de propuestas
;----------------------------------------------------------------------

(defrule finPropuestas
    (declare (salience -10))
    ?f <- (obtenerPropuestas)
    =>
    (retract ?f)
    (assert (mostrarPropuestas))
)


;##############################################################################
;#           MODULO PARA MOSTRAR LAS MEJORES PROPUESTAS OBTENIDAS             #
;##############################################################################

;----------------------------------------------------------------------
;   Inicializa el modulo. Esta regla inserta el hecho que indica buscar
;   las mejores propuestas, e inicializa el maximo valor con un hecho
;----------------------------------------------------------------------


(defrule mostrarMejoresPropuestas
    ?f <- (mostrarPropuestas)
    =>
    (retract ?f)
    (assert (buscarMejores))
    (assert (propuestasMostradas 0))
    (assert (Maximo -99999999.0))
    
)

;----------------------------------------------------------------------
; Regla que calcula la mejor propuesta
;----------------------------------------------------------------------

(defrule mejorPropuesta
    (declare (salience 5))
    (buscarMejores)
    (Propuesta
        (Tipo ?t)
        (Nombre1 ?n)
        (Nombre2 ?n2)
        (RE ?RE)
        (Explicacion ?exp)
    )
    (Maximo ?m)
    (propuestasMostradas ?p)
    ?r <- (Maximo ?m)
    (test (> ?RE ?m))
    (test (< ?p 5))
    =>
    (retract ?r)
    (assert (Maximo ?RE))
)

;----------------------------------------------------------------------
;   Regla que se encarga de mostrar las propuestas por pantalla
;   
;   En estas dos reglas, el uso del salience está justificado a 
;   que el experto dijo mostrar las propuestas de mayor a menor
;   Rendimiento Esperado y por ello, se usa el salience para la 
;   implementación de este conocimiento
;----------------------------------------------------------------------

(defrule imprimirPropuestas
    (declare (salience 3))
    (buscarMejores)
    ?p <- (propuestasMostradas ?n)
    ?max <- (Maximo ?m)
    ?propuesta <- (Propuesta 
        (Tipo ?t) 
        (Nombre1 ?nombre1) 
        (Nombre2 ?nombre2) 
        (RE ?m) 
        (Explicacion ?ex)
    )
    (test (< ?n 5))
    =>
    (if (eq ?t IntercambiarAMasRentable) then
        (printout t crlf (+ ?n 1) " Propongo intercambiar las acciones de " ?nombre2
            " por acciones de " ?nombre1 " porque " ?ex crlf "-- RENDIMIENTO ESPERADO ==> "
            ?m " --" crlf)
        (assert (Intercambiar ?nombre2 ?nombre1 ?m (+ ?n 1)))
        else 
            (if (eq ?t ComprarInfravalorado) then
                (printout t crlf (+ ?n 1) " Propongo comprar acciones de " ?nombre1
                    " porque " ?ex crlf "-- RENDIMIENTO ESPERADO ==> "
                    ?m " --" crlf)
                (assert (ComprarInfravalorado ?nombre1 ?m (+ ?n 1)))

                else
                    (if (eq ?t Vender_Peligroso) then
                        (printout t crlf (+ ?n 1) " Propongo vender acciones de " ?nombre1
                            " porque " ?ex crlf "-- RENDIMIENTO ESPERADO ==> " ?m " --" crlf)

                        (assert (Vender ?nombre1 ?m (+ ?n 1)))

                        else
                            (if (eq ?t Vender_Sobrevalorado) then
                                (printout t crlf (+ ?n 1) " Propongo vender acciones de " ?nombre1
                                    " porque " ?ex crlf "-- RENDIMIENTO ESPERADO ==> " ?m " --" crlf)

                                    (assert (Vender ?nombre1 ?m (+ ?n 1)))
                            )
                        
                    )
                
            )
    )
    (retract ?max)
    (retract ?propuesta)
    (assert (Maximo -99999999.0))
    (retract ?p)
    (assert (propuestasMostradas (+ ?n 1)))
)

;----------------------------------------------------------------------
;   Regla que se encarga de obtener la decisión del usuario
;----------------------------------------------------------------------

(defrule preguntarPorPropuestas
    (declare (salience 2))
    ?f <- (buscarMejores)
    (propuestasMostradas ?pM)
    (test (<= ?pM 5))
    =>
    (printout t (+ ?pM 1) " Para aplicar alguna propuesta selecciona un numero" crlf)
    (printout t "O pulsar " (+ ?pM 1) " para salir" crlf)
    (printout t "Desea aplicar alguna de estas propuestas?: " crlf)
    (bind ?propuesta (read))
    (retract ?f)
    (assert (eleccionUsuario ?propuesta))
)

;----------------------------------------------------------------------
;   Regla que se encarga de realizar la venta del valor mostrado 
;   en la salida por pantalla y que ha seleccionado el usuario
;----------------------------------------------------------------------

(defrule aplicarVenta

    ?eleccion <- (eleccionUsuario ?e)
    ?venta <- (Vender ?nombre ?RE ?e)
    ?dineroDisponible <- (Cartera (Nombre DISPONIBLE) (Acciones ?a) (ValorActual ?v))
    ?valorEmpresa <- (Cartera (Nombre ?nombre) (Acciones ?nA) (ValorActual ?vEmp))
    (test (< ?e 6))
    (test (> ?e 0))
    =>
    (assert (Cartera 
        (Nombre DISPONIBLE) 
        (Acciones (+ ?a ?vEmp))
        (ValorActual (+ ?v ?vEmp))))
    (retract ?venta)
    (retract ?dineroDisponible)
    (retract ?valorEmpresa)
    (retract ?eleccion)
    (printout t crlf "###############################################################################" crlf crlf)
    (printout t "Todas las acciones de la empresa " ?nombre " han sido vendidas" crlf)
    (printout t "El dinero disponible actual es de " (+ ?v ?vEmp) crlf)
    (printout t crlf "###############################################################################" crlf crlf)
    (assert (ActualizarPropuestas))
)

;----------------------------------------------------------------------
;   Regla que se encarga de intercambiar las acciones de un valor 
;   por el otro mostrado en la salida
;----------------------------------------------------------------------

(defrule intercambiarValores
    ?eleccion <- (eleccionUsuario ?e)
    ?intercambio <- (Intercambiar ?miEmpresa ?nuevaEmpresa ?RE ?e)
    ?valorEmpresa <- (Cartera (Nombre ?miEmpresa) (Acciones ?a) (ValorActual ?v))
    ?dineroDisponible <- (Cartera (Nombre DISPONIBLE) (Acciones ?aCash) (ValorActual ?vCash))
    (Empresa (Nombre ?nuevaEmpresa) (Precio ?p))
    (test (< ?e 6))
    (test (> ?e 0))
    =>
    (printout t crlf "###############################################################################" crlf crlf)
    (assert (Cartera (Nombre ?nuevaEmpresa) (Acciones ?a) (ValorActual (* ?p ?a))))
    (assert (Cartera 
            (Nombre DISPONIBLE) 
            (Acciones (- (+ ?aCash ?v) (* ?p ?a))) 
            (ValorActual (- (+ ?vCash ?v) (* ?p ?a))) 
        )
    )
    (printout t "Todas las acciones de " ?miEmpresa " se han cambiado por las de " ?nuevaEmpresa crlf)
    (printout t "El dinero disponible actual es de " (- (+ ?vCash ?v) (* ?p ?a)) crlf)
    (printout t crlf "###############################################################################" crlf crlf)
    (retract ?eleccion)
    (retract ?intercambio)
    (retract ?valorEmpresa)
    (retract ?dineroDisponible)
    (assert (ActualizarPropuestas))

)

;----------------------------------------------------------------------
;   Regla que se encarga de comprar el valor elegido
;----------------------------------------------------------------------

(defrule comprarValor
    ?eleccion <- (eleccionUsuario ?e)
    ?compra <- (ComprarInfravalorado ?nombre ?RE ?e)
    ?dineroDisponible <- (Cartera (Nombre DISPONIBLE) (Acciones ?aCash) (ValorActual ?vCash))
    (Empresa (Nombre ?nombre) (Precio ?precio))
    (test (< ?e 6))
    (test (> ?e 0))
    =>
    (printout t crlf "###############################################################################" crlf crlf)
    (printout t "Al realizar una compra de valores, recomiendo invertir un 5% del dinero en efectivo, "
        " para evitar tenerlo todo invertido en un mismo valor." crlf)
    (assert (Cartera 
            (Nombre ?nombre) 
            (Acciones (* ?vCash 0.05)) 
            (ValorActual (* ?precio (* ?vCash 0.05))) 
        )
    )
    (printout T "Se ha añadido " ?nombre " a la cartera con un total de " (* ?vCash 0.05)
        " acciones con un valor de " (* ?precio (* ?vCash 0.05)) crlf)
    (retract ?eleccion)
    (retract ?compra)
    (retract ?dineroDisponible)
    (assert (Cartera 
            (Nombre DISPONIBLE) 
            (Acciones (- ?aCash (* ?precio (* ?vCash 0.05)))) 
            (ValorActual (- ?vCash (* ?precio (* ?vCash 0.05)))) 
        )
    )
    (printout t "El dinero disponible actual es de " (- ?vCash (* ?precio (* ?vCash 0.05))) crlf)
    (printout t crlf "###############################################################################" crlf crlf)
    (assert (ActualizarPropuestas))
)

;----------------------------------------------------------------------
;   Regla que se encarga de actualizar las propuestas del sistema en
;   función de la propuesta elegida
;----------------------------------------------------------------------

(defrule actualizarPropuestasDelSistema
    (declare (salience -10))
    ?f <-(ActualizarPropuestas)
    (or ?hecho <- (Propuesta (Tipo ?t) (Nombre1 ?n1) (Nombre2 ?n2) (RE ?re) (Explicacion ?ex))
        ?hecho <- (Vender ?nombre ?RE ?e)
        ?hecho <- (Intercambiar ?emp1 ?emp2 ?RE ?exp)
        ?hecho <- (ComprarInfravalorado ?nombre ?RE ?e)
        ?hecho <- (Peligroso ?nombre ?explicacion)
        ?hecho <- (propuestasMostradas ?n)
    )
    => 
    (retract ?hecho)
)

;----------------------------------------------------------------------
;   Esta regla reinicia el ciclo principal del programa, haciendo que 
;   el sistema vuelva a obtener los valores peligrosos, las posibles 
;   propuestas, y hará que se vuelvan a mostrar al usuario.
;----------------------------------------------------------------------

(defrule ciclar
    (declare (salience -15))
    ?f <-(ActualizarPropuestas)
    =>
    (retract ?f)
    (assert (moduloValoresPeligrosos))
)

;----------------------------------------------------------------------
;   Esta regla finaliza el sistema
;----------------------------------------------------------------------

(defrule finalizarSistema
    (propuestasMostradas ?pm)
    ?elect <- (eleccionUsuario ?e)
    (or (test (> ?e ?pm))
        (test (<= ?e 0))
        )
    =>
    (printout t "Finalizando el sistema" crlf "¡Hasta otra!" crlf)
    (retract ?elect)
)
