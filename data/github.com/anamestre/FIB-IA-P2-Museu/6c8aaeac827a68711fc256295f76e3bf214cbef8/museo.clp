; Tue Nov 29 17:18:57 CET 2016
; 
;+ (version "3.5")
;+ (build "Build 663")

;;Ontologia --------------------------------------------------------------------

(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(single-slot Nombre_tematica
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Epoca_cuadro
		(type INSTANCE)
;+		(allowed-classes Epoca)
;+		(cardinality 0 1)
;+		(inverse-slot Cuadros_epoca)
		(create-accessor read-write))
	(single-slot Pintado_por
		(type INSTANCE)
;+		(allowed-classes Pintor)
;+		(cardinality 0 1)
;+		(inverse-slot Ha_pintado)
		(create-accessor read-write))
	(multislot Tematica_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(inverse-slot Pref_tematica)
		(create-accessor read-write))
	(multislot Epocas_pintor
		(type INSTANCE)
;+		(allowed-classes Epoca)
;+		(inverse-slot Pintores_epoca)
		(create-accessor read-write))
	(single-slot Titulo
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pref_epoca
		(type INSTANCE)
;+		(allowed-classes Epoca)
;+		(inverse-slot Epoca_pref)
		(create-accessor read-write))
	(single-slot Nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Epoca_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(inverse-slot Pref_epoca)
		(create-accessor read-write))
	(single-slot Conocimiento_visita
		(type INSTANCE)
;+		(allowed-classes Conocimiento)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Descripcion
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Tamanyo_visita
		(type INSTANCE)
;+		(allowed-classes Tamanyo)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot h
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Ha_pintado
		(type INSTANCE)
;+		(allowed-classes Cuadro)
;+		(inverse-slot Pintado_por)
		(create-accessor read-write))
	(single-slot Estilo_cuadro
		(type INSTANCE)
;+		(allowed-classes Estilo)
;+		(cardinality 0 1)
;+		(inverse-slot Cuadros_estilo)
		(create-accessor read-write))
	(multislot Cuadros_tematica
		(type INSTANCE)
;+		(allowed-classes Cuadro)
;+		(inverse-slot Tematica_cuadro)
		(create-accessor read-write))
	(multislot Pref_estilo
		(type INSTANCE)
;+		(allowed-classes Estilo)
;+		(inverse-slot Estilo_pref)
		(create-accessor read-write))
	(multislot Estilo_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(inverse-slot Pref_estilo)
		(create-accessor read-write))
	(single-slot w
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pintor_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(inverse-slot Pref_pintor)
		(create-accessor read-write))
	(single-slot T%C3%ADtol
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Complejidad
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pref_visita
		(type INSTANCE)
;+		(allowed-classes Visita)
;+		(inverse-slot Visita_pref)
		(create-accessor read-write))
	(single-slot Estil
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Relevancia
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pref_pintor
		(type INSTANCE)
;+		(allowed-classes Pintor)
;+		(inverse-slot Pintor_pref)
		(create-accessor read-write))
	(multislot Pintores_estilo
		(type INSTANCE)
;+		(allowed-classes Pintor)
;+		(inverse-slot Estilos_pintor)
		(create-accessor read-write))
	(single-slot Visita_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(cardinality 0 1)
;+		(inverse-slot Pref_visita)
		(create-accessor read-write))
	(multislot Cuadros_epoca
		(type INSTANCE)
;+		(allowed-classes Cuadro)
;+		(inverse-slot Epoca_cuadro)
		(create-accessor read-write))
	(single-slot Tem%C3%A0tica
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Dim
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Autor
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot %C3%88poca
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Anyo
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot HorasDia
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Tematica_cuadro
		(type INSTANCE)
;+		(allowed-classes Tematica)
;+		(cardinality 0 1)
;+		(inverse-slot Cuadros_tematica)
		(create-accessor read-write))
	(multislot Cuadros_estilo
		(type INSTANCE)
;+		(allowed-classes Cuadro)
;+		(inverse-slot Estilo_cuadro)
		(create-accessor read-write))
	(single-slot Complexitat
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Dias
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Estilos_pintor
		(type INSTANCE)
;+		(allowed-classes Estilo)
;+		(inverse-slot Pintores_estilo)
		(create-accessor read-write))
	(single-slot Nombre_estilo
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nivel
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nacionalidad
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pintores_epoca
		(type INSTANCE)
;+		(allowed-classes Pintor)
;+		(inverse-slot Epocas_pintor)
		(create-accessor read-write))
	(single-slot Nombre_epoca
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Any
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pref_tematica
		(type INSTANCE)
;+		(allowed-classes Tematica)
;+		(inverse-slot Tematica_pref)
		(create-accessor read-write))
	(single-slot Sala
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Cuadro
	(is-a USER)
	(role concrete)
	(single-slot Dim
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Epoca_cuadro
		(type INSTANCE)
;+		(allowed-classes Epoca)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Complejidad
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Pintado_por
		(type INSTANCE)
;+		(allowed-classes Pintor)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Anyo
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Relevancia
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Titulo
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Sala
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Tematica_cuadro
		(type INSTANCE)
;+		(allowed-classes Tematica)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Estilo_cuadro
		(type INSTANCE)
;+		(allowed-classes Estilo)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Pintor
	(is-a USER)
	(role concrete)
	(single-slot Nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Estilos_pintor
		(type INSTANCE)
;+		(allowed-classes Estilo)
		(create-accessor read-write))
	(multislot Epocas_pintor
		(type INSTANCE)
;+		(allowed-classes Epoca)
		(create-accessor read-write))
	(multislot Ha_pintado
		(type INSTANCE)
;+		(allowed-classes Cuadro)
		(create-accessor read-write))
	(single-slot Nacionalidad
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pintor_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(create-accessor read-write)))

(defclass Epoca
	(is-a USER)
	(role concrete)
	(multislot Pintores_epoca
		(type INSTANCE)
;+		(allowed-classes Pintor)
		(create-accessor read-write))
	(multislot Epoca_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(create-accessor read-write))
	(single-slot Nombre_epoca
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Cuadros_epoca
		(type INSTANCE)
;+		(allowed-classes Cuadro)
		(create-accessor read-write)))

(defclass Estilo
	(is-a USER)
	(role concrete)
	(multislot Cuadros_estilo
		(type INSTANCE)
;+		(allowed-classes Cuadro)
		(create-accessor read-write))
	(multislot Estilo_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(create-accessor read-write))
	(single-slot Nombre_estilo
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Pintores_estilo
		(type INSTANCE)
;+		(allowed-classes Pintor)
		(create-accessor read-write)))

(defclass Tematica
	(is-a USER)
	(role concrete)
	(single-slot Nombre_tematica
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Cuadros_tematica
		(type INSTANCE)
;+		(allowed-classes Cuadro)
		(create-accessor read-write))
	(multislot Tematica_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
		(create-accessor read-write)))

(defclass Visita
	(is-a USER)
	(role concrete)
	(single-slot Dias
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Conocimiento_visita
		(type INSTANCE)
;+		(allowed-classes Conocimiento)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Tamanyo_visita
		(type INSTANCE)
;+		(allowed-classes Tamanyo)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Visita_pref
		(type INSTANCE)
;+		(allowed-classes Preferencia)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot HorasDia
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Tamanyo
	(is-a USER)
	(role concrete)
	(single-slot Descripcion
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Conocimiento
	(is-a USER)
	(role concrete)
	(single-slot Nivel
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Preferencia
	(is-a USER)
	(role concrete)
	(multislot Pref_pintor
		(type INSTANCE)
;+		(allowed-classes Pintor)
		(create-accessor read-write))
	(multislot Pref_estilo
		(type INSTANCE)
;+		(allowed-classes Estilo)
		(create-accessor read-write))
	(multislot Pref_visita
		(type INSTANCE)
;+		(allowed-classes Visita)
		(create-accessor read-write))
	(multislot Pref_tematica
		(type INSTANCE)
;+		(allowed-classes Tematica)
		(create-accessor read-write))
	(multislot Pref_epoca
		(type INSTANCE)
;+		(allowed-classes Epoca)
		(create-accessor read-write)))


;;Instancias --------------------------------------------------------------------

(definstances instancias
; Tue Nov 29 17:54:23 CET 2016
; 
;+ (version "3.5")
;+ (build "Build 663")

([Cuadres_Class1] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class18]
		[Cuadres_Class82]
		[Cuadres_Class85]
		[Cuadres_Class86]
		[Cuadres_Class96]
		[Cuadres_Class114]
		[Cuadres_Class115]
		[Cuadres_Class116])
	(Nombre_epoca "Barroco")
	(Pintores_epoca
		[Cuadres_Class16]
		[Cuadres_Class50]
		[Cuadres_Class65]))

([Cuadres_Class10] of  Estilo

	(Cuadros_estilo [Cuadres_Class109])
	(Nombre_estilo "Cubismo")
	(Pintores_estilo [Cuadres_Class60]))

([Cuadres_Class100] of  Cuadro

	(Anyo 1801)
	(Complejidad 48841)
	(Dim "260 x 221")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class31])
	(Pintado_por [Cuadres_Class54])
	(Relevancia 4)
	(Sala 4)
	(Tematica_cuadro [Cuadres_Class22])
	(Titulo "Napoleon cruzando los Alpes"))

([Cuadres_Class101] of  Cuadro

	(Anyo 1603)
	(Complejidad 361)
	(Dim "19 x 26")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class55])
	(Relevancia 1)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Camino al mercado"))

([Cuadres_Class102] of  Cuadro

	(Anyo 1434)
	(Complejidad 3600)
	(Dim "82 x 60")
	(Epoca_cuadro [Cuadres_Class2])
	(Estilo_cuadro [Cuadres_Class15])
	(Pintado_por [Cuadres_Class56])
	(Relevancia 3)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "El matrimonio Arnolfini"))

([Cuadres_Class103] of  Cuadro

	(Anyo 1425)
	(Complejidad 361)
	(Dim "56 x 19")
	(Epoca_cuadro [Cuadres_Class2])
	(Estilo_cuadro [Cuadres_Class15])
	(Pintado_por [Cuadres_Class56])
	(Relevancia 4)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "La crucifixion"))

([Cuadres_Class104] of  Cuadro

	(Anyo 1590)
	(Complejidad 256036)
	(Dim "800 x 506")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class57])
	(Relevancia 1)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Concert"))

([Cuadres_Class105] of  Cuadro

	(Anyo 1502)
	(Complejidad 2809)
	(Dim "77 x 53")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class9])
	(Pintado_por [Cuadres_Class58])
	(Relevancia 8)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Mona Lisa"))

([Cuadres_Class106] of  Cuadro

	(Anyo 1510)
	(Complejidad 78400)
	(Dim "280 x 570")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class9])
	(Pintado_por [Cuadres_Class59])
	(Relevancia 6)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "Creación de Adan"))

([Cuadres_Class107] of  Cuadro

	(Anyo 1901)
	(Complejidad 3721)
	(Dim "83 x 61")
	(Epoca_cuadro [Cuadres_Class7])
	(Estilo_cuadro [Cuadres_Class39])
	(Pintado_por [Cuadres_Class60])
	(Relevancia 4)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Arlequin pensativo"))

([Cuadres_Class108] of  Cuadro

	(Anyo 1937)
	(Complejidad 121801)
	(Dim "349 x 777")
	(Epoca_cuadro [Cuadres_Class7])
	(Estilo_cuadro [Cuadres_Class14])
	(Pintado_por [Cuadres_Class60])
	(Relevancia 8)
	(Sala 4)
	(Tematica_cuadro [Cuadres_Class26])
	(Titulo "Gernika"))

([Cuadres_Class109] of  Cuadro

	(Anyo 1907)
	(Complejidad 54756)
	(Dim "234 x 234")
	(Epoca_cuadro [Cuadres_Class7])
	(Estilo_cuadro [Cuadres_Class10])
	(Pintado_por [Cuadres_Class60])
	(Relevancia 5)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class21])
	(Titulo "Las senoritas de Avignon"))

([Cuadres_Class11] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class82]
		[Cuadres_Class86]
		[Cuadres_Class96]
		[Cuadres_Class85])
	(Nombre_estilo "Escuela espanola")
	(Pintores_estilo
		[Cuadres_Class16]
		[Cuadres_Class50]))

([Cuadres_Class110] of  Cuadro

	(Anyo 1896)
	(Complejidad 3721)
	(Dim "61 x 82")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class35])
	(Pintado_por [Cuadres_Class60])
	(Relevancia 1)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Una cantera"))

([Cuadres_Class111] of  Cuadro

	(Anyo 1595)
	(Complejidad 355216)
	(Dim "800 x 596")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class61])
	(Relevancia 0)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "A Forest Pool"))

([Cuadres_Class112] of  Cuadro

	(Anyo 1866)
	(Complejidad 900)
	(Dim "30 x 41")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class35])
	(Pintado_por [Cuadres_Class62])
	(Relevancia 4)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class20])
	(Titulo "Naturaleza muerta"))

([Cuadres_Class113] of  Cuadro

	(Anyo 1509)
	(Complejidad 250000)
	(Dim "500 770")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class9])
	(Pintado_por [Cuadres_Class63])
	(Relevancia 2)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class22])
	(Titulo "La escuela de Atenas"))

([Cuadres_Class114] of  Cuadro

	(Anyo 1632)
	(Complejidad 2025)
	(Dim "61 x 45")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class13])
	(Pintado_por [Cuadres_Class64])
	(Relevancia 1)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Albert Cuyper"))

([Cuadres_Class115] of  Cuadro

	(Anyo 1658)
	(Complejidad 379456)
	(Dim "616 x 800")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class13])
	(Pintado_por [Cuadres_Class64])
	(Relevancia 2)
	(Sala 2)
    (Tematica_cuadro [Cuadres_Class24])
	(Titulo "Autoretrato con baston"))

([Cuadres_Class116] of  Cuadro

	(Anyo 1636)
	(Complejidad 32761)
	(Dim "221 x 181")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class12])
	(Pintado_por [Cuadres_Class65])
	(Relevancia 4)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Las tres gracias"))

([Cuadres_Class117] of  Cuadro

	(Anyo 1484)
	(Complejidad 29929)
	(Dim "279 x 173")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class9])
	(Pintado_por [Cuadres_Class66])
	(Relevancia 7)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "El nacimiento de Venus"))

([Cuadres_Class118] of  Cuadro

	(Anyo 1895)
	(Complejidad 19600)
	(Dim "140 x 184")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class39])
	(Pintado_por [Cuadres_Class67])
	(Relevancia 0)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Alegoria de la Poesia"))

([Cuadres_Class119] of  Cuadro

	(Anyo 1894)
	(Complejidad 6241)
	(Dim "88 x 79")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class30])
	(Pintado_por [Cuadres_Class67])
	(Relevancia 0)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Maria Rusinol en el Cau Ferrat"))

([Cuadres_Class12] of  Estilo

	(Cuadros_estilo [Cuadres_Class116])
	(Nombre_estilo "Escuela flamenca")
	(Pintores_estilo [Cuadres_Class65]))

([Cuadres_Class120] of  Cuadro

	(Anyo 1536)
	(Complejidad 339889)
	(Dim "583 x 800")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class71])
	(Relevancia 1)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "A Beauty"))

([Cuadres_Class121] of  Cuadro

	(Anyo 1580)
	(Complejidad 297025)
	(Dim "800 x 545")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class70])
	(Relevancia 0)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Chicken Vendors"))

([Cuadres_Class122] of  Cuadro

	(Anyo 1889)
	(Complejidad 2916)
	(Dim "54 x 64")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class32])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 0)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Angel"))

([Cuadres_Class123] of  Cuadro

	(Anyo 1889)
	(Complejidad 5625)
	(Dim "75 x 94")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class32])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 1)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Anochecer"))

([Cuadres_Class124] of  Cuadro

	(Anyo 1887)
	(Complejidad 529)
	(Dim "32 x 23")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 7)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Autoretrato"))

([Cuadres_Class125] of  Cuadro

	(Anyo 1888)
	(Complejidad 5041)
	(Dim "71 x 95")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class32])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 0)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Barcazas de carbon"))

([Cuadres_Class126] of  Cuadro

	(Anyo 1887)
	(Complejidad 220900)
	(Dim "800 x 470")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 1)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class21])
	(Titulo "Desnudo femenino echado"))

([Cuadres_Class127] of  Cuadro

	(Anyo 1888)
	(Complejidad 5184)
	(Dim "72 x 90")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class32])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 4)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class23])
	(Titulo "La habitación de Vincent en Arles"))

([Cuadres_Class128] of  Cuadro

	(Anyo 1885)
	(Complejidad 1024)
	(Dim "32 x 40")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class35])
	(Pintado_por [Cuadres_Class69])
	(Relevancia 1)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Paseo con dos figuras"))

([Cuadres_Class129] of  Cuadro

	(Anyo 1873)
	(Complejidad 4225)
	(Dim "65 x 76")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class35])
	(Pintado_por [Cuadres_Class68])
	(Relevancia 0)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "A casa do pintor en Presqueiras"))

([Cuadres_Class13] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class114]
		[Cuadres_Class115])
	(Nombre_estilo "Escuela holandesa")
	(Pintores_estilo [Cuadres_Class64]))

([Cuadres_Class130] of  Tamanyo

	(Descripcion "Individuo"))

([Cuadres_Class131] of  Tamanyo

	(Descripcion "Pareja"))

([Cuadres_Class132] of  Tamanyo

	(Descripcion "Grupo pequeno (3-12)"))

([Cuadres_Class133] of  Tamanyo

	(Descripcion "Grupo mediano (13-25)"))

([Cuadres_Class134] of  Tamanyo

	(Descripcion "Grupo grande (+25)"))

([Cuadres_Class135] of  Tamanyo

	(Descripcion "Familia"))

([Cuadres_Class136] of  Conocimiento

	(Nivel 1))

([Cuadres_Class137] of  Conocimiento

	(Nivel 2))

([Cuadres_Class138] of  Conocimiento

	(Nivel 3))

([Cuadres_Class139] of  Conocimiento

	(Nivel 4))

([Cuadres_Class14] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class88]
		[Cuadres_Class108])
	(Nombre_estilo "Expresionismo")
	(Pintores_estilo
		[Cuadres_Class45]
		[Cuadres_Class60]))

([Cuadres_Class140] of  Conocimiento

	(Nivel 5))

([Cuadres_Class141] of  Conocimiento

	(Nivel 6))

([Cuadres_Class142] of  Conocimiento

	(Nivel 7))

([Cuadres_Class143] of  Conocimiento

	(Nivel 0))

([Cuadres_Class15] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class89]
		[Cuadres_Class102]
		[Cuadres_Class103])
	(Nombre_estilo "Gotico flamenco")
	(Pintores_estilo
		[Cuadres_Class46]
		[Cuadres_Class48]
		[Cuadres_Class56]))

([Cuadres_Class16] of  Pintor

	(Epocas_pintor [Cuadres_Class1])
	(Estilos_pintor
		[Cuadres_Class8]
		[Cuadres_Class11])
	(Ha_pintado
		[Cuadres_Class18]
		[Cuadres_Class82]
		[Cuadres_Class85]
		[Cuadres_Class86])
	(Nacionalidad "espanol")
	(Nombre "Diego Velazquez"))

([Cuadres_Class17] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class18]
		[Cuadres_Class75]
		[Cuadres_Class86]
		[Cuadres_Class93]
		[Cuadres_Class97]
		[Cuadres_Class105]
		[Cuadres_Class107]
		[Cuadres_Class120]
		[Cuadres_Class124])
	(Nombre_tematica "Retrato"))

([Cuadres_Class18] of  Cuadro

	(Anyo 1643)
	(Complejidad 4356)
	(Dim "66 x  80")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class8])
	(Pintado_por [Cuadres_Class16])
	(Relevancia 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Autorretrato"))

([Cuadres_Class19] of  Tematica

	(Cuadros_tematica [Cuadres_Class99])
	(Nombre_tematica "Amor"))

([Cuadres_Class2] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class89]
		[Cuadres_Class102]
		[Cuadres_Class103])
	(Nombre_epoca "Gotico")
	(Pintores_epoca
		[Cuadres_Class46]
		[Cuadres_Class48]
		[Cuadres_Class56]
		[Cuadres_Class64]))

([Cuadres_Class20] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class96]
		[Cuadres_Class112])
	(Nombre_tematica "Bodegon"))

([Cuadres_Class21] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class109]
		[Cuadres_Class126])
	(Nombre_tematica "Desnudo"))

([Cuadres_Class22] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class94]
		[Cuadres_Class100]
		[Cuadres_Class113])
	(Nombre_tematica "Historia"))

([Cuadres_Class23] of  Tematica

	(Cuadros_tematica [Cuadres_Class127])
	(Nombre_tematica "Interior"))

([Cuadres_Class24] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class76]
		[Cuadres_Class82]
		[Cuadres_Class85]
		[Cuadres_Class91]
		[Cuadres_Class92]
		[Cuadres_Class95]
		[Cuadres_Class114]
		[Cuadres_Class116]
		[Cuadres_Class117]
		[Cuadres_Class118])
	(Nombre_tematica "Mitologia y alegorias"))

([Cuadres_Class25] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class77]
		[Cuadres_Class78]
		[Cuadres_Class79]
		[Cuadres_Class90]
		[Cuadres_Class98]
		[Cuadres_Class101]
		[Cuadres_Class110]
		[Cuadres_Class111]
		[Cuadres_Class128]
		[Cuadres_Class129])
	(Nombre_tematica "Paisajes y vistas"))

([Cuadres_Class26] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class88]
		[Cuadres_Class108])
	(Nombre_tematica "Protesta social"))

([Cuadres_Class27] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class72]
		[Cuadres_Class74]
		[Cuadres_Class87]
		[Cuadres_Class89]
		[Cuadres_Class103]
		[Cuadres_Class106])
	(Nombre_tematica "Religion"))

([Cuadres_Class28] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class77]
		[Cuadres_Class78]
		[Cuadres_Class79]
		[Cuadres_Class80]
		[Cuadres_Class124]
		[Cuadres_Class126])
	(Nombre_estilo "Impresionismo")
	(Pintores_estilo
		[Cuadres_Class43]
		[Cuadres_Class69]))

([Cuadres_Class29] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class74]
		[Cuadres_Class75]
		[Cuadres_Class76]
		[Cuadres_Class87]
		[Cuadres_Class91]
		[Cuadres_Class101]
		[Cuadres_Class104]
		[Cuadres_Class111]
		[Cuadres_Class120]
		[Cuadres_Class121])
	(Nombre_estilo "Manierismo")
	(Pintores_estilo
		[Cuadres_Class40]
		[Cuadres_Class41]
		[Cuadres_Class42]
		[Cuadres_Class44]
		[Cuadres_Class55]
		[Cuadres_Class57]
		[Cuadres_Class61]
		[Cuadres_Class71]
		[Cuadres_Class70]))

([Cuadres_Class3] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class74]
		[Cuadres_Class75]
		[Cuadres_Class76]
		[Cuadres_Class87]
		[Cuadres_Class101]
		[Cuadres_Class104]
		[Cuadres_Class105]
		[Cuadres_Class106]
		[Cuadres_Class111]
		[Cuadres_Class113]
		[Cuadres_Class117]
		[Cuadres_Class120]
		[Cuadres_Class121])
	(Nombre_epoca "Renacimiento")
	(Pintores_epoca
		[Cuadres_Class40]
		[Cuadres_Class41]
		[Cuadres_Class42]
		[Cuadres_Class44]
		[Cuadres_Class55]
		[Cuadres_Class57]
		[Cuadres_Class58]
		[Cuadres_Class59]
		[Cuadres_Class61]
		[Cuadres_Class63]
		[Cuadres_Class66]
		[Cuadres_Class70]))

([Cuadres_Class30] of  Estilo

	(Cuadros_estilo [Cuadres_Class119])
	(Nombre_estilo "Modernismo")
	(Pintores_estilo [Cuadres_Class67]))

([Cuadres_Class31] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class93]
		[Cuadres_Class95]
		[Cuadres_Class100])
	(Nombre_estilo "Neoclasicismo")
	(Pintores_estilo
		[Cuadres_Class49]
		[Cuadres_Class54]))

([Cuadres_Class32] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class122]
		[Cuadres_Class123]
		[Cuadres_Class125]
		[Cuadres_Class127])
	(Nombre_estilo "Postimpresionismo")
	(Pintores_estilo [Cuadres_Class69]))

([Cuadres_Class33] of  Estilo

	(Cuadros_estilo [Cuadres_Class94])
	(Nombre_estilo "Prerromanticismo")
	(Pintores_estilo [Cuadres_Class49]))

([Cuadres_Class34] of  Estilo

	(Cuadros_estilo [Cuadres_Class98])
	(Nombre_estilo "Puntillismo")
	(Pintores_estilo [Cuadres_Class51]))

([Cuadres_Class35] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class90]
		[Cuadres_Class110]
		[Cuadres_Class112]
		[Cuadres_Class128]
		[Cuadres_Class129])
	(Nombre_estilo "Realismo")
	(Pintores_estilo
		[Cuadres_Class47]
		[Cuadres_Class60]
		[Cuadres_Class62]
		[Cuadres_Class68]
		[Cuadres_Class69]))

([Cuadres_Class36] of  Estilo

	(Cuadros_estilo [Cuadres_Class97])
	(Nombre_estilo "Regionalismo")
	(Pintores_estilo [Cuadres_Class52]))

([Cuadres_Class37] of  Estilo

	(Cuadros_estilo [Cuadres_Class72])
	(Nombre_estilo "Romanico")
	(Pintores_estilo [Cuadres_Class73]))

([Cuadres_Class38] of  Estilo

	(Cuadros_estilo [Cuadres_Class92])
	(Nombre_estilo "Romanticismo")
	(Pintores_estilo [Cuadres_Class49]))

([Cuadres_Class39] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class99]
		[Cuadres_Class107]
		[Cuadres_Class118])
	(Nombre_estilo "Simbolismo")
	(Pintores_estilo
		[Cuadres_Class53]
		[Cuadres_Class60]
		[Cuadres_Class67]))

([Cuadres_Class4] of  Epoca

	(Cuadros_epoca [Cuadres_Class72])
	(Nombre_epoca "Romanico")
	(Pintores_epoca [Cuadres_Class73]))

([Cuadres_Class40] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class74])
	(Nacionalidad "holandes")
	(Nombre "Abraham Bloemaert"))

([Cuadres_Class41] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class75])
	(Nacionalidad "italiano")
	(Nombre "Adrien Cronenburch"))

([Cuadres_Class42] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class76])
	(Nacionalidad "italiano")
	(Nombre "Alessandro Bonvicino"))

([Cuadres_Class43] of  Pintor

	(Epocas_pintor
		[Cuadres_Class6]
		[Cuadres_Class5])
	(Estilos_pintor [Cuadres_Class28])
	(Ha_pintado
		[Cuadres_Class77]
		[Cuadres_Class78]
		[Cuadres_Class79]
		[Cuadres_Class80])
	(Nacionalidad "frances")
	(Nombre "Claude Oscar Monet"))

([Cuadres_Class44] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class87])
	(Nacionalidad "italiano")
	(Nombre "Domenico Beccafumi"))

([Cuadres_Class45] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class14])
	(Ha_pintado [Cuadres_Class88])
	(Nacionalidad "noruego")
	(Nombre "Edvard Munch"))

([Cuadres_Class46] of  Pintor

	(Epocas_pintor [Cuadres_Class2])
	(Estilos_pintor [Cuadres_Class15])
	(Ha_pintado [Cuadres_Class89])
	(Nacionalidad "holandes")
	(Nombre "El Bosco"))

([Cuadres_Class47] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class35])
	(Ha_pintado [Cuadres_Class90])
	(Nacionalidad "aleman")
	(Nombre "Eugene Bracht"))

([Cuadres_Class48] of  Pintor

	(Epocas_pintor [Cuadres_Class2])
	(Estilos_pintor [Cuadres_Class15])
	(Ha_pintado [Cuadres_Class91])
	(Nacionalidad "italiano")
	(Nombre "Federico Barocci"))

([Cuadres_Class49] of  Pintor

	(Epocas_pintor [Cuadres_Class5])
	(Estilos_pintor
		[Cuadres_Class38]
		[Cuadres_Class31]
		[Cuadres_Class33])
	(Ha_pintado
		[Cuadres_Class92]
		[Cuadres_Class93]
		[Cuadres_Class94]
		[Cuadres_Class95])
	(Nacionalidad "espanol")
	(Nombre "Francisco de Goya"))

([Cuadres_Class5] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class80]
		[Cuadres_Class91]
		[Cuadres_Class92]
		[Cuadres_Class93]
		[Cuadres_Class94]
		[Cuadres_Class95])
	(Nombre_epoca "Romanticismo")
	(Pintores_epoca
		[Cuadres_Class43]
		[Cuadres_Class49]
		[Cuadres_Class71]))

([Cuadres_Class50] of  Pintor

	(Epocas_pintor [Cuadres_Class1])
	(Estilos_pintor [Cuadres_Class11])
	(Ha_pintado [Cuadres_Class96])
	(Nacionalidad "espanol")
	(Nombre "Francisco de Zurbaran"))

([Cuadres_Class51] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class34])
	(Ha_pintado [Cuadres_Class98])
	(Nacionalidad "frances")
	(Nombre "Georges Seurat"))

([Cuadres_Class52] of  Pintor

	(Epocas_pintor [Cuadres_Class7])
	(Estilos_pintor [Cuadres_Class36])
	(Ha_pintado [Cuadres_Class97])
	(Nacionalidad "estadounidense")
	(Nombre "Grant Wood"))

([Cuadres_Class53] of  Pintor

	(Epocas_pintor [Cuadres_Class7])
	(Estilos_pintor [Cuadres_Class39])
	(Ha_pintado [Cuadres_Class99])
	(Nacionalidad "austriaco")
	(Nombre "Gustav Klimt"))

([Cuadres_Class54] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class31])
	(Ha_pintado [Cuadres_Class100])
	(Nacionalidad "frances")
	(Nombre "Jacques-Louis David"))

([Cuadres_Class55] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class101])
	(Nacionalidad "holandes")
	(Nombre "Jan Brueghel el Mozo"))

([Cuadres_Class56] of  Pintor

	(Epocas_pintor [Cuadres_Class2])
	(Estilos_pintor [Cuadres_Class15])
	(Ha_pintado
		[Cuadres_Class102]
		[Cuadres_Class103])
	(Nacionalidad "holandes")
	(Nombre "Jan Van Eyck"))

([Cuadres_Class57] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class104])
	(Nacionalidad "italiano")
	(Nombre "Leandro Bassano"))

([Cuadres_Class58] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class9])
	(Ha_pintado [Cuadres_Class105])
	(Nacionalidad "italiano")
	(Nombre "Leonardo da Vinci"))

([Cuadres_Class59] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class9])
	(Ha_pintado [Cuadres_Class106])
	(Nacionalidad "italiano")
	(Nombre "Miguel Angel"))

([Cuadres_Class6] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class77]
		[Cuadres_Class78]
		[Cuadres_Class79]
		[Cuadres_Class88]
		[Cuadres_Class90]
		[Cuadres_Class98]
		[Cuadres_Class100]
		[Cuadres_Class110]
		[Cuadres_Class112]
		[Cuadres_Class118]
		[Cuadres_Class119]
		[Cuadres_Class122]
		[Cuadres_Class123]
		[Cuadres_Class124]
		[Cuadres_Class125]
		[Cuadres_Class126]
		[Cuadres_Class127]
		[Cuadres_Class128]
		[Cuadres_Class129])
	(Nombre_epoca "Siglo XIX")
	(Pintores_epoca
		[Cuadres_Class43]
		[Cuadres_Class45]
		[Cuadres_Class47]
		[Cuadres_Class51]
		[Cuadres_Class54]
		[Cuadres_Class60]
		[Cuadres_Class62]
		[Cuadres_Class67]
		[Cuadres_Class69]
		[Cuadres_Class68]))

([Cuadres_Class60] of  Pintor

	(Epocas_pintor
		[Cuadres_Class6]
		[Cuadres_Class7])
	(Estilos_pintor
		[Cuadres_Class39]
		[Cuadres_Class35]
		[Cuadres_Class10]
		[Cuadres_Class14])
	(Ha_pintado
		[Cuadres_Class107]
		[Cuadres_Class108]
		[Cuadres_Class109]
		[Cuadres_Class110])
	(Nacionalidad "espanol")
	(Nombre "Pablo Picasso"))

([Cuadres_Class61] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class111])
	(Nacionalidad "holandes")
	(Nombre "Paul Brill"))

([Cuadres_Class62] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class35])
	(Ha_pintado [Cuadres_Class112])
	(Nacionalidad "frances")
	(Nombre "Paul Cezanne"))

([Cuadres_Class63] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class9])
	(Ha_pintado [Cuadres_Class113])
	(Nacionalidad "italiano")
	(Nombre "Rafael de Urbino"))

([Cuadres_Class64] of  Pintor

	(Epocas_pintor [Cuadres_Class2])
	(Estilos_pintor [Cuadres_Class13])
	(Ha_pintado
		[Cuadres_Class114]
		[Cuadres_Class115])
	(Nacionalidad "holandes")
	(Nombre "Rembrandt"))

([Cuadres_Class65] of  Pintor

	(Epocas_pintor [Cuadres_Class1])
	(Estilos_pintor [Cuadres_Class12])
	(Ha_pintado [Cuadres_Class116])
	(Nacionalidad "holandes")
	(Nombre "Rubens"))

([Cuadres_Class66] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class9])
	(Ha_pintado [Cuadres_Class117])
	(Nacionalidad "italiano")
	(Nombre "Sandro Botticelli"))

([Cuadres_Class67] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor
		[Cuadres_Class30]
		[Cuadres_Class39])
	(Ha_pintado
		[Cuadres_Class118]
		[Cuadres_Class119])
	(Nacionalidad "espanol")
	(Nombre "Santiago Rusinol"))

([Cuadres_Class68] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor [Cuadres_Class35])
	(Ha_pintado [Cuadres_Class129])
	(Nacionalidad "espanol")
	(Nombre "Virxilio Blanco"))

([Cuadres_Class69] of  Pintor

	(Epocas_pintor [Cuadres_Class6])
	(Estilos_pintor
		[Cuadres_Class28]
		[Cuadres_Class32]
		[Cuadres_Class35])
	(Ha_pintado
		[Cuadres_Class122]
		[Cuadres_Class123]
		[Cuadres_Class124]
		[Cuadres_Class125]
		[Cuadres_Class126]
		[Cuadres_Class127]
		[Cuadres_Class128])
	(Nacionalidad "holandes")
	(Nombre "Vincent Van Gogh"))

([Cuadres_Class7] of  Epoca

	(Cuadros_epoca
		[Cuadres_Class97]
		[Cuadres_Class99]
		[Cuadres_Class107]
		[Cuadres_Class108]
		[Cuadres_Class109])
	(Nombre_epoca "Siglo XX")
	(Pintores_epoca
		[Cuadres_Class52]
		[Cuadres_Class53]
		[Cuadres_Class60]))

([Cuadres_Class70] of  Pintor

	(Epocas_pintor [Cuadres_Class3])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class121])
	(Nacionalidad "italiano")
	(Nombre "Vicencio Campi"))

([Cuadres_Class71] of  Pintor

	(Epocas_pintor [Cuadres_Class5])
	(Estilos_pintor [Cuadres_Class29])
	(Ha_pintado [Cuadres_Class120])
	(Nacionalidad "italiano")
	(Nombre "Tiziano"))

([Cuadres_Class72] of  Cuadro

	(Anyo 1200)
	(Complejidad 2704)
	(Dim "108 x 52")
	(Epoca_cuadro [Cuadres_Class4])
	(Estilo_cuadro [Cuadres_Class37])
	(Pintado_por [Cuadres_Class73])
	(Relevancia 1)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "El diluvio o El arca de Noe"))

([Cuadres_Class73] of  Pintor

	(Epocas_pintor [Cuadres_Class4])
	(Estilos_pintor [Cuadres_Class37])
	(Ha_pintado [Cuadres_Class72])
	(Nombre "(anonimo)"))

([Cuadres_Class74] of  Cuadro

	(Anyo 1623)
	(Complejidad 298116)
	(Dim "546 x 800")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class40])
	(Relevancia 0)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "Adoration of the Magi"))

([Cuadres_Class75] of  Cuadro

	(Anyo 1587)
	(Complejidad 6084)
	(Dim "104 x 78")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class41])
	(Relevancia 0)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Dama y nina"))

([Cuadres_Class76] of  Cuadro

	(Anyo 1520)
	(Complejidad 6084)
	(Dim "102 x 78")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class42])
	(Relevancia 0)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Alegoria de la Fe"))

([Cuadres_Class77] of  Cuadro

	(Anyo 1916)
	(Complejidad 6241)
	(Dim "79 x 80")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class43])
	(Relevancia 4)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Blue Water Lilies"))

([Cuadres_Class78] of  Cuadro

	(Anyo 1899)
	(Complejidad 6084)
	(Dim "80 x 78")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class43])
	(Relevancia 5)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "El estanque de los nenúfares"))

([Cuadres_Class79] of  Cuadro

	(Anyo 1880)
	(Complejidad 3721)
	(Dim "61 x 81")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class43])
	(Relevancia 1)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "La escarcha"))

([Cuadres_Class8] of  Estilo

	(Cuadros_estilo [Cuadres_Class18])
	(Nombre_estilo "Barroco")
	(Pintores_estilo [Cuadres_Class16]))

([Cuadres_Class80] of  Cuadro

	(Anyo 1867)
	(Complejidad 467856)
	(Dim "800 x 684")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class28])
	(Pintado_por [Cuadres_Class43])
	(Relevancia 1)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class81])
	(Titulo "Mujer en el jardin de Saint-Adresse"))

([Cuadres_Class81] of  Tematica

	(Cuadros_tematica
		[Cuadres_Class80]
		[Cuadres_Class102]
		[Cuadres_Class104]
		[Cuadres_Class119]
		[Cuadres_Class121]
		[Cuadres_Class122]
		[Cuadres_Class123]
		[Cuadres_Class125])
	(Nombre_tematica "Costumbres"))

([Cuadres_Class82] of  Cuadro

	(Anyo 1630)
	(Complejidad 49729)
	(Dim "223 x 290")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class11])
	(Pintado_por [Cuadres_Class16])
	(Relevancia 4)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "La fragua de Vulcano"))

([Cuadres_Class85] of  Cuadro

	(Anyo 1657)
	(Complejidad 48400)
	(Dim "220 x 289")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class11])
	(Pintado_por [Cuadres_Class16])
	(Relevancia 4)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Las Hilanderas"))

([Cuadres_Class86] of  Cuadro

	(Anyo 1656)
	(Complejidad 76176)
	(Dim "318 x 276")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class11])
	(Pintado_por [Cuadres_Class16])
	(Relevancia 8)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "Las meninas"))

([Cuadres_Class87] of  Cuadro

	(Anyo 1545)
	(Complejidad 600625)
	(Dim "775 x 800")
	(Epoca_cuadro [Cuadres_Class3])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class44])
	(Relevancia 0)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "Annunciazione"))

([Cuadres_Class88] of  Cuadro

	(Anyo 1893)
	(Complejidad 5329)
	(Dim "89 x 73")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class14])
	(Pintado_por [Cuadres_Class45])
	(Relevancia 8)
	(Sala 4)
	(Tematica_cuadro [Cuadres_Class26])
	(Titulo "El grito"))

([Cuadres_Class89] of  Cuadro

	(Anyo 1500)
	(Complejidad 18225)
	(Dim "135 x 190")
	(Epoca_cuadro [Cuadres_Class2])
	(Estilo_cuadro [Cuadres_Class15])
	(Pintado_por [Cuadres_Class46])
	(Relevancia 2)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class27])
	(Titulo "El carro de Heno"))

([Cuadres_Class9] of  Estilo

	(Cuadros_estilo
		[Cuadres_Class105]
		[Cuadres_Class106]
		[Cuadres_Class113]
		[Cuadres_Class117])
	(Nombre_estilo "Cinquecento")
	(Pintores_estilo
		[Cuadres_Class58]
		[Cuadres_Class59]
		[Cuadres_Class63]
		[Cuadres_Class66]))

([Cuadres_Class90] of  Cuadro

	(Anyo 1873)
	(Complejidad 13689)
	(Dim "117 x 171")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class35])
	(Pintado_por [Cuadres_Class47])
	(Relevancia 0)
	(Sala 5)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Clouds over the Luneburger Heath"))

([Cuadres_Class91] of  Cuadro

	(Anyo 1598)
	(Complejidad 308025)
	(Dim "800 x 555")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class29])
	(Pintado_por [Cuadres_Class48])
	(Relevancia 0)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Aeneas Flight from Troy"))

([Cuadres_Class92] of  Cuadro

	(Anyo 1771)
	(Complejidad 576)
	(Dim "33 x 24")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class38])
	(Pintado_por [Cuadres_Class49])
	(Relevancia 1)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "El sacrificio a Pan"))

([Cuadres_Class93] of  Cuadro

	(Anyo 1790)
	(Complejidad 9604)
	(Dim "98 x 191")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class31])
	(Pintado_por [Cuadres_Class49])
	(Relevancia 7)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "La maja desnuda"))

([Cuadres_Class94] of  Cuadro

	(Anyo 1814)
	(Complejidad 70756)
	(Dim "266 x 345")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class33])
	(Pintado_por [Cuadres_Class49])
	(Relevancia 6)
	(Sala 4)
	(Tematica_cuadro [Cuadres_Class22])
	(Titulo "Los fusilamientos del tres de mayo"))

([Cuadres_Class95] of  Cuadro

	(Anyo 1821)
	(Dim "146 x 83")
	(Epoca_cuadro [Cuadres_Class5])
	(Estilo_cuadro [Cuadres_Class31])
	(Pintado_por [Cuadres_Class49])
	(Relevancia 6)
	(Sala 3)
	(Tematica_cuadro [Cuadres_Class24])
	(Titulo "Saturno devorando a un hijo"))

([Cuadres_Class96] of  Cuadro

	(Anyo 1635)
	(Complejidad 2116)
	(Dim "46 x 84")
	(Epoca_cuadro [Cuadres_Class1])
	(Estilo_cuadro [Cuadres_Class11])
	(Pintado_por [Cuadres_Class50])
	(Relevancia 4)
	(Sala 1)
	(Tematica_cuadro [Cuadres_Class20])
	(Titulo "Bodegon"))

([Cuadres_Class97] of  Cuadro

	(Anyo 1930)
	(Complejidad 4225)
	(Dim "78 x 65")
	(Epoca_cuadro [Cuadres_Class7])
	(Estilo_cuadro [Cuadres_Class36])
	(Pintado_por [Cuadres_Class52])
	(Relevancia 2)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class17])
	(Titulo "American Gothic"))

([Cuadres_Class98] of  Cuadro

	(Anyo 1884)
	(Complejidad 43264)
	(Dim "208 x 308")
	(Epoca_cuadro [Cuadres_Class6])
	(Estilo_cuadro [Cuadres_Class34])
	(Pintado_por [Cuadres_Class51])
	(Relevancia 2)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class25])
	(Titulo "Tarde de domingo en la isla de la Grande Jatte"))

([Cuadres_Class99] of  Cuadro

	(Anyo 1907)
	(Complejidad 32400)
	(Dim "180 x 180")
	(Epoca_cuadro [Cuadres_Class7])
	(Estilo_cuadro [Cuadres_Class39])
	(Pintado_por [Cuadres_Class53])
	(Relevancia 7)
	(Sala 2)
	(Tematica_cuadro [Cuadres_Class19])
	(Titulo "El beso")))

;;Clases nuestras

(defclass Recomendacion 
	(is-a USER)
	(role concrete)
    (slot nombre_cuadro
		(type INSTANCE)
		(create-accessor read-write))
    (slot puntuacion
        (type INTEGER)
        (create-accessor read-write))
    (multislot justificaciones
		(type STRING)
		(create-accessor read-write))
)

(defclass Dia
	(is-a USER)
	(role concrete)
	(multislot recomendaciones
		(type INSTANCE)
		(create-accessor read-write))
	(slot tiempo-maximo
		(type INTEGER)
		(create-accessor read-write))
)

;;; Modulo principal de utilidades

(defmodule MAIN (export ?ALL))

;;; Modulo de recopilacion de los datos del grupo + preferencias

(defmodule recopilacion-grupo
	(import MAIN ?ALL)
	(export ?ALL)
)

(defmodule recopilacion-preferencias
	(import MAIN ?ALL)
	(import recopilacion-grupo deftemplate ?ALL)
	(export ?ALL)
)

(defmodule procesado-datos
	(import MAIN ?ALL)
	(import recopilacion-grupo deftemplate ?ALL)
	(import recopilacion-preferencias deftemplate ?ALL)
	(export ?ALL)
)

(defmodule generacion_soluciones
	(import MAIN ?ALL)
	(export ?ALL)
)

(defmodule resultados_al_grupo
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Imprime los datos de un contenido

(defmessage-handler MAIN::Cuadro imprimir ()
	(format t "Titulo: %s %n" ?self:Titulo)
	(printout t crlf)
	(format t "Anyo: %d" ?self:Anyo)
	(printout t crlf)
    (format t "Epoca del cuadro: %s" (send ?self:Epoca_cuadro get-Nombre_epoca))
	(printout t crlf)
    (format t "Dimensiones: %s" ?self:Dim)
	(printout t crlf)
    (format t "Sala: %d" ?self:Sala)
	(printout t crlf)
    (format t "Pintado por: %s" (send ?self:Pintado_por get-Nombre))
	(printout t crlf)
    (format t "Tematica del cuadro: %s" (send ?self:Tematica_cuadro get-Nombre_tematica))
	(printout t crlf)
)


(defmessage-handler MAIN::Recomendacion imprimir ()
	(printout t "-----------------------------------" crlf)
	(printout t (send ?self:nombre_cuadro imprimir))
	(printout t crlf)
	(format t "Nivel de recomendacion: %d %n" ?self:puntuacion)
	(printout t "Justificacion de la eleccion: " crlf)
	(progn$ (?curr-just ?self:justificaciones)
		(printout t ?curr-just crlf)
	)
	(printout t crlf)
	(printout t "-----------------------------------" crlf)
)

(defmessage-handler MAIN::Dia imprimir ()
	(printout t "============================================" crlf)    
	(bind $?recs ?self:recomendaciones)
	(progn$ (?curr-rec $?recs)
		(printout t (send ?curr-rec imprimir))
	)
	(printout t "============================================" crlf)
)


;;; Declaracion de templates --------------------------

;;; Template para los datos del grupo

(deftemplate MAIN::datos_grupo
	(slot descripcion (type STRING) (default "desc")) ;tamanyo del grupo
	(slot nivel (type INTEGER)(default -1)) ;conocimiento
	(slot edad (type INTEGER)(default -1)) ;edad general del grupo
    (slot dias (type INTEGER)(default -1)) ;nº dias en visitar el museo
    (slot horasdia (type INTEGER)(default -1)) ;nº horas/dia
    (slot tiempo (type INTEGER)(default -1)) ;total de tiempo
)

;;; Template para las preferencias del usuario
(deftemplate MAIN::preferencias_grupo
	(multislot autores_favoritos (type INSTANCE))
	(multislot tematicas_obras_fav (type INSTANCE))
	(multislot estilos_favoritos (type INSTANCE))
	(multislot epocas_favoritas (type INSTANCE))
)

;;; Template para una lista de recomendaciones sin orden
(deftemplate MAIN::lista-rec-desordenada
	(multislot recomendaciones (type INSTANCE))
)

;;; Template para una lista de recomendaciones con orden
(deftemplate MAIN::lista-rec-ordenada
	(multislot recomendaciones (type INSTANCE))
)

(deftemplate MAIN::lista-dias
	(multislot dias (type INSTANCE))
)


(deftemplate MAIN::dias-orden-sala
	(multislot dias (type INSTANCE))
)

;;; Cuadros ordenados
(defclass ListaCuadros 
	(is-a USER)
	(role concrete)
	(multislot cuadros
		(type INSTANCE)
		(create-accessor read-write))
)

(deftemplate MAIN::lista-cuadros-ordenada
	(multislot cuadros (type INSTANCE))
)

(deffunction maximo-puntuacion ($?lista)
	(bind ?maximo -1)
	(bind ?elemento nil)
	(progn$ (?curr-rec $?lista)
		(bind ?curr-cont (send ?curr-rec get-nombre_cuadro))
		(bind ?curr-punt (send ?curr-rec get-puntuacion))
		(if (> ?curr-punt ?maximo)
			then 
			(bind ?maximo ?curr-punt)
			(bind ?elemento ?curr-rec)
		)
	)
	?elemento
)

(deffunction orden-sala ($?lista)
	(bind ?minimo 6)
	(bind ?elemento nil)
    (progn$ (?curr-rec $?lista)
         (bind ?curr-cuadro (send ?curr-rec get-nombre_cuadro))
         (bind ?curr-sala (send ?curr-cuadro get-Sala))
        (if (<= ?curr-sala ?minimo)
            then
            (bind ?minimo ?curr-sala)
            (bind ?elemento ?curr-rec)
         )
     )  		
	?elemento
)

;;; Funcion para hacer una pregunta no-numerica-univalor
(deffunction pregunta-datos (?pregunta)
    (format t "%s " ?pregunta)
	(bind ?respuesta (read))
	(while (not (lexemep ?respuesta)) do
		(format t "%s " ?pregunta)
		(bind ?respuesta (read))
    )
	?respuesta
)

;;; Funcion para hacer una pregunta numerica-univalor
(deffunction MAIN::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s (De %d hasta %d) " ?pregunta ?rangini ?rangfi)
	(bind ?respuesta (read))
	(while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
		(format t "%s (De %d hasta %d) " ?pregunta ?rangini ?rangfi)
		(bind ?respuesta (read))
	)
	?respuesta
)


;;; Funcion para hacer pregunta con muchas opciones
(deffunction MAIN::pregunta-opciones (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (bind ?respuesta (pregunta-numerica "Escoge una opcion:" 1 (length$ ?valores-posibles)))
	?respuesta
)

;;; Funcion para hacer una pregunta general con una serie de respuestas admitidas
(deffunction MAIN::pregunta-opciones2 (?question $?allowed-values)
   (format t "%s "?question)
   (progn$ (?curr-value $?allowed-values)
		(format t "[%s]" ?curr-value)
	)
   (printout t ": ")
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (format t "%s "?question)
	  (progn$ (?curr-value $?allowed-values)
		(format t "[%s]" ?curr-value)
	  )
	  (printout t ": ")
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer
)
;;; Funcion para hacer una pregunta de tipo si/no
(deffunction MAIN::pregunta-si-no (?question)
   (bind ?response (pregunta-opciones ?question si no))
   (if (or (eq ?response si) (eq ?response s))
       then TRUE 
       else FALSE)
)

;;; Funcion para hacer una pregunta multi-respuesta con indices
(deffunction MAIN::pregunta-multirespuesta (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (format t "%s" "Indica los numeros referentes a las preferencias separados por un espacio: ")
    (bind ?resp (readline))
    (bind ?numeros (str-explode ?resp))
    (bind $?lista (create$))
    (progn$ (?var ?numeros) 
        (if (and (integerp ?var) (and (>= ?var 0) (<= ?var (length$ ?valores-posibles))))
            then 
                (if (not (member$ ?var ?lista))
                    then (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?var))
                )
        ) 
    )
    (if (or(member$ 0 ?lista)(= (length$ ?lista) 0)) then (bind ?lista (create$ ))) 
    ;(if (member$ 0 ?lista) then (bind ?lista (create$ 0)))
    ?lista
)
;;; Funcion para hacer pregunta con indice de respuestas posibles
(deffunction MAIN::pregunta-indice (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (bind ?respuesta (pregunta-numerica "Escoge una opcion:" 1 (length$ ?valores-posibles)))
	?respuesta
)

(defrule MAIN::initialRule "Regla inicial"
	(declare (salience 10))
	=>
	(printout t"----------------------------------------------------------" crlf)
  	(printout t"          Personalizacion de visitas a un museo           " crlf)
	(printout t"----------------------------------------------------------" crlf)
  	(printout t crlf)  	
	(printout t"¡Bienvenido! A continuacion se le formularan una serie de preguntas para poder recomendarle una visita adecuada a sus preferencias." crlf)
	(printout t crlf)
	(focus recopilacion-grupo)
)

;;; Recopilacion de datos de entrada -------------------------------------------------

(defrule recopilacion-grupo::establecer-tamanyo "Establece el tamanyo del grupo"
	(not (datos_grupo))
	=>
	(bind ?d (pregunta-numerica "¿De cuantos visitantes esta formado el grupo? " 1 100))
    (if (= ?d 1) then (bind ?descripcion "Individual"))
    (if (= ?d 2) then (bind ?descripcion "Pareja"))
    (if (and(> ?d 2) (< ?d 13)) then (bind ?descripcion "Grupo pequeno (3-12)"))
    (if (and(> ?d 12) (< ?d 26)) then (bind ?descripcion "Grupo mediano (13-25)"))
    (if (> ?d 25) then (bind ?descripcion "Grupo grande (+25)"))
	(assert (datos_grupo (descripcion ?descripcion)))
)


(defrule recopilacion-grupo::establecer-edad "Establece la edad media del grupo"
	?g <- (datos_grupo (edad ?edad))
	(test (< ?edad 0))
	=>
	(bind ?edad (pregunta-numerica "¿Cual es la media de edad del grupo? " 1 110)) 
	(modify ?g (edad ?edad))
)

(defrule recopilacion-grupo::establecer-dias "Establece el nº dias de la visita"
	?g <- (datos_grupo (dias ?dias))
    (test (< ?dias 0) 
    )
	=>
	(bind ?dias (pregunta-numerica "¿Durante cuantos dias realizara la visita?" 1 7))
	(modify ?g (dias ?dias))
)

(defrule recopilacion-grupo::establecer-horas "Establece el nº dias de horas de la visita"
	?g <- (datos_grupo (horasdia ?horasdia))
    (test (< ?horasdia 0))    
    =>
    (bind ?horasdia (pregunta-numerica "¿Cuantas horas dedicara diariamente a visitar el museo?" 1 6))
	(modify ?g (horasdia ?horasdia))
)

(defrule recopilacion-grupo::establecer-tiempo "Establece el nº dias de horas de la visita"
	?g <- (datos_grupo (dias ?dias))
    ?d <- (datos_grupo (horasdia ?horasdia))
    ?t <- (datos_grupo (tiempo ?tiempo))
    (test (< ?tiempo 0)) 
    =>  
    (modify ?t (tiempo (bind ?tiempo (* ?horasdia ?dias))))
)

(defrule recopilacion-grupo::preguntas-calcula-nivel "Pregunta al usuario sus conocimientos"
    ?g <- (datos_grupo (nivel ?nivel))
    (test( < ?nivel 0))
	=>
    (bind ?puntuacio 0)
	(bind ?formatos (create$ "Si" "No"))
	(bind ?respuesta (pregunta-indice "Conoces 'El Grito' de Munch?" ?formatos))
	(if (= ?respuesta 1) then (bind ?puntuacio (+ 1 ?puntuacio)))
   
    (bind ?formatos (create$ "Si" "No"))
	(bind ?respuesta (pregunta-indice "Conoces 'Las Meninas' de Velazquez?" ?formatos))
	(if (= ?respuesta 1) then (bind ?puntuacio (+ 1 ?puntuacio)))

    (bind ?formatos (create$ "Si" "No"))
	(bind ?respuesta (pregunta-indice "Conoces 'El nacimiento de Venus' de Boticelli" ?formatos))
	(if (= ?respuesta 1) then (bind ?puntuacio (+ 1 ?puntuacio)))
		
	(bind ?formatos (create$ "La Gioconda (la Mona Lisa)." "El jardin de las delicias." "La ultima cena."))
	(bind ?respuesta (pregunta-indice "Cual de los siguientes titulos no pertenece a un cuadro de Leonardo da Vinci " ?formatos))
	(if (= ?respuesta 2) then (bind ?puntuacio (+ 1 ?puntuacio)))
    
    (bind ?formatos (create$ "La persistencia de la memoria." "Alegoria de la poesia." "American Gothic." "Alegoria de la fe."))
	(bind ?respuesta (pregunta-indice "Cual de estas obras es de Dali" ?formatos))
	(if (= ?respuesta 1) then (bind ?puntuacio (+ 1 ?puntuacio)))
    
    (bind ?formatos (create$ "El Greco." "Francisco de Goya." "Diego Velazquez."))
	(bind ?respuesta (pregunta-indice "¿Quien pinto el cuadro 'Las Hilanderas'?" ?formatos))
	(if (= ?respuesta 2) then (bind ?puntuacio (+ 1 ?puntuacio)))

    
    (bind ?formatos (create$ "Klimt" "Tiziano" "Yanyez" "El Greco"))
	(bind ?respuesta (pregunta-indice "¿Quien pinto 'El Beso'?" ?formatos))
	(if (= ?respuesta 1) then (bind ?puntuacio (+ 1 ?puntuacio)))
    
    (modify ?g (nivel ?puntuacio))
)  

(defrule recopilacion-grupo::pasar-a-preferencias "Pasa a la recopilacion de preferencias"
    (declare (salience 10))	
	?g <- (datos_grupo (descripcion ~"desc")(edad ?e) (dias ?d) (horasdia ?horasdia) (nivel ?nivel) (tiempo ?tiempo))
    (test (> ?e -1))
    (test (> ?d -1))
    (test (> ?nivel -1))
    (test (> ?tiempo -1))
    (test (> ?horasdia -1))
	=>
	(focus recopilacion-preferencias)
)

(deffacts recopilacion-preferencias::hechos-iniciales "Establece hechos para poder recopilar informacion"      
    (autores_fav ask)
    (tematicas_obras ask)
    (estilos_fav ask)
	(epocas_fav ask)
    (preferencias_grupo )
)

(defrule recopilacion-preferencias::establecer-pintores-favoritos "Establece los pintores favoritos del grupo"
    ?hecho <- (autores_fav ask)
	?pref <- (preferencias_grupo)
	=>
	(bind $?obj-pintores (find-all-instances ((?inst Pintor)) TRUE))
	(bind $?nom-pintores (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-pintores)) do
		(bind ?curr-obj (nth$ ?i ?obj-pintores))
		(bind ?curr-nom (send ?curr-obj get-Nombre))
		(bind $?nom-pintores(insert$ $?nom-pintores (+ (length$ $?nom-pintores) 1) ?curr-nom))
	)
	(bind ?escogido (pregunta-multirespuesta "Escoja sus pintores favoritos(o 0 en el caso contrario): " $?nom-pintores))
	(assert (autores_fav TRUE))
    (bind $?respuesta (create$ ))
	(loop-for-count (?i 1 (length$ ?escogido)) do
		(bind ?curr-index (nth$ ?i ?escogido))
        (if (= ?curr-index 0) then (assert (autores_fav FALSE)))
		(bind ?curr-autor (nth$ ?curr-index ?obj-pintores))
		(bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-autor))
	)
	
	(retract ?hecho)
    (modify ?pref (autores_favoritos $?respuesta))
)


(defrule recopilacion-preferencias::establecer-tematicas-favorias "Establece las tematicas favoritas del grupo "
    ?hecho <- (tematicas_obras ask)
	?pref <- (preferencias_grupo)
	=>
	(bind $?obj-tematicas (find-all-instances ((?inst Tematica)) TRUE))
	(bind $?nom-tematicas (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-tematicas)) do
		(bind ?curr-obj (nth$ ?i ?obj-tematicas))
		(bind ?curr-nom (send ?curr-obj get-Nombre_tematica))
		(bind $?nom-tematicas(insert$ $?nom-tematicas (+ (length$ $?nom-tematicas) 1) ?curr-nom))
	)
	(bind ?escogido (pregunta-multirespuesta "Escoja sus tematicas favoritas(o 0 en el caso contrario): " $?nom-tematicas))

	(bind $?respuesta (create$ ))
    (assert (tematicas_obras TRUE))
	(loop-for-count (?i 1 (length$ ?escogido)) do
		(bind ?curr-index (nth$ ?i ?escogido))
        (if (= ?curr-index 0) then (assert (tematicas_obras_fav FALSE)))
		(bind ?curr-tematica (nth$ ?curr-index ?obj-tematicas))
		(bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-tematica))
	)
	
	(retract ?hecho)
	(modify ?pref (tematicas_obras_fav $?respuesta))
)

(defrule recopilacion-preferencias::establecer-estilos-favoritos "Establece los estilos favoritos del grupo"
    ?hecho <- (estilos_fav ask)
	?pref <- (preferencias_grupo)
	=>
	(bind $?obj-estilos (find-all-instances ((?inst Estilo)) TRUE))
	(bind $?nom-estilos (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-estilos)) do
		(bind ?curr-obj (nth$ ?i ?obj-estilos))
		(bind ?curr-nom (send ?curr-obj get-Nombre_estilo))
		(bind $?nom-estilos(insert$ $?nom-estilos (+ (length$ $?nom-estilos) 1) ?curr-nom))
	)
	(bind ?escogido (pregunta-multirespuesta "Escoja sus estilos favoritos(o 0 en el caso contrario): " $?nom-estilos))

	(bind $?respuesta (create$ ))
    (assert (estilos_fav TRUE))
	(loop-for-count (?i 1 (length$ ?escogido)) do
		(bind ?curr-index (nth$ ?i ?escogido))
        (if (= ?curr-index 0) then (assert (estilos_favoritos FALSE)))
		(bind ?curr-estilos (nth$ ?curr-index ?obj-estilos))
		(bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-estilos))
	)
	
	(retract ?hecho)
	(modify ?pref (estilos_favoritos $?respuesta))
)

(defrule recopilacion-preferencias::establecer-epocas-favoritas "Establece las epocas favoritas del grupo"
    ?hecho <- (epocas_fav ask)
	?pref <- (preferencias_grupo)
	=>
	(bind $?obj-epocas (find-all-instances ((?inst Epoca)) TRUE))
	(bind $?nom-epocas (create$ ))
	(loop-for-count (?i 1 (length$ $?obj-epocas)) do
		(bind ?curr-obj (nth$ ?i ?obj-epocas))
		(bind ?curr-nom (send ?curr-obj get-Nombre_epoca))
		(bind $?nom-epocas(insert$ $?nom-epocas (+ (length$ $?nom-epocas) 1) ?curr-nom))
	)
	(bind ?escogido (pregunta-multirespuesta "Escoja sus epocas favoritas(o 0 en el caso contrario): " $?nom-epocas))

	(bind $?respuesta (create$ ))
    (assert (epocas_fav TRUE))
	(loop-for-count (?i 1 (length$ ?escogido)) do
		(bind ?curr-index (nth$ ?i ?escogido))
        (if (= ?curr-index 0) then (assert (epocas_favoritas FALSE)))
		(bind ?curr-epocas (nth$ ?curr-index ?obj-epocas))
		(bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-epocas))
	)
	
	(retract ?hecho)   
	(modify ?pref (epocas_favoritas $?respuesta))
)

(defrule recopilacion-preferencias::pasar_procesado_datos "Pasa al modulo de procesado de datos"
	(declare (salience -1))
	?h1 <- (autores_fav TRUE|FALSE)
	?h2 <- (tematicas_obras TRUE|FALSE)
	?h3 <- (estilos_fav TRUE|FALSE)
	?h4 <- (epocas_fav TRUE|FALSE)
	=>
	(focus procesado-datos)
    (printout t "Procesando los datos obtenidos..." crlf)
)

;;; Modulo procesado de datos ---------------------------------------------------

(defrule procesado-datos::anadir-cuadros "Se añaden todos los cuadros"
    (declare (salience 10))
	=>
	(bind $?lista (find-all-instances ((?inst Cuadro)) TRUE))
	(progn$ (?curr-con ?lista)
		(make-instance (gensym) of Recomendacion (nombre_cuadro ?curr-con)(puntuacion 0))
	)	
   (printout t "..." crlf)
)

(defrule procesado-datos::aux-autores "Crea hechos para poder procesar los autores favoritos"
    (preferencias_grupo (autores_favoritos $?gen))
	?hecho <- (autores_fav ?aux)
	(test (or (eq ?aux TRUE) (eq ?aux FALSE)))
	=>
	(retract ?hecho)
	(if (eq ?aux TRUE)then 
		(progn$ (?curr-gen $?gen)
			(assert (autores ?curr-gen))
		)
	)
    (printout t "..." crlf)
)

(defrule procesado-datos::aux-tematicas "Crea hechos para poder procesar las tematicas favoritas"
	(preferencias_grupo (tematicas_obras_fav $?gen))
	?hecho <- (tematicas_obras ?aux)
	(test (or (eq ?aux TRUE) (eq ?aux FALSE)))
	=>
	(retract ?hecho)
	(if (eq ?aux TRUE)then 
		(progn$ (?curr-gen $?gen)
			(assert (tematicas ?curr-gen))
		)
	)
    (printout t "..." crlf)
)

(defrule procesado-datos::aux-estilos "Crea hechos para poder procesar los estilos favoritos"
    (preferencias_grupo (estilos_favoritos $?gen))
	?hecho <- (estilos_fav ?aux)
	(test (or (eq ?aux TRUE) (eq ?aux FALSE)))
	=>
	(retract ?hecho)
	(if (eq ?aux TRUE)then 
		(progn$ (?curr-gen $?gen)
			(assert (estilos ?curr-gen))
		)
	)
)

(defrule procesado-datos::aux-epocas "Crea hechos para poder procesar las espocas favoritas"	
    (preferencias_grupo (epocas_favoritas $?gen))
	?hecho <- (epocas_fav ?aux)
	(test (or (eq ?aux TRUE) (eq ?aux FALSE)))
	=>
	(retract ?hecho)
	(if (eq ?aux TRUE)then 
		(progn$ (?curr-gen $?gen)
			(assert (epocas ?curr-gen))
		)
	)
    (printout t "..." crlf)
)

;;; Aplicamos los filtros de las preguntas -------------------------------------------------

(defrule procesado-datos::valorar-nivel-mayor-a-4 "Se mejora la puntuacion de los cuadros"
	(datos_grupo (nivel ?nivel))
	(test (> ?nivel 3)) 
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	?cont <-(object (is-a Cuadro) (Relevancia ?relevancia) (Complejidad ?complejidad))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorado-nivel ?cont ?nivel))
	=>
    (if (> ?complejidad 10000) then
		(bind ?p (+ ?p 60))
		(bind $?just (insert$ $?just (+ (length$ $?just) 1) "Tiene una complejidad alta acorde al nivel del visitante -> +60")) 
	)
    (if (< ?relevancia 4) then
        (bind ?p (+ ?p 40))
		(bind $?just (insert$ $?just (+ (length$ $?just) 1) "Tiene una relevancia mediana/baja acorde al interes del visitante -> +40")) 
	)
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just) 
    (assert (valorado-nivel ?cont ?nivel))
    (printout t "Valorando nivel del grupo..." crlf)
)

(defrule procesado-datos::valorar-nivel-menor-a-4 "Se mejora la puntuacion de los cuadros"
	(datos_grupo (nivel ?nivel))
	(test (< ?nivel 4)) 
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	?cont <-(object (is-a Cuadro) (Relevancia ?relevancia) (Complejidad ?complejidad))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorado-nivel ?cont ?nivel))
	=>
    (if (< ?complejidad 10000) then
		(bind ?p (+ ?p 30))
		(bind $?just (insert$ $?just (+ (length$ $?just) 1) "Tiene una complejidad baja acorde al nivel del visitante -> +30")) 
	)
    (if (> ?relevancia 2) then
        (bind ?p (+ ?p 70))
		(bind $?just (insert$ $?just (+ (length$ $?just) 1) "Tiene una relevancia alta acorde al interes del visitante -> +70")) 
	)
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just) 
    (assert (valorado-nivel ?cont ?nivel))
    (printout t "Valorando nivel del grupo..." crlf)
)

(defrule procesado-datos::valorar-autores-favoritos "Se mejora la puntuacion de los cuadros de autores favoritos"
	?hecho <- (autores ?auto)
	?cont <-(object (is-a Cuadro) (Pintado_por ?autor))
	(test (eq (instance-name ?auto) ?autor))
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorado-autor-favorito ?cont ?auto)) ;?auto al final
	=>
	(bind ?p (+ ?p 50))
	(bind ?text (str-cat "Pertenece al autor favorito: " (send ?auto get-Nombre) " -> +50"))
    (bind $?just (insert$ $?just (+ (length$ $?just) 1) ?text))
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just)
	(assert (valorado-autor-favorito ?cont ?auto))
    (printout t "Comprobando autores favoritos..." crlf)
)

(defrule procesado-datos::valorar-tematicas-favoritas "Se mejora la puntuacion de las tematicas favoritas"
	?hecho <- (tematicas ?tem)
	?cont <-(object (is-a Cuadro) (Tematica_cuadro ?tema))
	(test (eq (instance-name ?tem) ?tema))
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorar-tematicas-favoritas ?cont ?tem))
	=>
	(bind ?p (+ ?p 50))
	(bind ?text (str-cat "Pertenece a la tematica favorita: " (send ?tem get-Nombre_tematica) " -> +50"))
    (bind $?just (insert$ $?just (+ (length$ $?just) 1) ?text))
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just)
	(assert (valorar-tematicas-favoritas ?cont ?tem))
    (printout t "Comprobando tematicas favoritas..." crlf)
)

(defrule procesado-datos::valorar-estilos-favoritos "Se mejora la puntuacion de los estilos favoritos"
	?hecho <- (estilos ?estilo)
	?cont <-(object (is-a Cuadro) (Estilo_cuadro ?estilos))
	(test (eq (instance-name ?estilo) ?estilos))
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorar-estilos-favoritos ?cont ?estilo))
	=>
	(bind ?p (+ ?p 50))
	(bind ?text (str-cat "Pertenece al estilo favorito: " (send ?estilo get-Nombre_estilo) " -> +50"))
    (bind $?just (insert$ $?just (+ (length$ $?just) 1) ?text))
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just)
	(assert (valorar-estilos-favoritos ?cont ?estilo))
     (printout t "Comprobando estilos favoritos..." crlf)
)

(defrule procesado-datos::valorar-epocas-favorias "Se mejora la puntuacion de las epocas favoritas"
	?hecho <- (epocas ?epoca)
	?cont <-(object (is-a Cuadro) (Epoca_cuadro ?epocas))
	(test (eq (instance-name ?epoca) ?epocas))
	?rec <- (object (is-a Recomendacion) (nombre_cuadro ?conta) (puntuacion ?p) (justificaciones $?just))
	(test (eq (instance-name ?cont) (instance-name ?conta)))
	(not (valorar-epocas-favorias ?cont ?epoca))
	=>
	(bind ?p (+ ?p 50))
	(bind ?text (str-cat "Pertenece a la epoca favorita: " (send ?epoca get-Nombre_epoca) " -> +50"))
    (bind $?just (insert$ $?just (+ (length$ $?just) 1) ?text))
	(send ?rec put-puntuacion ?p)
    (send ?rec put-justificaciones $?just)
	(assert (valorar-epocas-favorias ?cont ?epoca))
    (printout t "Comprobando epocas favoritas..." crlf)
)

(defrule procesado-datos::pasar-a-generacion "Pasa al modulo de generacion de respuestas"
	(declare(salience -10))
	=>
	(printout t "Generando respuesta..." crlf)
	(focus generacion_soluciones)
)

;;; Módulo de generacion de respuestas -------------------------------------------------

(defrule generacion_soluciones::crea-lista-recomendaciones "Se crea una lista de recomendaciones para ordenarlas"
	(not (lista-rec-desordenada))
	=>
	(assert (lista-rec-desordenada))
)

(defrule generacion_soluciones::anyadir-recomendacion "Anyade una recomendacion a la lista de recomendaciones"
	(declare (salience 10))
	?rec <- (object (is-a Recomendacion))
	?hecho <- (lista-rec-desordenada (recomendaciones $?lista))
	(test (not (member$ ?rec $?lista)))
	=>
	(bind $?lista (insert$ $?lista (+ (length$ $?lista) 1) ?rec))
	(modify ?hecho (recomendaciones $?lista))
)

(defrule generacion_soluciones::crea-lista-ordenada "Se crea una lista ordenada de contenido"
	(not (lista-rec-ordenada))
	(lista-rec-desordenada (recomendaciones $?lista))
	=>
	(bind $?resultado (create$ ))
	(while (not (eq (length$ $?lista) 0))  do
		(bind ?curr-rec (maximo-puntuacion $?lista))
		(bind $?lista (delete-member$ $?lista ?curr-rec))
		(bind $?resultado (insert$ $?resultado (+ (length$ $?resultado) 1) ?curr-rec))
	)
	(assert (lista-rec-ordenada (recomendaciones $?resultado)))
    (printout t "Ordenando obras de arte..." crlf)
)

(defrule generacion_soluciones::asigna-contenido-a-dias "Se asigna los contenidos recomendados a dias"
    ?g <- (datos_grupo (dias ?dias) (horasdia ?horas) (descripcion ?descripcion) (nivel ?nivel)); 
	(lista-rec-ordenada (recomendaciones $?recs))
	(not (lista-dias))
	=>
    (bind ?horas (* ?horas 60))
	(bind $?lista (create$ ))
    (while (not(= (length$ $?lista) ?dias)) do
        (bind $?lista (insert$ $?lista (+ (length$ $?lista) 1) (make-instance (gensym) of Dia (tiempo-maximo ?horas))))
    )
	(bind ?i 1)
	(bind ?rec-ant FALSE)
	(while (and (> (length$ $?recs) 0) (<= ?i ?dias)) 
		(bind ?dia (nth$ ?i $?lista))
		(bind $?recs-dia (create$ ))
		(bind ?t-max (send ?dia get-tiempo-maximo))
		(bind ?t-ocu 0)
		(bind ?try 1)
		(bind ?asignados 0)
        (bind ?j 1)
		(while (and(and(< ?t-ocu ?t-max) (< ?try 4)) (> (length$ $?recs) 0) (<= ?j (length$ ?recs))) do
			(bind ?rec (nth$ ?j $?recs))
			(bind ?cont (send ?rec get-nombre_cuadro))
			(bind ?a (send ?cont get-Complejidad))
            (if (or (eq ?descripcion "Pareja") (eq ?descripcion "Individual")) then
                (if (> ?a 120000) then (bind ?t 13)) 
                (if (and (> ?a 13000) (< ?a 120000)) then (bind ?t 10))
                (if (and (> ?a 2000) (< ?a 13000)) then (bind ?t 6))
                (if (and (> ?a 0) (< ?a 2000)) then (bind ?t 4))
            )
            (if (eq ?descripcion "Grupo pequeno (3-12)") then
                (if (> ?a 120000) then (bind ?t 16))
                (if (and (> ?a 13000) (< ?a 120000)) then (bind ?t 12))
                (if (and (> ?a 2000) (< ?a 13000)) then (bind ?t 8))
                (if (and (> ?a 0) (< ?a 2000)) then (bind ?t 5))

            )
            (if (eq ?descripcion "Grupo mediano (13-25)") then
                (if (> ?a 120000) then (bind ?t 18))
                (if (and (> ?a 13000) (< ?a 120000)) then (bind ?t 14))
                (if (and (> ?a 2000) (< ?a 13000)) then (bind ?t 10))
                (if (and (> ?a 0) (< ?a 2000)) then (bind ?t 7))

            )
            (if (eq ?descripcion "Grupo grande (+25)") then
                (if (> ?a 120000) then (bind ?t 20)) 
                (if (and (> ?a 13000) (< ?a 120000)) then (bind ?t 15))
                (if (and (> ?a 2000) (< ?a 13000)) then (bind ?t 12))
                (if (and (> ?a 0) (< ?a 2000)) then (bind ?t 8))
            )
			(if (< (+ ?t-ocu ?t) ?t-max) 
				then
					(bind ?t-ocu (+ ?t-ocu ?t))
					(bind ?try 1)
					(bind ?asignados (+ ?asignados 1))
					(bind ?recs-dia (insert$ $?recs-dia (+ (length$ $?recs-dia) 1) ?rec))
					(bind $?recs (delete-member$ $?recs ?rec))
				else
					(bind ?try (+ ?try 1))
			)
        (bind ?j (+ ?j 1))
		)
		(send ?dia put-recomendaciones $?recs-dia)		
        (bind ?i (+ ?i 1))
	)
	(assert (lista-dias (dias $?lista)))
    (printout t "Computando una ruta optima de visitas..." crlf)
)

(defrule generacion_soluciones::ordena-por-salas "Ordena cada dia por salas."
    (lista-dias (dias $?lista))
    =>
    (progn$ (?curr-dia $?lista)
        (bind $?resultado (create$ ))

        (bind $?recs (send ?curr-dia get-recomendaciones))
        (while (not (eq (length$ $?recs) 0))  do
            (bind ?curr-rec (orden-sala $?recs))
            (bind $?recs (delete-member$ $?recs ?curr-rec))
            (bind $?resultado (insert$ $?resultado (+ (length$ $?resultado) 1) ?curr-rec))
        )
        (send ?curr-dia put-recomendaciones $?resultado)
       
    )
    (assert (dias-orden-sala (dias $?lista))) 
)

(defrule generacion_soluciones::pasar-a-resultados "Se pasa al modulo de presentacion"
    (dias-orden-sala)
	=>
	(focus resultados_al_grupo)
)

(defrule resultados_al_grupo::mostrar-respuesta "Muestra el contenido escogido"
	(dias-orden-sala (dias $?dias))
	(not (final))
	=>
	(printout t crlf)
	(format t "Esta es nuestra recomendacion de ruta para el grupo. Esperamos que la disfruteis.")
	(printout t crlf)
    (format t "%n")
    (printout t crlf)
    (printout t "============================================" crlf)
    (bind ?i 0)
	(progn$ (?curr-dia $?dias)
        (bind ?i(+ ?i 1))
        	(format t "Dia %d" ?i)
            (printout t crlf)
		(printout t (send ?curr-dia imprimir))
	)
	(assert (final))
)




