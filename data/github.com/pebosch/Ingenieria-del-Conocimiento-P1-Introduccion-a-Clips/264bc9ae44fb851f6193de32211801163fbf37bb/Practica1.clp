; *********************************************************************************************
; INGENIERÍA DEL CONOCIMIENTO.
; Solución al problema del Guion de prácticas 1: Introducción a Clips.
; Se ha escrito un programa que contiene el conocimiento sobre mi familia directa: Padres, 
; hermanos, tios, primos y abuelos.
; Cuando se solicite el nombre de una persona de la familia, el programa devolverá como salida 
; las relaciones del resto de la familia con ese miembro (padre, madre, hijo/a, tio/a...).
; Pasos para ejecutar en CLIPS 6.3: 
; 1) Load el archivo .clp
; 2) (reset)
; 3) (run)
; *********************************************************************************************
; Autor: Pedro Fernández Bosch. E-Mail: pedro@pedrobosch.es
; Departamento de Ciencias de la Computación e Inteligencia Artificial, Universidad de Granada.
; Última modificación: 21/03/2015.
; *********************************************************************************************

; DEFFACTS

(deffacts Personas
	(Hombre Antonio)	; Mi abuelo paterno
	(Mujer Leonor)		; Mi abuela paterna
	
	(Hombre Clemente)	; Mi abuelo materno
	(Mujer Marta)		; Mi abuela materna
	
	(Hombre Alberto)	; Mi padre
	(Mujer Rocio)		; Mi madre
	(Hombre Pedro) 		; Yo
	
	(Hombre Vicente)	; Mi tío paterno
	(Mujer Veronica)	; Mi tía política paterna
	(Mujer Irene)		; Mi prima
	(Mujer Alba)		; Mi prima
	
	(Hombre Juan)		; Mi tío paterno
	(Mujer Beatriz)		; Mi tía política paterna
	(Hombre Roberto)	; Mi primo
	
	(Mujer Nuria)		; Mi tía materna
	(Hombre Fernando)	; Mi tío político materno
	(Hombre Sergio)		; Mi primo
)

(deffacts Casadocon
	(Casadocon Clemente Marta)
	(Casadocon Antonio Leonor)
	(Casadocon Alberto Rocio)
	(Casadocon Juan Beatriz)
	(Casadocon Vicente Veronica)
	(Casadocon Fernando Nuria)
)

(deffacts Hijos
	(Hijode Clemente Rocio)
	(Hijode Clemente Nuria)
	(Hijode Antonio Alberto)
	(Hijode Antonio Vicente)
	(Hijode Antonio Juan)
	(Hijode Alberto Pedro)
	(Hijode Vicente Irene)
	(Hijode Vicente Alba)
	(Hijode Juan Roberto)
	(Hijode Fernando Sergio)
)

; DEFRULE 

(defrule nombre
	=>
	(printout t "Intruduzca el nombre de un miembro de su familia: ")
	(assert (insertanombre (read)))
)

(defrule casado
	(insertanombre ?A)
	(Hombre ?A)
	(Casadocon ?A ?B)
	=>
	(printout t "Esta casado con " ?B crlf)
)

(defrule casada
	(insertanombre ?A)
	(Mujer ?A)
	(Casadocon ?B ?A)
	=>
	(printout t "Esta casada con " ?B crlf)
)

(defrule hijo
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	=>
	(printout t "Es hijo de " ?B crlf)
	(printout t "Es hijo de " ?C crlf)
)

(defrule hija
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	=>
	(printout t "Es hija de " ?B crlf)
	(printout t "Es hija de " ?C crlf)
)

(defrule padre
	(insertanombre ?A) 
	(Hijode ?A ?B)
	(Hombre ?A)
	=>
	(printout t "Es padre de " ?B crlf)
)

(defrule madre
	(insertanombre ?A) 
	(Casadocon ?B ?A)
	(Hijode ?B ?C)
	(Mujer ?A)
	=>
	(printout t "Es madre de " ?C crlf)
)

(defrule hermano
	(insertanombre ?A)
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	=>
	(printout t "Es hermano de " ?C crlf)
)

(defrule hermana
	(insertanombre ?A)
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	=>
	(printout t "Es hermana de " ?C crlf)
)

(defrule nietopadre
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Casadocon ?C ?D)
	=>
	(printout t "Es nieto de " ?C crlf)
	(printout t "Es nieto de " ?D crlf)
)

(defrule nietapadre
	(insertanombre ?A)
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Casadocon ?C ?D)
	=>
	(printout t "Es nieta de " ?C crlf)
	(printout t "Es nieta de " ?D crlf)
)

(defrule nietomadre
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Casadocon ?D ?E)
	=>
	(printout t "Es nieto de " ?D crlf)
	(printout t "Es nieto de " ?E crlf)
)

(defrule nietamadre
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Casadocon ?D ?E)
	=>
	(printout t "Es nieta de " ?D crlf)
	(printout t "Es nieta de " ?E crlf)
)

(defrule abuelohijonieto
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?A ?B)
	(Hijode ?B ?C)
	=>
	(printout t "Es abuelo de " ?C crlf)
)

(defrule abuelohijanieto
	(insertanombre ?A)
	(Hombre ?A)	
	(Hijode ?A ?B)
	(Casadocon ?C ?B)
	(Hijode ?C ?D)
	=>
	(printout t "Es abuelo de " ?D crlf)
)

(defrule abuelahijonieto
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?B ?A)
	(Hijode ?B ?C)
	(Hijode ?C ?D)
	=>
	(printout t "Es abuela de " ?D crlf)
)

(defrule abuelahijanieto
	(insertanombre ?A)
	(Mujer ?A)
	(Casadocon ?B ?A)
	(Hijode ?B ?C)
	(Casadocon ?D ?C)
	(Hijode ?D ?E)
	=>
	(printout t "Es abuela de " ?E crlf)
)
	
(defrule tiohermano
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	(Hijode ?C ?D)
	=>
	(printout t "Es tio de " ?D crlf)
)

(defrule tiohermana
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	(Casadocon ?D ?C)
	(Hijode ?D ?E)
	=>
	(printout t "Es tio de " ?E crlf)
)

(defrule tiahermano
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	(Hijode ?C ?D)
	=>
	(printout t "Es tia de " ?D crlf)
)

(defrule tiahermana
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?B ?C & ~?A)
	(Casadocon ?D ?C)
	(Hijode ?D ?E)
	=>
	(printout t "Es tia de " ?E crlf)
)

(defrule tiopoliticohermano
	(insertanombre ?A) 
	(Hombre ?A)
	(Casadocon ?A ?X)
	(Hijode ?B ?X)
	(Hijode ?B ?C & ~?X)
	(Hijode ?C ?D)
	=>
	(printout t "Es tio politico de " ?D crlf)
)

(defrule tiopoliticohermana
	(insertanombre ?A) 
	(Hombre ?A)
	(Casadocon ?A ?X)
	(Hijode ?B ?X)
	(Hijode ?B ?C & ~?X)
	(Casadocon ?D ?C)
	(Hijode ?D ?E)
	=>
	(printout t "Es tio politico de " ?E crlf)
)

(defrule tiapoliticahermano
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?X ?A)
	(Hijode ?B ?X)
	(Hijode ?B ?C & ~?X)
	(Hijode ?C ?D)
	=>
	(printout t "Es tia politica de " ?D crlf)
)

(defrule tiapoliticahermana
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?X ?A)
	(Hijode ?B ?X)
	(Hijode ?B ?C & ~?X)
	(Casadocon ?D ?C)
	(Hijode ?D ?E)
	=>
	(printout t "Es tia politica de " ?E crlf)
)
	
(defrule sobrinopadre
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Casadocon ?D ?E)
	=>
	(printout t "Es sobrino de " ?D crlf)
	(printout t "Es sobrino de " ?E crlf)
)

(defrule sobrinapadre
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Casadocon ?D ?E)
	=>
	(printout t "Es sobrina de " ?D crlf)
	(printout t "Es sobrina de " ?E crlf)
)

(defrule sobrinomadre
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Casadocon ?E ?F)
	=>
	(printout t "Es sobrino de " ?E crlf)
	(printout t "Es sobrino de " ?F crlf)
)

(defrule sobrinamadre
	(insertanombre ?A) 
	(Mujer ?A)
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Casadocon ?E ?F)
	=>
	(printout t "Es sobrina de " ?E crlf)
	(printout t "Es sobrina de " ?F crlf)
)

(defrule primopadre
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Hijode ?D ?E)
	=>
	(printout t "Es primo de " ?E crlf)
)

(defrule primapadre
	(insertanombre ?A)
	(Mujer ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Hijode ?D ?E)
	=>
	(printout t "Es prima de " ?E crlf)
)
	
(defrule primopadrehijas
	(insertanombre ?A)
	(Hombre ?A)
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Casadocon ?E ?D)
	(Hijode ?E ?F)
	=>
	(printout t "Es primo/a por parte de padre de " ?F crlf)
)

(defrule primapadrehijas
	(insertanombre ?A)
	(Mujer ?A)	
	(Hijode ?B ?A)
	(Hijode ?C ?B)
	(Hijode ?C ?D & ~?B)
	(Casadocon ?E ?D)
	(Hijode ?E ?F)
	=>
	(printout t "Es prima por parte de padre de " ?F crlf)
)

(defrule primomadre
	(insertanombre ?A) 
	(Hombre ?A)	
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Hijode ?E ?F)
	=>
	(printout t "Es primo de " ?F crlf)
)

(defrule primamadre
	(insertanombre ?A)
	(Mujer ?A)	
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Hijode ?E ?F)
	=>
	(printout t "Es prima de " ?F crlf)
)

(defrule primomadrehijas
	(insertanombre ?A) 
	(Hombre ?A)	
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Casadocon ?F ?E)
	(Hijode ?F ?G)
	=>
	(printout t "Es primo de " ?G crlf)
)

(defrule primamadrehijas
	(insertanombre ?A) 
	(Mujer ?A)	
	(Hijode ?B ?A)
	(Casadocon ?B ?C)
	(Hijode ?D ?C)
	(Hijode ?D ?E & ~?C)
	(Casadocon ?F ?E)
	(Hijode ?F ?G)
	=>
	(printout t "Es prima de " ?G crlf)
)

(defrule yerno
	(insertanombre ?A) 
	(Hombre ?A)
	(Casadocon ?A ?B)
	(Hijode ?C ?B)
	(Casadocon ?C ?D)
	=>
	(printout t "Es yerno de " ?C crlf)
	(printout t "Es yerno de " ?D crlf)
)

(defrule nuera
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?B ?A)
	(Hijode ?C ?B)
	(Casadocon ?C ?D)
	=>
	(printout t "Es nuera de " ?C crlf)
	(printout t "Es nuera de " ?D crlf)
)
	
(defrule suegrohijo
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?A ?B)
	(Hombre ?B)
	(Casadocon ?B ?C)
	=>
	(printout t "Es suegro de " ?C crlf)
)

(defrule suegrohija
	(insertanombre ?A) 
	(Hombre ?A)
	(Hijode ?A ?B)
	(Mujer ?B)
	(Casadocon ?C ?B)
	=>
	(printout t "Es suegro de " ?C crlf)
)

(defrule suegrahijo
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?B ?A)
	(Hijode ?B ?C)
	(Hombre ?C)
	(Casadocon ?C ?D)
	=>
	(printout t "Es suegra de " ?D crlf)
)

(defrule suegrahija
	(insertanombre ?A) 
	(Mujer ?A)
	(Casadocon ?B ?A)
	(Hijode ?B ?C)
	(Mujer ?C)
	(Casadocon ?D ?C)
	=>
	(printout t "Es suegra de " ?D crlf)
)

; FIN DEL PROGRAMA