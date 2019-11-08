;; #############################
;; ## Definire valori globale ##
;; #############################

(defglobal ?*val-ulcer* = 65)
(defglobal ?*val-toxi* = 55)
(defglobal ?*val-apendicita* = 75)
(defglobal ?*val-enterocolita* = 60)
(defglobal ?*val-litiaza* = 60)
(defglobal ?*val-gastrita* = 65)
(defglobal ?*val-candidoza* = 50)


;; ####################################
;; ## Definire template-uri digestiv ##
;; ####################################

(deftemplate boala
	(slot denumire)
	(slot cf))

(deftemplate intrebari-d
	(slot intrebare)
	(slot id)
	(slot da)
	(slot nu))

(deftemplate intrebare-curenta
	(slot val))

(deftemplate opreste
	(slot stop))
	
(deftemplate ponderi-diagnostic
	(slot nume)
	(slot pondere))
	
(deftemplate ponderi-simptome
	(slot simptom)
	(slot boala)
	(slot pondere))

;; ###############################
;; ## Intrebari sistem digestiv ##
;; ###############################

(deffacts digestiv
	(intrebari-d (id 1) (intrebare "Aveti dureri in cavitatea bucala?") (da 2) (nu 2))
	(intrebari-d (id 2) (intrebare "Ati sesizat secretii albicioase pe limba?")(da 3) (nu 3))
	(intrebari-d (id 3) (intrebare "Aveti pete albe in cavitatea bucala?")(da 4) (nu 4))
	(intrebari-d (id 4) (intrebare "Intampinati dificultate la inghitire?")(da 5) (nu 5))
	(intrebari-d (id 5) (intrebare "Sunt prezente leziuni pe limba, cerul gurii, interiorul obrajilor sau gingii?")(da 6) (nu 6))
	(intrebari-d (id 6) (intrebare "Va confruntati cu dificultatea de a simti gustul alimentelor?")(da 36) (nu 36))	
	(intrebari-d (id 7) (intrebare "Resimtiti dureri in epigastru? (popular: capul pieptului)")(da 9) (nu 8))
	(intrebari-d (id 8) (intrebare "Aveti dureri in zona hipocondrului drept?")(da 9) (nu 26))
	(intrebari-d (id 9) (intrebare "Durerea apare dupa masa?")(da 10) (nu 10))
	(intrebari-d (id 10) (intrebare "Durerea este accentuata noaptea?")(da 26) (nu 26))
	(intrebari-d (id 11) (intrebare "Aveti dureri abdominale?")(da 12) (nu 15))
	(intrebari-d (id 12) (intrebare "Resimtiti durerile in partea dreapta a abdomenului?")(da 13) (nu 13))
	(intrebari-d (id 13) (intrebare "Aveti dureri la palparea abdomenului?")(da 14) (nu 14))
	(intrebari-d (id 14) (intrebare "Abdomenul este umflat?")(da 31) (nu 31))	
	(intrebari-d (id 15) (intrebare "Aveti dureri de cap?")(da 37) (nu 37))	
	(intrebari-d (id 16) (intrebare "Aveti stari de greata si voma?")(da 17) (nu 19))
	(intrebari-d (id 17) (intrebare "Starea de greata si voma se manifesta in special dupa masa?")(da 24) (nu 24))
	(intrebari-d (id 18) (intrebare "Aveti febra?")(da 11) (nu 11))
	(intrebari-d (id 19) (intrebare "Aveti diaree?")(da 20) (nu 20))
	(intrebari-d (id 20) (intrebare "Va simtiti balonat?")(da 21) (nu 21))
	(intrebari-d (id 21) (intrebare "V-a disparut pofta de mancare?")(da 22) (nu 22))
	(intrebari-d (id 22) (intrebare "Va simtiti epuizat?")(da 23) (nu 23))
	(intrebari-d (id 23) (intrebare "Ati sesizat aparitia unor zgomote intestinale puternice?")(da 28) (nu 28))	
	(intrebari-d (id 24) (intrebare "Ati sesizat o scadere in greutate?")(da 25) (nu 25))	
	(intrebari-d (id 25) (intrebare "Aveti parte de scaune de culoare negru inchis?")(da 19) (nu 19))
	(intrebari-d (id 26) (intrebare "Aveti parte de scaune deschise la culoare?") (da 27) (nu 27))
	(intrebari-d (id 27) (intrebare "Pielea si albul ochilor dumneavoastra a inceput sa se ingalbeneasca?")(da 34) (nu 34))
	(intrebari-d (id 28) (intrebare "Va simtiti deshidratat?")(da 29) (nu 29))
	(intrebari-d (id 29) (intrebare "Aveti ameteli?")(da 30) (nu 30))
	(intrebari-d (id 30) (intrebare "Va simtiti muschii slabiti?")(da 35) (nu 35))
	(intrebari-d (id 31) (intrebare "Aveti crampe abdominale?")(da 32) (nu 32))
	(intrebari-d (id 32) (intrebare "Simtiti durere cand urinati?")(da 15) (nu 15))
	(intrebari-d (id 33) (intrebare "Suferiti de indigestie?")(da 16) (nu 16))
	(intrebari-d (id 34) (intrebare "Urina dumneavoastra are o culoare inchisa?") (da 18) (nu 18))
	(intrebari-d (id 35) (intrebare "Diagnosticarea s-a incheiat."))
	(intrebari-d (id 36) (intrebare "Aveti rude care sufera de Ulcer Gastroduodenal sau Litiaza Biliara?")(da 7)(nu 7))
	(intrebari-d (id 37) (intrebare "Aveti senzatia de arsura la nivelul stomacului?")(da 33)(nu 33))

	(intreaba-d)
	(id-actual-d 1)
	(intrebare-curenta))

;; ###############################################
;; ## Reguli pentru aplicare intrebari Digestiv ##
;; ###############################################	
	
(defrule opreste
	(declare (salience 100))
	?p <- (stop)
	?n <- (id-actual-d ?id)
	(intrebari-d (id 35))
    =>
	(retract ?n)
	(retract ?p)
	(assert (id-actual-d 35))
	(assert (intreaba-d))
	(assert (opreste (stop da))))

(defrule pune-intrebare
	?e <- (intreaba-d)
	?n <- (id-actual-d ?id)
	?p <- (intrebare-curenta)
	(intrebari-d (id ?id) (intrebare ?val))
	=>
	(retract ?e)
	(retract ?p)
	(assert (intrebare-curenta (val ?val))))

(defrule raspuns-da
	?n <- (id-actual-d ?id)
	?o <- (raspuns da)
	(intrebari-d (id ?id) (da ?p))
    =>
	(retract ?n)
	(retract ?o)
	(assert (id-actual-d ?p))
	(assert (intreaba-d)))

(defrule raspuns-nu
  	?n <- (id-actual-d ?id)
	?o <- (raspuns nu)
	(intrebari-d (id ?id) (nu ?p))
    =>
	(retract ?n)
	(retract ?o)
	(assert (id-actual-d ?p))
	(assert (intreaba-d)))
	
(defrule eliminare-boli-cf-mic
	?f1 <- (boala (denumire ?) (cf ?y1))
	?f2 <- (boala (denumire ?) (cf ?y2))
	(test (> ?y1 ?y2))
	=>
	(retract ?f2))
	
;; ###############################################
;; ## Reguli pentru atribuire simptome Digestiv ##
;; ###############################################
	
(defrule dureri-cav-bucala-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 1))
	=>
	(assert (dureri-cavitatea-bucala da)))
	
(defrule dureri-cav-bucala-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 1))
	=>
	(assert (dureri-cavitatea-bucala nu)))
	
(defrule secretii-limba-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 2))
	=>
	(assert (secretii-limba da)))
	
(defrule secretii-limba-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 2))
	=>
	(assert (secretii-limba nu)))
	
(defrule pete-albe-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 3))
	=>
	(assert (pete-albe da)))
	
(defrule pete-albe-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 3))
	=>
	(assert (pete-albe nu)))
	
(defrule dificultate-inghitire-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 4))
	=>
	(assert (dificultate-inghitire da)))
	
(defrule dificultate-inghitire-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 4))
	=>
	(assert (dificultate-inghitire nu)))
	
(defrule leziuni-gura-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 5))
	=>
	(assert (leziuni-gura da)))
	
(defrule leziuni-gura-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 5))
	=>
	(assert (leziuni-gura nu)))
	
(defrule dificultate-gust-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 6))
	=>
	(assert (dificultate-gust da)))
	
(defrule dificultate-gust-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 6))
	=>
	(assert (dificultate-gust nu)))
	
(defrule durere-epigastru-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 7))
	=>
	(assert (durere-epigastru da)))
	
(defrule durere-epigastru-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 7))
	=>
	(assert (durere-epigastru nu)))
	
(defrule durere-hipocondru-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 8))
	=>
	(assert (durere-hipocondru-drept da)))
	
(defrule durere-hipocondru-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 8))
	=>
	(assert (durere-hipocondru-drept nu)))
	
(defrule durere-dupa-masa-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 9))
	=>
	(assert (durere-dupa-masa da)))
	
(defrule durere-dupa-masa-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 9))
	=>
	(assert (durere-dupa-masa nu)))
	
(defrule durere-noaptea-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 10))
	=>
	(assert (durere-noaptea da)))
	
(defrule durere-noaptea-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 10))
	=>
	(assert (durere-noaptea nu)))
	
(defrule durere-abdomen-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 11))
	=>
	(assert (durere-abdomen da)))
	
(defrule durere-abdomen-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 11))
	=>
	(assert (durere-abdomen nu)))
	
(defrule durere-abdomen-dreapta-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 12))
	=>
	(assert (durere-abdomen-dreapta da)))
	
(defrule durere-abdomen-dreapta-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 12))
	=>
	(assert (durere-abdomen-dreapta nu)))

(defrule durere-abdomen-palpare-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 13))
	=>
	(assert (durere-abdomen-palpare da)))
	
(defrule durere-abdomen-palpare-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 13))
	=>
	(assert (durere-abdomen-palpare nu)))
	
(defrule abdomen-umflat-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 14))
	=>
	(assert (abdomen-umflat da)))
	
(defrule abdomen-umflat-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 14))
	=>
	(assert (abdomen-umflat nu)))
	
(defrule durere-cap-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 15))
	=>
	(assert (durere-cap da)))
	
(defrule durere-cap-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 15))
	=>
	(assert (durere-cap nu)))
	
(defrule greata-voma-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 16))
	=>
	(assert (greata-voma da)))
	
(defrule greata-voma-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 16))
	=>
	(assert (greata-voma nu)))
	
(defrule greata-voma-dupa-masa-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 17))
	=>
	(assert (greata-voma-dupa-masa da)))
	
(defrule greata-voma-dupa-masa-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 17))
	=>
	(assert (greata-voma-dupa-masa nu)))
	
(defrule febra-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 18))
	=>
	(assert (febra da)))
	
(defrule febra-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 18))
	=>
	(assert (febra nu)))
	
(defrule diaree-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 19))
	=>
	(assert (diaree da)))
	
(defrule diaree-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 19))
	=>
	(assert (diaree nu)))
	
(defrule balonare-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 20))
	=>
	(assert (balonare da)))
	
(defrule balonare-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 20))
	=>
	(assert (balonare nu)))
	
(defrule lipsa-pofta-mancare-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 21))
	=>
	(assert (lipsa-pofta-mancare da)))
	
(defrule lipsa-pofta-mancare-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 21))
	=>
	(assert (lipsa-pofta-mancare nu)))
	
(defrule epuizare-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 22))
	=>
	(assert (epuizare da)))
	
(defrule epuizare-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 22))
	=>
	(assert (epuizare nu)))
	
(defrule zgomote-intestinale-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 23))
	=>
	(assert (zgomote-intestinale da)))
	
(defrule zgomote-intestinale-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 23))
	=>
	(assert (zgomote-intestinale nu)))
	
(defrule scadere-greutate-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 24))
	=>
	(assert (scadere-greutate da)))
	
(defrule scadere-greutate-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 24))
	=>
	(assert (scadere-greutate nu)))
	
(defrule scaune-inchise-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 25))
	=>
	(assert (scaune-inchise da)))
	
(defrule scaune-inchise-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 25))
	=>
	(assert (scaune-inchise nu)))
	
(defrule scaune-deschise-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 26))
	=>
	(assert (scaune-deschise da)))
	
(defrule scaune-deschise-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 26))
	=>
	(assert (scaune-deschise nu)))
	
(defrule icter-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 27))
	=>
	(assert (icter da)))
	
(defrule icter-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 27))
	=>
	(assert (icter nu)))
	
(defrule deshidratare-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 28))
	=>
	(assert (deshidratare da)))
	
(defrule deshidratare-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 28))
	=>
	(assert (deshidratare nu)))
	
(defrule ameteli-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 29))
	=>
	(assert (ameteli da)))
	
(defrule ameteli-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 29))
	=>
	(assert (ameteli nu)))
	
(defrule slabire-musculara-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 30))
	=>
	(assert (slabire-musculara da)))
	
(defrule slabire-musculara-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 30))
	=>
	(assert (slabire-musculara nu)))
	
(defrule crampe-abdominale-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 31))
	=>
	(assert (crampe-abdominale da)))
	
(defrule crampe-abdominale-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 31))
	=>
	(assert (crampe-abdominale nu)))
	
(defrule urinari-dureroase-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 32))
	=>
	(assert (urinari-dureroase da)))
	
(defrule urinari-dureroase-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 32))
	=>
	(assert (urinari-dureroase nu)))
	
(defrule indigestie-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 33))
	=>
	(assert (indigestie da)))
	
(defrule indigestie-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 33))
	=>
	(assert (indigestie nu)))
	
(defrule urina-inchisa-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 34))
	=>
	(assert (urina-inchisa da)))

(defrule urina-inchisa-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 34))
	=>
	(assert (urina-inchisa nu)))

(defrule sansa-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 36))
	=>
	(assert (sansaUlcerLitiaza)))
	
(defrule arsuri-da
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns da)
	(test (eq ?id 37))
	=>
	(assert (arsuri da)))

(defrule arsuri-nu
	(declare (salience 99))
	?n <- (id-actual-d ?id)
	(raspuns nu)
	(test (eq ?id 37))
	=>
	(assert (arsuri nu)))
	
;; #####################################
;; ## Reguli pentru atribuire ponderi ##
;; #####################################


(defrule p1
	(dureri-cavitatea-bucala da)
	=>
	(assert (ponderi-simptome(simptom dureri-cavitatea-bucala-da)(boala Candidoza-Bucala)(pondere 8))))
	
(defrule p2
	(dureri-cavitatea-bucala nu)
	=>
	(assert (ponderi-simptome(simptom dureri-cavitatea-bucala-nu)(boala Candidoza-Bucala)(pondere 0))))

(defrule p3
	(secretii-limba da)
	=>
	(assert (ponderi-simptome(simptom secretii-limba-da)(boala Candidoza-Bucala)(pondere 10))))
	
(defrule p4
	(secretii-limba nu)
	=>
	(assert (ponderi-simptome(simptom secretii-limba-nu)(boala Candidoza-Bucala)(pondere 0))))
	
(defrule p5
	(pete-albe da)
	=>
	(assert (ponderi-simptome(simptom pete-albe-da)(boala Candidoza-Bucala)(pondere 10))))
	
(defrule p6
	(pete-albe nu)
	=>
	(assert (ponderi-simptome(simptom pete-albe-nu)(boala Candidoza-Bucala)(pondere 0))))
	
(defrule p7
	(dificultate-inghitire da)
	=>
	(assert (ponderi-simptome(simptom dificultate-inghitire-da)(boala Candidoza-Bucala)(pondere 6))))
	
(defrule p8
	(dificultate-inghitire nu)
	=>
	(assert (ponderi-simptome(simptom dificultate-inghitire-nu)(boala Candidoza-Bucala)(pondere 0))))
	
(defrule p9
	(leziuni-gura da)
	=>
	(assert (ponderi-simptome(simptom leziuni-gura-da)(boala Candidoza-Bucala)(pondere 10))))
	
(defrule p10
	(leziuni-gura nu)
	=>
	(assert (ponderi-simptome(simptom leziuni-gura-nu)(boala Candidoza-Bucala)(pondere 0))))
	
(defrule p11
	(dificultate-gust da)
	=>
	(assert (ponderi-simptome(simptom dificultate-gust-da)(boala Candidoza-Bucala)(pondere 6))))
	
(defrule p12
	(dificultate-gust nu)
	=>
	(assert (ponderi-simptome(simptom dificultate-gust-nu)(boala Candidoza-Bucala)(pondere 0))))

(defrule p13
	(durere-epigastru da)
	=>
	(assert (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere 10)))
	(assert (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere 9)))
	(assert (ponderi-simptome(simptom durere-epigastru-da)(boala Litiaza-Biliara)(pondere 10))))

(defrule p14
	(durere-hipocondru-drept da)
	=>
	(assert (ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere 10)))
	(assert (ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Litiaza-Biliara)(pondere 10))))
	
(defrule p15
	(durere-epigastru nu)
	=>
	(assert (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-epigastru-nu)(boala Ulcer-Gastroduodenal)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-epigastru-nu)(boala Litiaza-Biliara)(pondere 0))))

(defrule p16
	(durere-hipocondru-drept nu)
	=>
	(assert (ponderi-simptome(simptom durere-hipocondru-drept-nu)(boala Ulcer-Gastroduodenal)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-hipocondru-drept-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p17
	(durere-dupa-masa da)
	=>
	(assert (ponderi-simptome(simptom durere-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere 9)))
	(assert (ponderi-simptome(simptom durere-dupa-masa-da)(boala Litiaza-Biliara)(pondere 8))))
	
(defrule p18
	(durere-dupa-masa nu)
	=>
	(assert (ponderi-simptome(simptom durere-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-dupa-masa-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p19
	(durere-noaptea da)
	=>
	(assert (ponderi-simptome(simptom durere-noaptea-da)(boala Litiaza-Biliara)(pondere 8))))
	
(defrule p20
	(durere-noaptea nu)
	=>
	(assert (ponderi-simptome(simptom durere-noaptea-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p21
	(febra da)
	=>
	(assert (ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere 7)))
	(assert (ponderi-simptome(simptom febra-da)(boala Apendicita)(pondere 8)))
	(assert (ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere 5))))
	
(defrule p22
	(febra nu)
	=>
	(assert (ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere 0)))
	(assert (ponderi-simptome(simptom febra-nu)(boala Apendicita)(pondere 0)))
	(assert (ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p23
	(urina-inchisa da)
	=>
	(assert (ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere 7))))
	
(defrule p24
	(urina-inchisa nu)
	=>
	(assert (ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p25
	(icter da)
	=>
	(assert (ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere 7))))
	
(defrule p26
	(icter nu)
	=>
	(assert (ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p27
	(scaune-deschise da)
	=>
	(assert (ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere 5))))
	
(defrule p28
	(scaune-deschise nu)
	=>
	(assert (ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere 0))))
	
(defrule p29
	(scaune-inchise da)
	=>
	(assert (ponderi-simptome(simptom scaune-inchise-da)(boala Ulcer-Gastroduodenal)(pondere 6))))
	
(defrule p30
	(scaune-inchise nu)
	=>
	(assert (ponderi-simptome(simptom scaune-inchise-nu)(boala Ulcer-Gastroduodenal)(pondere 0))))
	
(defrule p31
	(lipsa-pofta-mancare da)
	=>
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Enterocolita)(pondere 7)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere 7)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Apendicita)(pondere 8)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Ulcer-Gastroduodenal)(pondere 7))))
	
(defrule p32
	(lipsa-pofta-mancare nu)
	=>
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Enterocolita)(pondere 0)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere 0)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Apendicita)(pondere 0)))
	(assert (ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Ulcer-Gastroduodenal)(pondere 0))))
	
(defrule p33
	(balonare da)
	=>
	(assert (ponderi-simptome(simptom balonare-da)(boala Enterocolita)(pondere 8)))
	(assert (ponderi-simptome(simptom balonare-da)(boala Gastrita)(pondere 7)))
	(assert (ponderi-simptome(simptom balonare-da)(boala Apendicita)(pondere 8)))
	(assert (ponderi-simptome(simptom balonare-da)(boala Ulcer-Gastroduodenal)(pondere 8))))
	
(defrule p34
	(balonare nu)
	=>
	(assert (ponderi-simptome(simptom balonare-nu)(boala Enterocolita)(pondere 0)))
	(assert (ponderi-simptome(simptom balonare-nu)(boala Gastrita)(pondere 0)))
	(assert (ponderi-simptome(simptom balonare-nu)(boala Apendicita)(pondere 0)))
	(assert (ponderi-simptome(simptom balonare-nu)(boala Ulcer-Gastroduodenal)(pondere 0))))
	
(defrule p35
	(greata-voma-dupa-masa da)
	=>
	(assert (ponderi-simptome(simptom greata-voma-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere 8))))
	
(defrule p36
	(greata-voma-dupa-masa nu)
	=>
	(assert (ponderi-simptome(simptom greata-voma-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere 0))))
	
(defrule p37
	(scadere-greutate da)
	=>
	(assert (ponderi-simptome(simptom scadere-greutate-da)(boala Ulcer-Gastroduodenal)(pondere 7))))
	
(defrule p38
	(scadere-greutate nu)
	=>
	(assert (ponderi-simptome(simptom scadere-greutate-nu)(boala Ulcer-Gastroduodenal)(pondere 0))))
	
(defrule p39
	(epuizare da)
	=>
	(assert (ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere 9))))
	
(defrule p40
	(epuizare nu)
	=>
	(assert (ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere 0))))
	
(defrule p41
	(greata-voma da)
	=>
	(assert (ponderi-simptome(simptom greata-voma-da)(boala Gastrita)(pondere 8)))
	(assert (ponderi-simptome(simptom greata-voma-da)(boala Toxiinfectie-Alimentara)(pondere 9)))
	(assert (ponderi-simptome(simptom greata-voma-da)(boala Enterocolita)(pondere 9))))
	
(defrule p42
	(greata-voma nu)
	=>
	(assert (ponderi-simptome(simptom greata-voma-nu)(boala Gastrita)(pondere 0)))
	(assert (ponderi-simptome(simptom greata-voma-nu)(boala Toxiinfectie-Alimentara)(pondere 0)))
	(assert (ponderi-simptome(simptom greata-voma-nu)(boala Enterocolita)(pondere 0))))
	
(defrule p43
	(zgomote-intestinale da)
	=>
	(assert (ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere 10))))
	
(defrule p44
	(zgomote-intestinale nu)
	=>
	(assert (ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere 0))))
	
(defrule p45
	(diaree da)
	=>
	(assert (ponderi-simptome(simptom diaree-da)(boala Toxiinfectie-Alimentara)(pondere 10)))
	(assert (ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere 10))))
	
(defrule p46
	(diaree nu)
	=>
	(assert (ponderi-simptome(simptom diaree-nu)(boala Toxiinfectie-Alimentara)(pondere 0)))
	(assert (ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere 0))))
	
(defrule p47
	(durere-cap da)
	=>
	(assert (ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere 6))))
	
(defrule p48
	(durere-cap nu)
	=>
	(assert (ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere 0))))
	
(defrule p49
	(indigestie da)
	=>
	(assert (ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere 9))))

(defrule p50
	(indigestie nu)
	=>
	(assert (ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere 0))))
	
(defrule p51
	(arsuri da)
	=>
	(assert (ponderi-simptome(simptom arsuri-da)(boala Gastrita)(pondere 10))))
	
(defrule p52
	(arsuri nu)
	=>
	(assert (ponderi-simptome(simptom arsuri-nu)(boala Gastrita)(pondere 0))))
	
(defrule p53
	(durere-abdomen da)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere 10)))
	(assert (ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere 9)))
	(assert (ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere 8))))
	
(defrule p54
	(durere-abdomen nu)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-nu)(boala Apendicita)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere 0)))
	(assert (ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere 0))))
	
(defrule p55
	(durere-abdomen-dreapta da)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere 10))))
	
(defrule p56
	(durere-abdomen-dreapta nu)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere 0))))
	
(defrule p57
	(durere-abdomen-palpare da)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere 10))))
	
(defrule p58
	(durere-abdomen-palpare nu)
	=>
	(assert (ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere 0))))
	
(defrule p59
	(abdomen-umflat da)
	=>
	(assert (ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere 9))))
	
(defrule p60
	(abdomen-umflat nu)
	=>
	(assert (ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere 0))))
	
(defrule p61
	(crampe-abdominale da)
	=>
	(assert (ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere 8))))
	
(defrule p62
	(crampe-abdominale nu)
	=>
	(assert (ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere 0))))
	
(defrule p63
	(urinari-dureroase da)
	=>
	(assert (ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere 4))))
	
(defrule p64
	(urinari-dureroase nu)
	=>
	(assert (ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere 0))))
	
(defrule p65
	(ameteli da)
	=>
	(assert (ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere 9))))
	
(defrule p66
	(ameteli nu)
	=>
	(assert (ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere 0))))
	
(defrule p67
	(slabire-musculara da)
	=>
	(assert (ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere 9))))
	
(defrule p68
	(slabire-musculara nu)
	=>
	(assert (ponderi-simptome(simptom slabire-musculara-nu)(boala Toxiinfectie-Alimentara)(pondere 0))))
	
(defrule p69
	(deshidratare da)
	=>
	(assert (ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere 10))))
	
(defrule p70
	(deshidratare nu)
	=>
	(assert (ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere 0))))

	
;; ##########################################
;; ## Reguli pentru diagnosticare Digestiv ##
;; ##########################################

(defrule r1
	(ponderi-simptome(simptom dureri-cavitatea-bucala-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza)(pondere (+ ?a ?b ?c)))))
	
(defrule r2
	(ponderi-simptome(simptom dureri-cavitatea-bucala-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r3
	(ponderi-simptome(simptom dureri-cavitatea-bucala-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r4
	(ponderi-simptome(simptom dureri-cavitatea-bucala-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r5
	(ponderi-simptome(simptom dureri-cavitatea-bucala-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r6
	(ponderi-simptome(simptom dureri-cavitatea-bucala-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r7
	(ponderi-simptome(simptom dureri-cavitatea-bucala-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))

(defrule r8
	(ponderi-simptome(simptom dureri-cavitatea-bucala-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom secretii-limba-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom pete-albe-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza) (pondere (+ ?a ?b ?c)))))
	
(defrule r9
	(ponderi-simptome(simptom dificultate-inghitire-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r10
	(ponderi-simptome(simptom dificultate-inghitire-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r11
	(ponderi-simptome(simptom dificultate-inghitire-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r12
	(ponderi-simptome(simptom dificultate-inghitire-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r13
	(ponderi-simptome(simptom dificultate-inghitire-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-da)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r14
	(ponderi-simptome(simptom dificultate-inghitire-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-da)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r15
	(ponderi-simptome(simptom dificultate-inghitire-da)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r16
	(ponderi-simptome(simptom dificultate-inghitire-nu)(boala Candidoza-Bucala)(pondere ?a))
	(ponderi-simptome(simptom leziuni-gura-nu)(boala Candidoza-Bucala)(pondere ?b))
	(ponderi-simptome(simptom dificultate-gust-nu)(boala Candidoza-Bucala)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Candidoza+) (pondere (+ ?a ?b ?c)))))
	
(defrule r17
	(ponderi-diagnostic (nume Candidoza) (pondere ?p1))
	(ponderi-diagnostic (nume Candidoza+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-candidoza*) 100)) 30))
	=>
	(assert (stop))
	(assert (boala (denumire Candidoza-Bucala) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-candidoza*) 100))))))
	
(defrule r18
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Litiaza-Biliara)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Litiaza-Biliara)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom durere-noaptea-da)(boala Litiaza-Biliara)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Litiaza) (pondere (+ ?a ?b ?c)))))
	
(defrule r19
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Litiaza-Biliara)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-nu)(boala Litiaza-Biliara)(pondere ?a)))
	=>
	(assert (ponderi-diagnostic (nume Litiaza) (pondere ?a))))

(defrule r20
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Litiaza-Biliara)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Litiaza-Biliara)(pondere ?a)))
	(or (and (ponderi-simptome(simptom durere-dupa-masa-nu)(boala Litiaza-Biliara)(pondere ?b))
			(ponderi-simptome(simptom durere-noaptea-da)(boala Litiaza-Biliara)(pondere ?c))))
	(or (and (ponderi-simptome(simptom durere-dupa-masa-da)(boala Litiaza-Biliara)(pondere ?b))
			(ponderi-simptome(simptom durere-noaptea-nu)(boala Litiaza-Biliara)(pondere ?c))))
	=>
	(assert (ponderi-diagnostic (nume Litiaza) (pondere (+ ?a ?b ?c)))))
	
(defrule r22
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r23
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r24
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r25
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r26
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r27
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r28
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r29
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere 20))))
	
(defrule r30
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r31
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r32
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r33
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-da)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r34
	(ponderi-simptome(simptom urina-inchisa-da)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r35
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-da)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r36
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-da)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r37
	(ponderi-simptome(simptom urina-inchisa-nu)(boala Litiaza-Biliara)(pondere ?a))
	(ponderi-simptome(simptom icter-nu)(boala Litiaza-Biliara)(pondere ?b))
	(ponderi-simptome(simptom scaune-deschise-nu)(boala Litiaza-Biliara)(pondere ?c))
	(ponderi-simptome(simptom febra-nu)(boala Litiaza-Biliara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Litiaza+) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r38
	(ponderi-diagnostic (nume Litiaza) (pondere ?p1))
	(ponderi-diagnostic (nume Litiaza+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-litiaza*) 100)) 30))
	=>
	(assert (stop))
	(assert (boala (denumire Litiaza-Biliara) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-litiaza*) 100))))))
	
(defrule r39
	(sansaUlcerLitiaza)
	?f <- (boala (denumire Litiaza-Biliara)(cf ?x))
	(test (< ?x 90))
	=>
	(modify ?f (cf (+ ?x 10))))
	
(defrule r40
	(sansaUlcerLitiaza)
	?f <- (boala (denumire Ulcer-Gastroduodenal)(cf ?x))
	(test (< ?x 90))
	=>
	(modify ?f (cf (+ ?x 10))))
	
(defrule r41
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r42
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r43
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r44
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r45
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r46
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r47
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r48
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r49
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r50
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r51
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r52
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r53
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere 50))))
	
(defrule r54
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r55
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r56
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r57
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r58
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r59
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r60
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r61
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r62
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r63
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r64
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r65
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r66
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r67
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r68
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-da)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r69
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-da)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r70
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-da)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r71
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-da)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r72
	(ponderi-simptome(simptom durere-abdomen-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom abdomen-umflat-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom durere-abdomen-palpare-nu)(boala Apendicita)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-dreapta-nu)(boala Apendicita)(pondere ?d))
	(ponderi-simptome(simptom crampe-abdominale-nu)(boala Apendicita)(pondere ?e))
	(ponderi-simptome(simptom urinari-dureroase-nu)(boala Apendicita)(pondere ?f))
	=>
	(assert (ponderi-diagnostic (nume Apendicita) (pondere (+ ?a ?b ?c ?d ?e ?f)))))
	
(defrule r73
	(ponderi-simptome(simptom febra-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-da)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r74
	(ponderi-simptome(simptom febra-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-nu)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r75
	(ponderi-simptome(simptom febra-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-da)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r76
	(ponderi-simptome(simptom febra-nu)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-da)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r77
	(ponderi-simptome(simptom febra-nu)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-da)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r78
	(ponderi-simptome(simptom febra-nu)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-nu)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r79
	(ponderi-simptome(simptom febra-da)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-nu)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r80
	(ponderi-simptome(simptom febra-nu)(boala Apendicita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Apendicita)(pondere ?b))
	(ponderi-simptome(simptom balonare-nu)(boala Apendicita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Apendicita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r81
	(ponderi-diagnostic (nume Apendicita) (pondere ?p1))
	(ponderi-diagnostic (nume Apendicita+) (pondere ?p2))
	(test ( > (integer (* (/ (+ ?p1 ?p2) ?*val-apendicita*) 100)) 30))
	=>
	(assert (stop))
	(assert (boala (denumire Apendicita)(cf (integer (* (/ (+ ?p1 ?p2) ?*val-apendicita*) 100))))))
	
(defrule r82
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))

(defrule r83
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r84
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r85
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r86
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r87
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r88
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r89
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r90
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r91
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r92
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r93
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-nu)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-nu)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r94
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r95
	(ponderi-simptome(simptom durere-cap-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r96
	(ponderi-simptome(simptom durere-cap-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom indigestie-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Gastrita)(pondere ?c))
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Gastrita)(pondere ?d))
		(ponderi-simptome(simptom durere-abdomen-da)(boala Gastrita)(pondere ?d)))
	=>
	(assert (ponderi-diagnostic (nume Gastrita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r97
	(ponderi-simptome(simptom greata-voma-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-da)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r98
	(ponderi-simptome(simptom greata-voma-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-nu)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r99
	(ponderi-simptome(simptom greata-voma-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-nu)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r100
	(ponderi-simptome(simptom greata-voma-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-da)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r101
	(ponderi-simptome(simptom greata-voma-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-nu)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r102
	(ponderi-simptome(simptom greata-voma-da)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-da)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r103
	(ponderi-simptome(simptom greata-voma-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-da)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r104
	(ponderi-simptome(simptom greata-voma-nu)(boala Gastrita)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Gastrita)(pondere ?b))
	(ponderi-simptome(simptom arsuri-nu)(boala Gastrita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Gastrita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r105
	(ponderi-diagnostic (nume Gastrita) (pondere ?p1))
	(ponderi-diagnostic (nume Gastrita+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-gastrita*) 100)) 30))
	=>
	(assert (stop))
	(assert (boala (denumire Gastrita) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-gastrita*) 100))))))
	
(defrule r21
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-da)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r106
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-nu)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r107
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-nu)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r108
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-da)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r109
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-nu)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r110
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-da)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r111
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-da)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r112
	(or (ponderi-simptome(simptom durere-epigastru-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
		(ponderi-simptome(simptom durere-hipocondru-drept-da)(boala Ulcer-Gastroduodenal)(pondere ?a)))
	(ponderi-simptome(simptom durere-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-dupa-masa-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	(ponderi-simptome(simptom scaune-inchise-nu)(boala Ulcer-Gastroduodenal)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Ulcer) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r113
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))

(defrule r114
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))

(defrule r115
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r116
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r117
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r118
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r119
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-da)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-da)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r120
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Ulcer-Gastroduodenal)(pondere ?a))
	(ponderi-simptome(simptom balonare-nu)(boala Ulcer-Gastroduodenal)(pondere ?b))
	(ponderi-simptome(simptom scadere-greutate-nu)(boala Ulcer-Gastroduodenal)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Ulcer+) (pondere (+ ?a ?b ?c)))))
	
(defrule r121
	(ponderi-diagnostic (nume Ulcer) (pondere ?p1))
	(ponderi-diagnostic (nume Ulcer+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-ulcer*) 100)) 30))
	=>
	(assert (stop))
	(assert (boala (denumire Ulcer-Gastroduodenal) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-ulcer*) 100))))))
	
(defrule r122
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r123
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r124
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r125
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r126
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r127
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r128
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r129
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r130
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r131
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r132
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r133
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r134
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r135
	(ponderi-simptome(simptom zgomote-intestinale-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r136
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-da)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-da)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r137
	(ponderi-simptome(simptom zgomote-intestinale-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom epuizare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom febra-nu)(boala Enterocolita)(pondere ?c))
	(ponderi-simptome(simptom diaree-nu)(boala Enterocolita)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r138
	(ponderi-simptome(simptom balonare-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-da)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r139
	(ponderi-simptome(simptom balonare-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-nu)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r140
	(ponderi-simptome(simptom balonare-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-nu)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r141
	(ponderi-simptome(simptom balonare-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-da)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r142
	(ponderi-simptome(simptom balonare-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-nu)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r143
	(ponderi-simptome(simptom balonare-da)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-da)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r144
	(ponderi-simptome(simptom balonare-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-da)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-da)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r145
	(ponderi-simptome(simptom balonare-nu)(boala Enterocolita)(pondere ?a))
	(ponderi-simptome(simptom lipsa-pofta-mancare-nu)(boala Enterocolita)(pondere ?b))
	(ponderi-simptome(simptom greata-voma-nu)(boala Enterocolita)(pondere ?c))
	=>
	(assert (ponderi-diagnostic (nume Enterocolita+) (pondere (+ ?a ?b ?c)))))
	
(defrule r146
	(ponderi-diagnostic (nume Enterocolita) (pondere ?p1))
	(ponderi-diagnostic (nume Enterocolita+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-enterocolita*) 100)) 20))
	=>
	(assert (stop))
	(assert (boala (denumire Enterocolita) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-enterocolita*) 100))))))
	
(defrule r147
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r148
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r149
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r150
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r151
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r152
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r153
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r154
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r155
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r156
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r157
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r158
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r159
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r160
	(ponderi-simptome(simptom deshidratare-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r161
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-da)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-da)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r162
	(ponderi-simptome(simptom deshidratare-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom slabire-musculara-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	(ponderi-simptome(simptom ameteli-nu)(boala Toxiinfectie-Alimentara)(pondere ?c))
	(ponderi-simptome(simptom durere-abdomen-nu)(boala Toxiinfectie-Alimentara)(pondere ?d))
	=>
	(assert (ponderi-diagnostic (nume Infectie) (pondere (+ ?a ?b ?c ?d)))))
	
(defrule r163
	(ponderi-simptome(simptom diaree-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom greata-voma-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	=>
	(assert (ponderi-diagnostic (nume Infectie+) (pondere (+ ?a ?b)))))
	
(defrule r164
	(ponderi-simptome(simptom diaree-da)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom greata-voma-nu)(boala Toxiinfectie-Alimentara)(pondere ?b))
	=>
	(assert (ponderi-diagnostic (nume Infectie+) (pondere (+ ?a ?b)))))
	
(defrule r165
	(ponderi-simptome(simptom diaree-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom greata-voma-da)(boala Toxiinfectie-Alimentara)(pondere ?b))
	=>
	(assert (ponderi-diagnostic (nume Infectie+) (pondere (+ ?a ?b)))))
	
(defrule r166
	(ponderi-simptome(simptom diaree-nu)(boala Toxiinfectie-Alimentara)(pondere ?a))
	(ponderi-simptome(simptom greata-voma-nu)(boala Toxiinfectie-Alimentara)(pondere ?b))
	=>
	(assert (ponderi-diagnostic (nume Infectie+) (pondere (+ ?a ?b)))))
	
(defrule r167
	(ponderi-diagnostic (nume Infectie) (pondere ?p1))
	(ponderi-diagnostic (nume Infectie+) (pondere ?p2))
	(test (> (integer (* (/ (+ ?p1 ?p2) ?*val-toxi*) 100)) 20))
	=>
	(assert (stop))
	(assert (boala (denumire Toxiinfectie-Alimentara) (cf (integer (* (/ (+ ?p1 ?p2) ?*val-toxi*) 100))))))
	
(defrule r168
	(dureri-cavitatea-bucala nu)
	(secretii-limba nu)
	(pete-albe nu)
	(dificultate-inghitire nu)
	(leziuni-gura nu)
	(dificultate-gust nu)
	(durere-epigastru nu)
	(durere-hipocondru-drept nu)
	(scaune-deschise nu)
	(icter nu)
	(urina-inchisa nu)
	(febra nu)
	(durere-abdomen nu)
	(durere-cap nu)
	(arsuri nu)
	(indigestie nu)
	(greata-voma nu)
	(diaree nu)
	(balonare nu)
	(lipsa-pofta-mancare nu)
	(epuizare nu)
	(zgomote-intestinale nu)
	(deshidratare nu)
	(ameteli nu)
	(slabire-musculara nu)
	=>
	(assert (stop))
	(assert (boala (denumire Sanatos) (cf 100))))