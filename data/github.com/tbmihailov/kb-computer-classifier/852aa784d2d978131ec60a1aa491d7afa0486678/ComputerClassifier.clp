;===================================
;=======Computer classifier=========
;======Todor Mihaylov==M24425=======
;===================================
;=======KNOLEDGEBASE==COURSE========
;===================================

	
;=====Functions - Ask Questions=====
(deffunction ask-question (?question $?allowed-values)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(if (lexemep ?answer) then
		(bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) then
			(bind ?answer (lowcase ?answer))))
	?answer)
   
(deffunction ask-question-number-ans (?question)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(while (not (numberp ?answer)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read)))
	?answer)

(deffunction ask-question-integer-ans (?question)
	(printout t crlf)
	(printout t ?question)
	(printout t crlf)
	(bind ?answer (read))
	(while (not (integerp ?answer)) do
		(printout t crlf)
		(printout t ?question)
		(bind ?answer (read)))
	?answer)

(deffunction ask-question-integer-ans-in-bounds (?question ?lower-bound ?upper-bound)
	(bind ?answer
		  (ask-question-integer-ans ?question))
	(while (not (<= ?lower-bound ?answer ?upper-bound))
		(bind ?answer
			  (ask-question-integer-ans ?question)))
	?answer)

(deffunction yes-or-no-p (?question)
	(bind ?response (ask-question ?question yes no y n))
	(if (or (eq ?response yes) (eq ?response y)) then
		TRUE
	else
		FALSE))
		
;===== Fact rules========

	
;===Computer type
(defrule check-comp-type "Type: Laptop(L), Desktop(D), Rack(R)"
    =>
    (bind ?response
          (ask-question "What is the computer type: Laptop-(L), Desktop-(D), Rack-(R):"
                        L l D d R r))
    (if (or (eq ?response L) (eq ?response l)) then
        (assert (comptype Laptop))
    else
        (if (or (eq ?response D) (eq ?response d)) then
            (assert (comptype Desktop))
        else
            (assert (comptype Rack))))
    (assert (checked comptype)))

;===Processor frequency
(defrule check-processor-freq "processor frequency : Slow Normal Fast"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the processor frequency? (between 0 MHZ and 10 000 MHz):"
                                              0 10000))
    (if (>= ?response 2800) then
        (assert (frequency High))
    else
        (if (>= ?response 1400) then
            (assert (frequency Medium))
        else
            (assert (frequency Low))))
    (assert (checked frequency)))
	
;===Processor count
(defrule check-processor-count "processor count : 1,1-3,4+"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the processors count? 1-128:"
                                              1 128))
    (if (>= ?response 4) then
        (assert (proccount 0-1))
    else
        (if (>= ?response 2) then
            (assert (proccount 2-3))
        else
            (assert (proccount 4+))))
    (assert (checked proccount)))
	
;===RAM
(defrule check-ram "ram : Low Medium High"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the operation memory(RAM)? 0-128000 MB:"
                                              1 128000))
    (if (>= ?response 8000) then
        (assert (ram High))
    else
        (if (>= ?response 2000) then
            (assert (ram Medium))			
			else
				(assert (ram Low))))
    (assert (checked ram)))	
	
;==HDD Capacity	
(defrule check-hddcapacity "HDD Capacity : Low Medium High"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the HDD Capacity? 0-16000 GB :"
                                              1 16000))
    (if (>= ?response 600) then
        (assert (hddcapacity High))
    else
        (if (>= ?response 250) then
            (assert (hddcapacity Medium))			
			else
				(assert (hddcapacity Low))))
    (assert (checked hddcapacity)))	
	
;===HDD Speed	
(defrule check-hddspeed "HDD speed: Slow(s), Medium(m), Fast(f)"
    =>
    (bind ?response
          (ask-question "What is the HDD speed: Slow(s), Medium(m), Fast(f):"
                        s S m M f F ))
   (if (or (eq ?response S) (eq ?response s)) then
        (assert (hddspeed Slow))
    else
       (if (or (eq ?response M) (eq ?response m)) then
             (assert (hddspeed Medium))
        else
            (assert (hddspeed Fast))))
    (assert (checked hddspeed)))

;===Video power
(defrule check-videopower "video power: Low(l), Medium(m), High(h)"
    =>
    (bind ?response
          (ask-question "What is the video power: Low(l), Medium(m), High(h)"
                        l L m M h H ))
   (if (or (eq ?response L) (eq ?response l)) then
        (assert (videopower Low))
    else
       (if (or (eq ?response M) (eq ?response m)) then
             (assert (videopower Medium))
        else
            (assert (videopower High))))
    (assert (checked videopower)))


;===Monitor size
(defrule check-monitor "monitor size: Small, Medium, Large"
    =>
    (bind ?response
          (ask-question-integer-ans-in-bounds "What is the Monitor size? 1-85 inches:"
                                              1 85))
    (if (>= ?response 21) then
        (assert (monitor Large))
    else
        (if (>= ?response 15) then
            (assert (monitor Medium))
        else
            (assert (monitor Small))))
    (assert (checked monitor)))
	
;===Cooling
(defrule check-cooling "computer cooling: Normal(n), Good(g), Extra(e)"
    =>
    (bind ?response
          (ask-question "What is the computer cooling: Normal(n), Good(g), Extra(e)"
                        n N g G e E ))
   (if (or (eq ?response N) (eq ?response n)) then
        (assert (cooling Normal))
    else
       (if (or (eq ?response G) (eq ?response g)) then
             (assert (cooling Good))
        else
            (assert (cooling Extra))))
    (assert (checked cooling)))


;==EXTRA

;===USB
(defrule check-for-usb "USB : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have an USB? (yes/no)") then
        (assert (extra usb)))
    (assert (checked usb)))
	
;===HDMI
(defrule check-for-hdmi "HDMI : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a HDMI? (yes/no)") then
        (assert (extra hdmi)))
    (assert (checked hdmi)))

;===VGA
(defrule check-for-vga "VGA : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a VGA? (yes/no)") then
        (assert (extra vga)))
    (assert (checked vga)))
	
;===dvdrom
(defrule check-for-dvdrom "DVD-ROM : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a CD/DVD-R/RW? (yes/no)") then
        (assert (extra DVD-CD-R-RW)))
    (assert (checked dvdrom)))	

;===dvdrom
(defrule check-for-dvdrom "SecondHDD : yes/no"
    =>
    (if (yes-or-no-p "Does the computer have a second HDD? (yes/no)") then
        (assert (extra extrahdd)))
    (assert (checked extrahdd)))	
	
;===COMPUTER TYPE EVALUATION RULES
(defrule all-checked "Assert that all specifications are checked"
	(checked comptype)
	(checked frequency)
	(checked proccount)
	(checked ram)
	(checked hddcapacity)
	(checked hddspeed)
	(checked videopower)
	(checked monitor)
	(checked cooling)
	(checked usb)
	(checked vga)
	(checked hdmi)
	(checked dvdrom)
	(checked extrahdd)
	=>
	(printout t "All checked" crlf)
	(assert (all-checked)))

;==evaluate for docs
(defrule evaluate-for-docs-and-browsing
	(all-checked)
	(or (comptype Laptop)
		(comptype Desktop))	
	(or (frequency Low)
		(frequency  Medium))	
	(or (proccount 0-1)
		(proccount 2-3))	
	(or (ram Low))	
	(not (hddcapacity Low))	
	(extra usb)
    =>
    (printout t "The computer is suitable for Docs and Internet" crlf)
    (assert (type DocsAndInternet)))
	
;==evaluate for movies
(defrule evaluate-for-movies
	(all-checked)
	(or (comptype Laptop)
		(comptype Desktop))	
	(or (proccount 0-1)
		(proccount 2-3))	
	(or (ram Low)
		(ram Medium)		)	
	(or (hddcapacity Low)
		(hddcapacity Medium))	
	(not (videopower Low))
	(monitor Large)
	(extra usb)
	(extra hdmi)
	(extra dvdrom)
    =>
    (printout t "The computer is suitable for Movies" crlf)
    (assert (type Movies)))	

;==evaluate for Business
(defrule evaluate-for-business
	(comptype Laptop)
	(or (frequency  Medium)
		(frequency  High))	
	(not (proccount 0-1))	
	(not (ram Low))	
	(not (hddspeed Slow))	
	(not (videopower Low))	
	(extra usb)
	(extra dvdrom)
    =>
    (printout t "The computer is suitable for Business" crlf)
    (assert (type Business)))	

;==evaluate Games
(defrule evaluate-for-gaming
	(or (comptype Laptop)
		(comptype Desktop))	
	(frequency  High)
	(proccount 4+)
	(ram High)
	(hddcapacity High)	
	(hddspeed Fast)
	(videopower High)
	(monitor Large)
	(cooling Extra)
	(extra usb)
	(extra vga)
	(extra hdmi)
	(extra dvdrom)
    =>
    (printout t "The computer is suitable for Gaming" crlf)
    (assert (type Gaming)))

;==evaluate Server
(defrule evaluate-for-server
	(or (comptype Rack)
		(comptype Desktop))	
	(frequency  High)
	(proccount 4+)
	(ram High)
	(hddcapacity High)	
	(hddspeed Fast)
	(cooling Extra)
	(extra usb)
	(extra extrahdd)
    =>
    (printout t "The computer is suitable Server" crlf)
    (assert (type Server)))
	


;==DETERMINE ADVANTAGE CHARACTERISTICS
(defrule frequency-advantage
    (type ?type)
    (frequency High)
    (test (eq ?type Movies))
    =>
    (assert (pros ?type frequency)))

(defrule hddcapacity-advantage
    (type ?type)
    (hddcapacity High)
    (test (eq ?type Business))
    =>
    (assert (pros ?type hddcapacity)))
	
(defrule hddspeed-advantage
    (type ?type)
    (hddspeed Fast)
    (test (eq ?type DocsAndInternet))
    =>
    (assert (pros ?type hddspeed)))
	
(defrule monitor-advantage
    (type ?type)
    (monitor Large)
    (or (test (eq ?type DocsAndInternet))
		(test (eq ?type Business))
	)
    =>
    (assert (pros ?type monitor)))	
	
(defrule cooling-advantage
    (type ?type)

    (or (and(test (eq ?type DocsAndInternet))
			(test (eq cooling Good)))
		(and(test (eq ?type Movies))
			(test (eq cooling Good)))
		(and(test (eq ?type Business))
			(test (eq cooling Good)))
	)
    =>
    (assert (pros ?type monitor)))	

(defrule extras-advantage
    (type ?type)
    (extra ?extra)
    (or (and (test (eq ?type DocsAndInternet))
             (test (neq ?extra usb)))     	 ;this is a requirement
        (and (test (eq ?type Movies))
             (test (neq ?extra hdmi))		 ;this is a reuirement
             (test (neq ?extra usb))		 ;this is a reuirement
             (test (neq ?extra dvdrom)))     ;this is a reuirement
        (and (test (eq ?type Business))
             (test (neq ?extra usb))		 ;this is a reuirement
             (test (neq ?extra dvdrom)))     ;this is a reuirement			 
        (and (test (eq ?type Server))
             (test (neq ?extra dvdrom))		 ;not necessary
             (test (neq ?extra extrahdd)))   ;this is a requirement
	)
    =>
    (assert (pros ?type ?extra)))

;==Type symmary ryls
(defrule no-type-summary "This is called last if no computer type was fount"
    (declare (salience -1))
    (all-checked)
    (not (type DocsAndInternet))
    (not (type Movies))
    (not (type Business))
    (not (type Gaming))
    (not (type Server))
    =>
    (printout t "The computer is no suitable for a particular type ot operations." crlf))



;=====pros summary
(defrule pros-summary
    (pros ?type ?pros)
    =>
    (printout t "As a " ?type " computer, its ")
    (printout t ?pros)
    (printout t " is considered a pros." crlf))


		