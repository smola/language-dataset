;####################################################################################
;### These functions are used to manipulate and work with dates in CLIPS.         ###
;### As at v. 6.30, CLIPS has no internal date functions and these must be        ###
;###    supplied by internal CLIPS functions or external functions from the       ###
;###    surrounding programming language or scripting language.                   ###
;###                                                                              ###
;### All these functions are internal CLIPS functions and do not depend on any    ###
;###    external language or operating system.                                    ###
;###                                                                              ###
;### NB Earlier versions of these calculations used the UNIX time, which is       ###
;###    based on the UNIX Epoch date. The UNIX Epoch or POSIX date is:            ###
;###    0000, 1 January 1970 UTC.                                                 ###
;###    Now the functions use the Chronological Julian Day Number (CJDN).         ###
;###    In ISO 8601 format, the UNIX Epoch date is: 1970-01-01T00:00:00Z.         ###
;###    Where dates are strings, the functions use and expect ISO 8601.           ###
;####################################################################################

(deffunction string-to-integer
    (?sNumber)


    ;We assume a string of integers only.
    ; As soon as we strike a character that is not an integer, bail and return nil.
    (bind ?iLen (str-length ?sNumber))
    (bind ?iNum 0)
    (bind ?iCount ?iLen)
    (while (> ?iCount 0)
		(switch (sub-string ?iCount ?iCount ?sNumber)
			(case "0" then (bind ?iDigit 0))
			(case "1" then (bind ?iDigit 1))
			(case "2" then (bind ?iDigit 2))
			(case "3" then (bind ?iDigit 3))
			(case "4" then (bind ?iDigit 4))
			(case "5" then (bind ?iDigit 5))
			(case "6" then (bind ?iDigit 6))
			(case "7" then (bind ?iDigit 7))
			(case "8" then (bind ?iDigit 8))
			(case "9" then (bind ?iDigit 9))
			(default (return nil))
		)
		;Get the sum of each newly found digit multiplied by the appropriate power of ten.
		(bind ?iNum (+ (integer (* ?iDigit (** 10 (- ?iLen ?iCount)))) ?iNum))
		(bind ?iCount (- ?iCount 1))
    )

    ?iNum
)
(deffunction daysAdd
    (?baseDate ?daysToAddToBase)

    ;As at April 2016, we change from calculating dates by seconds from the UNIX Epoch,
	;   and now calculate CJDN. Thus adding a day is simplified.

    ;Check that both arguments are numeric
    (if (not (integerp ?daysToAddToBase)) then
        (return 0)
    )
    (if (not (integerp ?baseDate)) then
        (return 0)
    )

	(return (+ ?baseDate ?daysToAddToBase))
)
(deffunction isThisAGLeapYear
    (?baseYear)

    ;We assume the following definition of a Leap Year (or intercalary or bissextile year),
    ;   as defined in the Gregorian Calendar:
    ;      1. February has 28 days each year, but 29 in a Leap Year.
    ;      2. All years, except century years, that are evenly divisible by 4 are Leap Years.
    ;      3. Only century years evenly divisible by 400 are Leap Years.
    ;We ignore the fact that the Gregorian calendar began in 1582 and in other years for
    ;   some countries, as our system date does not allow dates before 1970.

    ;Check that the argument is numeric
    (if (not (integerp ?baseYear)) then
        (return nil)
    )

    ;Check for leap centuries, then leap years that are not centuries.
    (if (= (mod ?baseYear 400) 0) then
        ;We have a Leap Year century
        (return TRUE)
    )
    (if (= (mod ?baseYear 100) 0) then
        ;We have a standard year century
        (return FALSE)
    )
    (if (= (mod ?baseYear 4) 0) then
        ;We have a leap year that is not a century.
        (return TRUE)
    )

    ;If not a leap year, we fall out here.
    (return FALSE)
)
(deffunction isThisAnOLeapYear
    (?baseYear)
    ;Orthodox Leap Year or Revised Julian Leap Year:
    ; if year divisible by 100, then if remainder 200 or 600, when divided by 900
    ; if year divisible by 4, but not 100.

     ;Check that the argument is numeric
    (if (not (integerp ?baseYear)) then
        (return nil)
    )

    ;Check for centuries
    (if (= (mod ?baseYear 100) 0) then
  		(if (or (= (mod ?baseYear 900) 200) (= (mod ?baseYear 900) 600)) then
  			(return TRUE)
  		else
  			(return FALSE)
  		)
    )

    (if (= (mod ?baseYear 4) 0) then
        ;We have a leap year that is not a century.
        (return TRUE)
    )

    ;If not a leap year, we fall out here.
    (return FALSE)
)
(deffunction isThisAJLeapYear
    (?baseYear)

    ;We assume the following definition of a Leap Year (or intercalary or bissextile year),
    ;   as defined in the Julian Calendar:
    ;      1. February has 28 days each year, but 29 in a Leap Year.
    ;      2. All years that are evenly divisible by 4 are Leap Years.

    ;Check that the argument is numeric
    (if (not (integerp ?baseYear)) then
        (return nil)
    )

    (if (= (mod ?baseYear 4) 0) then
        ;We have a leap year.
        (return TRUE)
    )

    ;If not a leap year, we fall out here.
    (return FALSE)

)
(deffunction isThisALeapYear
    (?baseYear $?extraArgs)

	 ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

    ;Basic check of the arguments
    (if (not (integerp ?baseYear)) then
        ;incorrect input
        (return nil)
    )

	;return whether the year of the given calendar is a leap year, according
	;   to those calendars' rules.
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (isThisAJLeapYear ?baseYear)))
        (case ?*iEDM_ORTHODOX* then (return (isThisAnOLeapYear ?baseYear)))
        (case ?*iEDM_WESTERN* then (return (isThisAGLeapYear ?baseYear)))
        (default (return (isThisAGLeapYear ?baseYear)))
    )
)
(deffunction floor
    (?iBaseNum)
    ;This function is not available in the standard mathematics library in CLIPS.
	;This function changed 2016-04-14 to return (integer ?iSomeNumber), rather than
	;   the result of integer division (div ?iSomeNumber 1), which appears to return a float
	;   with a mantissa of zero (e.g. 1.0) in some cases.

    ;Check that the argument is numeric
    (if (and (not (integerp ?iBaseNum)) (not (floatp ?iBaseNum))) then
        (return nil)
    )

    ;If zero, then return zero
    (if(= ?iBaseNum 0) then
	     (return (integer ?iBaseNum))
    )

    ;If the number is positive, just return the result of integer division (div ?iBaseNum 1).
    (if (> ?iBaseNum 0) then
	     (return (integer ?iBaseNum))
    )

    ;If the number is negative, if the modulus of the number by one is zero, return the number, else subtract 1.
    (if (< ?iBaseNum 0) then
    	(if (= (mod ?iBaseNum 1) 0) then
    	    (return (integer ?iBaseNum))
    	else
    	    (return (- (integer ?iBaseNum) 1))
    	)
    )
)

(deffunction pGregorianToCJDN
    (?baseYear ?baseMonth ?baseDay)
    ;The Chronological Julian Day Number is a whole number representing a day.
    ;  Its day begins at 00:00 Local Time.
    ;The zero point for a Julian Date (JD 0.0) corresponds to 12.00 UTC, 1 January -4712.
    ; The zero point for the CJDN is 1 January -4712 (the whole day in local time).
    ;From: http://aa.quae.nl/en/reken/juliaansedag.html .

    (bind ?iC0 (floor (/ (- ?baseMonth 3) 12)))
    (bind ?iX4 (+ ?baseYear ?iC0))
    (bind ?iX3 (floor (/ ?iX4 100)))
    (bind ?iX2 (mod ?iX4 100))
    (bind ?iX1 (- ?baseMonth (* 12 ?iC0) 3))
    (bind ?iJ (+ (floor (/ (* 146097 ?iX3) 4)) (floor (/ (* 36525 ?iX2) 100)) (floor (/ (+ (* 153 ?iX1) 2) 5)) ?baseDay 1721119))

    (return (integer ?iJ))
)

(deffunction pCJDNToGregorian
    (?iCJDN $?extraArgs)

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;Set defaults and assume if not accurately asking for "Y","m", or "d" as integer,
    ;  we return an ISO8601 string of the CJDN passed in.
    (bind ?bDO_YEAR FALSE)
    (bind ?bDO_MONTH FALSE)
    (bind ?bDO_DAY FALSE)
    (if (> (length ?extraArgs) 0) then
        (if (eq (nth$ 1 ?extraArgs) "Y") then
            (bind ?bDO_YEAR TRUE)
        else
            (if (eq (nth$ 1 ?extraArgs) "m") then
                (bind ?bDO_MONTH TRUE)
            else
                (if (eq (nth$ 1 ?extraArgs) "d") then
                    (bind ?bDO_DAY TRUE)
                )
            )
        )
    )

    ;Perform the calculations
    (bind ?iK3 (+ (* 4 (- ?iCJDN 1721120)) 3))
    (bind ?iX3 (floor (/ ?iK3 146097)))
    (bind ?iK2 (+ (* 100 (floor (/ (mod ?iK3 146097) 4))) 99))
    (bind ?iX2 (floor (/ ?iK2 36525)))
    (bind ?iK1 (+ (* (floor (/ (mod ?iK2 36525) 100)) 5) 2))
    (bind ?iX1 (floor (/ ?iK1 153)))
    (bind ?iC0 (floor (/ (+ ?iX1 2) 12)))
    (bind ?iYear (+ (* 100 ?iX3) ?iX2 ?iC0))
    (bind ?iMonth (+ (- ?iX1 (* 12 ?iC0)) 3))
    (bind ?iDay (integer (+ (floor (/ (mod ?iK1 153) 5)) 1)))

    ;(return (mkDate ?iYear ?iMonth ?iDay))
    ;If only a component of the date is required, return that as an integer.
    (if ?bDO_YEAR then
        (return ?iYear)
    )
    (if ?bDO_MONTH then
        (return ?iMonth)
    )
    (if ?bDO_DAY then
        (return ?iDay)
    )

    ;Otherwise return an ISO 8601 string of the date
    (bind ?sTemp (str-cat "0000" ?iYear))
    (bind ?sDate (sub-string (- (str-length ?sTemp) 3) (str-length ?sTemp) ?sTemp))
    (bind ?sTemp (str-cat "00" ?iMonth))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))
    (bind ?sTemp (str-cat "00" ?iDay))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))

    (return ?sDate)
)

(deffunction pMilankovicToCJDN
    (?baseYear ?baseMonth ?baseDay)
    ;The Chronological Julian Day Number is a whole number representing a day.
    ;  Its day begins at 00:00 Local Time.
    ;The zero point for a Julian Date (JD 0.0) corresponds to 12.00 UTC, 1 January -4712.
    ; The zero point for the CJDN is 1 January -4712 (the whole day in local time).
    ;From: http://aa.quae.nl/en/reken/juliaansedag.html .

    (bind ?iC0 (floor (/ (- ?baseMonth 3) 12)))
    (bind ?iX4 (+ ?baseYear ?iC0))
    (bind ?iX3 (floor (/ ?iX4 100)))
    (bind ?iX2 (mod ?iX4 100))
    (bind ?iX1 (- ?baseMonth (* 12 ?iC0) 3))
    (bind ?iJ (+ (floor (/ (+ (* 328718 ?iX3) 6) 9)) (floor (/ (* 36525 ?iX2) 100)) (floor (/ (+ (* 153 ?iX1) 2) 5)) ?baseDay 1721119))

    (return (integer ?iJ))
)

(deffunction pCJDNToMilankovic
    (?iCJDN $?extraArgs)

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;Set defaults and assume if not accurately asking for "Y","m", or "d" as integer,
    ;  we return an ISO8601 string of the CJDN passed in.
    (bind ?bDO_YEAR FALSE)
    (bind ?bDO_MONTH FALSE)
    (bind ?bDO_DAY FALSE)
    (if (> (length ?extraArgs) 0) then
        (if (eq (nth$ 1 ?extraArgs) "Y") then
            (bind ?bDO_YEAR TRUE)
        else
            (if (eq (nth$ 1 ?extraArgs) "m") then
                (bind ?bDO_MONTH TRUE)
            else
                (if (eq (nth$ 1 ?extraArgs) "d") then
                    (bind ?bDO_DAY TRUE)
                )
            )
        )
    )

    ;Perform the calculations
    (bind ?iK3 (+ (* 9 (- ?iCJDN 1721120)) 2))
    (bind ?iX3 (floor (/ ?iK3 328718)))
    (bind ?iK2 (+ (* 100 (floor (/ (mod ?iK3 328718) 9))) 99))
    (bind ?iX2 (floor (/ ?iK2 36525)))
    (bind ?iK1 (+ (* (floor (/ (mod ?iK2 36525) 100)) 5) 2))
    (bind ?iX1 (floor (/ (+ (* (floor (/ (mod ?iK2 36525) 100)) 5) 2) 153)))
    (bind ?iC0 (floor (/ (+ ?iX1 2) 12)))
    (bind ?iYear (+ (* 100 ?iX3) ?iX2 ?iC0))
    (bind ?iMonth (+ (- ?iX1 (* 12 ?iC0)) 3))
    (bind ?iDay (integer (+ (floor (/ (mod ?iK1 153) 5)) 1)))

    ;(return (mkDate ?iYear ?iMonth ?iDay))
    ;If only a component of the date is required, return that as an integer.
    (if ?bDO_YEAR then
        (return ?iYear)
    )
    (if ?bDO_MONTH then
        (return ?iMonth)
    )
    (if ?bDO_DAY then
        (return ?iDay)
    )

    ;Otherwise return an ISO 8601 string of the date
    (bind ?sTemp (str-cat "0000" ?iYear))
    (bind ?sDate (sub-string (- (str-length ?sTemp) 3) (str-length ?sTemp) ?sTemp))
    (bind ?sTemp (str-cat "00" ?iMonth))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))
    (bind ?sTemp (str-cat "00" ?iDay))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))

    (return ?sDate)
)

(deffunction pJulianToCJDN
    (?baseYear ?baseMonth ?baseDay)

    (bind ?iJ0 1721117)
    (bind ?iC0 (floor (/ (- ?baseMonth 3) 12)))
    (bind ?iJ1 (floor (/ (* 1461 (+ ?baseYear ?iC0)) 4)))
    (bind ?iJ2 (floor (/ (- (* 153 ?baseMonth) (* 1836 ?iC0) 457) 5)))
    (bind ?iJ (+ ?iJ1 ?iJ2 ?baseDay ?iJ0))
    (return (integer ?iJ))
)

(deffunction pCJDNToJulian
    (?iCJDN $?extraArgs)

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;Set defaults and assume if not accurately asking for "Y","m", or "d" as integer,
    ;  we return an ISO8601 string of the CJDN passed in.
    (bind ?bDO_YEAR FALSE)
    (bind ?bDO_MONTH FALSE)
    (bind ?bDO_DAY FALSE)
    (if (> (length ?extraArgs) 0) then
        (if (eq (nth$ 1 ?extraArgs) "Y") then
            (bind ?bDO_YEAR TRUE)
        else
            (if (eq (nth$ 1 ?extraArgs) "m") then
                (bind ?bDO_MONTH TRUE)
            else
                (if (eq (nth$ 1 ?extraArgs) "d") then
                    (bind ?bDO_DAY TRUE)
                )
            )
        )
    )

    ;Perform the calculations
    (bind ?iY2 (- ?iCJDN 1721118))
    (bind ?iK2 (+ (* 4 ?iY2) 3))
    (bind ?iK1 (+ (* 5 (floor (/ (mod ?iK2 1461) 4))) 2))
    (bind ?iX1 (floor (/ ?iK1 153)))
    (bind ?iC0 (floor (/ (+ ?iX1 2) 12)))
    (bind ?iYear (+ (floor (/ ?iK2 1461)) ?iC0))
    (bind ?iMonth (+ (- ?iX1 (* 12 ?iC0)) 3))
    (bind ?iDay (integer (+ (floor (/ (mod ?iK1 153) 5)) 1)))

    ;(return (mkDate ?iYear ?iMonth ?iDay))
    ;If only a component of the date is required, return that as an integer.
    (if ?bDO_YEAR then
        (return ?iYear)
    )
    (if ?bDO_MONTH then
        (return ?iMonth)
    )
    (if ?bDO_DAY then
        (return ?iDay)
    )

    ;Otherwise return an ISO 8601 string of the date
    (bind ?sTemp (str-cat "0000" ?iYear))
    (bind ?sDate (sub-string (- (str-length ?sTemp) 3) (str-length ?sTemp) ?sTemp))
	  (bind ?sTemp (str-cat "00" ?iMonth))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))
    (bind ?sTemp (str-cat "00" ?iDay))
    (bind ?sDate (str-cat ?sDate "-" (sub-string (- (str-length ?sTemp) 1) (str-length ?sTemp) ?sTemp)))

    (return ?sDate)
)

(deffunction mkDate
    (?baseYear ?baseMonth ?baseDay $?extraArgs)

    ;Converts integers YYYY, MM, and DD into a CJDN (Chronological Julian Day Number).

    ;Basic check of the arguments
    (if (not (and (integerp ?baseYear) (integerp ?baseMonth) (integerp ?baseDay))) then
        ;incorrect input
        (return nil)
    )
    (if (or (< ?baseMonth 1) (> ?baseMonth 12)) then
        (return nil)
    )
    (if (or (< ?baseDay 1) (> ?baseDay 31)) then
        (return nil)
    )

    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )

    ;Assume we got valid input for the EDM. The default will handle the case, where this is not true,
    ;   and take a reasonable default, i.e. the Gregorian calendar.
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (pJulianToCJDN ?baseYear ?baseMonth ?baseDay)))
        (case ?*iEDM_ORTHODOX* then (return (pMilankovicToCJDN ?baseYear ?baseMonth ?baseDay)))
        (case ?*iEDM_WESTERN* then (return (pGregorianToCJDN ?baseYear ?baseMonth ?baseDay)))
        (default (return (pGregorianToCJDN ?baseYear ?baseMonth ?baseDay)))
    )
)
(deffunction unmakeDate
    (?iCJDN $?extraArgs)

    ;Converts integers YYYY, MM, and DD into a CJDN (Chronological Julian Day Number).

    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;return the date for the CJDN of the given calendar
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (pCJDNToJulian ?iCJDN)))
        (case ?*iEDM_ORTHODOX* then (return (pCJDNToMilankovic ?iCJDN)))
        (case ?*iEDM_WESTERN* then (return (pCJDNToGregorian ?iCJDN)))
        (default (return (pCJDNToGregorian ?iCJDN)))
    )
)
(deffunction dayFromDateINT
    (?iCJDN $?extraArgs)

    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;return the date for the CJDN of the given calendar
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (pCJDNToJulian ?iCJDN "d")))
        (case ?*iEDM_ORTHODOX* then (return (pCJDNToMilankovic ?iCJDN "d")))
        (case ?*iEDM_WESTERN* then (return (pCJDNToGregorian ?iCJDN "d")))
        (default (return (pCJDNToGregorian ?iCJDN "d")))
    )
)
(deffunction monthFromDateINT
    (?iCJDN $?extraArgs)

    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;return the date for the CJDN of the given calendar
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (pCJDNToJulian ?iCJDN "m")))
        (case ?*iEDM_ORTHODOX* then (return (pCJDNToMilankovic ?iCJDN "m")))
        (case ?*iEDM_WESTERN* then (return (pCJDNToGregorian ?iCJDN "m")))
        (default (return (pCJDNToGregorian ?iCJDN "m")))
    )
)
(deffunction yearFromDateINT
    (?iCJDN $?extraArgs)

    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

    ;Basic check of the arguments
    (if (not (integerp ?iCJDN)) then
        ;incorrect input
        (return nil)
    )

    ;return the date for the CJDN of the given calendar
    (switch ?iTempEDM
        (case ?*iEDM_JULIAN* then (return (pCJDNToJulian ?iCJDN "Y")))
        (case ?*iEDM_ORTHODOX* then (return (pCJDNToMilankovic ?iCJDN "Y")))
        (case ?*iEDM_WESTERN* then (return (pCJDNToGregorian ?iCJDN "Y")))
        (default (return (pCJDNToGregorian ?iCJDN "Y")))
    )
)
(deffunction DoW
    (?dDate $?extraArgs)

    (if (not (integerp ?dDate)) then
        (return nil)
    )
    ;Find the calendar used or Easter Dating Method requested, or just use the value of the
    ;   system global ?*EDM*.
    (if (= (length ?extraArgs) 0) then
        (bind ?iTempEDM ?*EDM*)
    else
        (bind ?iTempEDM (nth$ 1 ?extraArgs))
    )
    (if (and (!= ?iTempEDM ?*iEDM_JULIAN*) (!= ?iTempEDM ?*iEDM_ORTHODOX*) (!= ?iTempEDM ?*iEDM_WESTERN*)) then
        (bind ?iTempEDM ?*iEDM_WESTERN*)
    )

	;Calculates the day of the week for a given date (passed in as a CJDN.
	(bind ?iDay (dayFromDateINT ?dDate ?iTempEDM))
	(bind ?iMonth (monthFromDateINT ?dDate ?iTempEDM))
	(bind ?iYear (yearFromDateINT ?dDate ?iTempEDM))

	(bind ?iA (floor (/ (- 14 ?iMonth) 12)))
	(bind ?iY (- ?iYear ?iA))
	(bind ?iM (- (+ ?iMonth (* 12 ?iA)) 2))

	(if (= ?iTempEDM ?*iEDM_JULIAN*) then
		;for the Julian calendar
		(bind ?iD (mod (+ 5 ?iDay ?iY (floor (/ ?iY 4)) (floor (/ (* 31 ?iM) 12))) 7))
	else
		(if (= ?iTempEDM ?*iEDM_ORTHODOX*) then
			(if (>= ?iYear 2800) then
				;Check whether date is later than AD 2800. If not, just use the calculation for the Gregorian calendar.
				;;Now convert Revised Julian date to a Gregorian date.
				;(bind ?dTemp (pCJDNToGregorian (pMilankovicToCJDN ?iYear ?iMonth ?iDay)))
				;(bind ?iDay (dayFromDateINT ?dTemp))
				;(bind ?iMonth (monthFromDateINT ?dTemp))
				;(bind ?iYear (yearFromDateINT ?dTemp))
				;(bind ?iA (floor (/ (- 14 ?iMonth) 12)))
				;(bind ?iY (- ?iYear ?iA))
				;(bind ?iM (- (+ ?iMonth (* 12 ?iA)) 2))
				;;for the Gregorian calendar and the Revised Julian until AD 2800.
				(bind ?iD (mod (+ (- (+ ?iDay ?iY (floor (/ ?iY 4))) (floor (/ ?iY 100))) (floor (/ (+ ?iY 300) 900)) (floor (/ (+ ?iY 700) 900)) (floor (/ (* 31 ?iM) 12))) 7))
			else
				;for the Gregorian calendar and the Revised Julian until AD 2800.
				(bind ?iD (mod (+ (- (+ ?iDay ?iY (floor (/ ?iY 4))) (floor (/ ?iY 100))) (floor (/ ?iY 400)) (floor (/ (* 31 ?iM) 12))) 7))
			)
		else
			;for the Gregorian calendar and the Revised Julian until AD 2800.
			(bind ?iD (mod (+ (- (+ ?iDay ?iY (floor (/ ?iY 4))) (floor (/ ?iY 100))) (floor (/ ?iY 400)) (floor (/ (* 31 ?iM) 12))) 7))
		)
	)

	;Convert day to the sequence used in these calculations.
	(if (= ?iD 0) then
		(bind ?iD 7)
	)

	(return ?iD)

)

(deffunction clFindSun
    (?dStartDate ?dEndDate)

    (if (or (not (integerp ?dStartDate)) (not (integerp ?dEndDate))) then
        (return nil)
    )
    (if (> ?dStartDate ?dEndDate) then
        (return nil)
    )

    ;Find the day of the week for the start day.
    (bind ?TempDoW (DoW ?dStartDate))
    (if (= ?TempDoW 7) then
        (return ?dStartDate)
    )

    ;Find difference and calculate day difference.
    (bind ?dTempDate (+ (- 7 ?TempDoW) ?dStartDate))

    ;Final check that we are still within the range.
    (if (<= ?dTempDate ?dEndDate) then
        (return ?dTempDate)
    else
        (return nil)
    )

    ?dTempDate
)

(deffunction clFindSat
    (?dStartDate ?dEndDate)

    (if (or (not (integerp ?dStartDate)) (not (integerp ?dEndDate))) then
        (return nil)
    )
    (if (> ?dStartDate ?dEndDate) then
        (return nil)
    )

    ;Find the day of the week for the start day.
    (bind ?TempDoW (DoW ?dStartDate))
    (if (= ?TempDoW 6) then
        (return ?dStartDate)
    )
    ;Check if it is a Sunday, and then all the other days of the week.
    (if (= ?TempDoW 7) then
        (bind ?dTempDate (+ 7 ?dStartDate))
    else
        ;Find difference and calculate day difference.
        (bind ?dTempDate (+ (- 6 ?TempDoW) ?dStartDate))
    )

    ;Final check that we are still within the range.
    (if (<= ?dTempDate ?dEndDate) then
        (return ?dTempDate)
    else
        (return nil)
    )

    ?dTempDate
)

(deffunction CalcDayDiffJulianCal
    (?dDate)

    ;Find the days difference between the passed-in Gregorian date and the Julian date
    ;  Find the hundreds part of the year
    (bind ?sYearHundreds (sub-string 1 2 (unmakeDate ?dDate)))
    ;We need 1.25 to round to 1, and -1.25 to round to -2 -- i.e. floor or integer division function.
    (bind ?iYearHundreds (string-to-integer ?sYearHundreds))
    (bind ?iFlooredValue (floor (/ ?iYearHundreds 4)))
    (if (< ?iYearHundreds 0) then
    	;Create a real floored value, not just the result of integer division
    	(bind ?iFlooredValue (- ?iFlooredValue 1))
    )
    (bind ?iDaysDiff (- ?iYearHundreds ?iFlooredValue 2))
    (return ?iDaysDiff)
)
(deffunction mkODate
    (?baseYear ?baseMonth ?baseDay)

    ;First makes the date according to the Gregorian calendar,
    ;   then converts it to an Orthodox or Revised Julian date.
    ;   The Revised Julian calendar was implemented 13 October 1923 (Gregorian).
    ;  cf. http://articles.adsabs.harvard.edu/full/seri/AN.../0220/0000203.000.html .

    ;To save time, only perform the computation, after the first occurrence of a date,
    ; which differs from the Gregorian Calendar. In the Gregorian Calendar, 2800 is a leap year;
    ; in the Revised Julian or Milankovic Calendar it is not.
    (if (>= ?baseYear 2800) then
    	(bind ?iTempCJDN (pGregorianToCJDN ?baseYear ?baseMonth ?baseDay))
    	(return (pCJDNToMilankovic ?iTempCJDN))
    else
      (return (mkDate ?baseYear ?baseMonth ?baseDay))
    )
)

(deffunction mkJDate
    (?baseYear ?baseMonth ?baseDay)

   ;First makes the date according to the Gregorian calendar, then converts it to a
   ;   date on the Julian calendar.
    (bind ?iTempCJDN (pGregorianToCJDN ?baseYear ?baseMonth ?baseDay))
    (return (pCJDNToJulian ?iTempCJDN))
)

(deffunction mkDateForCurrentCal
    (?baseYear ?baseMonth ?baseDay)
    ;Converts the parameters to a date in the chosen EDM calendar
    (if (= ?*EDM* ?*iEDM_WESTERN*) then
        (return (mkDate ?baseYear ?baseMonth ?baseDay))
    else
        (if (= ?*EDM* ?*iEDM_ORTHODOX*) then
            (return (mkODate ?baseYear ?baseMonth ?baseDay))
        else
            (if (= ?*EDM* ?*iEDM_JULIAN*) then
                (return (mkJDate ?baseYear ?baseMonth ?baseDay))
            )
        )
    )
)
