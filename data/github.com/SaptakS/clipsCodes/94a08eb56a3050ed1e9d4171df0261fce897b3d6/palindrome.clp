; ----------------------------------------------------------
; Clips Program to detect if a string is Palindrome
; -----------------------------------------------------------
; -----------------------------------------------------------
; ------- Author: Saptak Sengupta ---------------------------
; -----------------------------------------------------------
; -----------------------------------------------------------




; -------------------------------------
; ------Various Objects Needed --------
; -------------------------------------

(deftemplate Original (slot original))
(deftemplate Reverse (slot rev))

;-------------------------------
;--------Function to Reverse----
;-------------------------------

 (deffunction reverse (?string)
       (bind ?rtn "")
       (loop-for-count (?i (length ?string))
          (bind ?rtn (str-cat (sub-string ?i ?i ?string) ?rtn)))
       ?rtn)

; ------------------------
; ------INPUT-------------
; ------------------------

(defrule GetOriginal
   (declare (salience 100))
   =>
   (printout t "Enter String: ")
   (bind ?response (read))
   (assert (Original (original ?response))))

;---------------------------
;--------RULES--------------
;---------------------------

(defrule ReverseStr
   (declare (salience -100))
   (Original (original ?s))
   =>
   (bind ?r (reverse ?s))
   (assert (Reverse (rev ?r)))
   (printout t "Reverse is: " ?r crlf))

(defrule Palindrome
   (declare (salience -200))
   (Original (original ?s))
   (Reverse (rev ?r))
   (test (= (str-compare ?r ?s) 0))
   =>
   (assert (pali yes))
   (printout t "Palindrome" crlf))

(defrule Notpali
   (declare (salience -200))
   (not (pali yes))
   =>
   (printout t "Not Palindrome" crlf))
