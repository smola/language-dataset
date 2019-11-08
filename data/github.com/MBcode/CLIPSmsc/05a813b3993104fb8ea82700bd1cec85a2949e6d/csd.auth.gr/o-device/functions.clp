;;; ######################################################################################
;;; Custom function for printing messages in the output (screen). The message can be of type
;;; info, warn and error. An error message calls the (halt) function of CLIPS
;;; in order to stop the execution of the program. If no valid ?type has been specified, 
;;; then ?type = info
;;; ######################################################################################
(deffunction debug (?type $?message)
	(if (eq ?type error) then 
		(printout t "##ERROR##[O-DEVICE]:: ")
		(loop-for-count (?n 1 (length$ ?message)) do 
			(printout t (nth$ ?n $?message)))
		(printout t crlf)
		(halt)
		(return))			
	(if (and (neq ?type info)(neq ?type warn)) then 
		(bind ?type info))			
	(if (and ?*info* (eq ?type info))	then 
		(printout t INFO "[O-DEVICE]:: ")
		(loop-for-count (?n 1 (length$ ?message)) do 
			(printout t (nth$ ?n $?message)))
		(printout t crlf crlf)
	else		
		(if (and ?*warn* (eq ?type warn))	then 
			(printout t "WARN[O-DEVICE]:: ")
			(loop-for-count (?n 1 (length$ ?message))	do 
				(printout t (nth$ ?n $?message)))
			(printout t crlf crlf)))
)
	
;;; ######################################################################################
;;; Returns a random name
;;; ######################################################################################
(deffunction create-unique-class-name()
	(return (sym-cat ## (random) - (random)))
)

;;; ######################################################################################
;;; Returns a random object name
;;; ######################################################################################
(deffunction random-object-name()
	(return (sym-cat obj (random) - (random)))
)


;;; ######################################################################################
;;; Updates the current state of O-DEVICE
;;; ######################################################################################
(deffunction update()
	(bind ?fact (nth$ 1 (find-fact ((?f UPDATE))TRUE)))
	(modify ?fact (refresh TRUE))
)

;;; ######################################################################################
;;; Retracts a list of facts
;;; ######################################################################################
(deffunction retract$ ($?list)
	(progn$ (?n $?list)
		(retract ?n))
)

;;; ######################################################################################
;;; Adds the ?element to the $?list and returns the new list
;;; ######################################################################################
(deffunction add-element (?element $?list)
	(create$ $?list ?element)
)

;;; ######################################################################################
;;; Returns all the elements of an rdf list as a list. The rdf:first and rdf:rest triples
;;; are retract from the fact list
;;; ######################################################################################
(deffunction collect-list-elements(?l)
	(bind $?remove-fact-list (create$))
	(bind $?list_elements (create$))
	(bind ?fact_first (nth$ 1 
		(find-fact ((?f triple))
			(and 
				(eq ?f:subject ?l)
				(eq ?f:predicate ?*rdf:first*)))))
	(bind $?remove-fact-list (create$ $?remove-fact-list ?fact_first))
	(bind ?fact_rest (nth$ 1 
		(find-fact ((?f triple))
			(and 
				(eq ?f:subject ?l)
				(eq ?f:predicate ?*rdf:rest*)))))
	(bind $?remove-fact-list (create$ $?remove-fact-list ?fact_rest))
	(while (neq (fact-slot-value ?fact_rest object) ?*rdf:nil*)	do
		(bind $?list_elements (add-element (fact-slot-value ?fact_first object) $?list_elements))
		(bind ?fact_first (nth$ 1 
			(find-fact ((?f triple))
				(and 
					(eq ?f:subject (fact-slot-value ?fact_rest object))
					(eq ?f:predicate ?*rdf:first*)))))
		(bind $?remove-fact-list (create$ $?remove-fact-list ?fact_first))
		(bind ?fact_rest (nth$ 1 
			(find-fact ((?f triple))
				(and 
					(eq ?f:subject (fact-slot-value ?fact_rest object))
					(eq ?f:predicate ?*rdf:rest*)))))
		(bind $?remove-fact-list (create$ $?remove-fact-list ?fact_rest)))
	(bind $?list_elements (add-element (fact-slot-value ?fact_first object) $?list_elements))
	(retract$ $?remove-fact-list)
	(return $?list_elements)
)

;;; ######################################################################################
;;; Returns TRUE if ?p is a property 
;;; ######################################################################################
(deffunction property-fact-exists (?p)
	(any-factp ((?f PROPERTY))(eq ?f:name ?p))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is the name of a restriction class
;;; ######################################################################################
(deffunction is-restriction-class (?r)
	(any-factp ((?f RESTRICTION))(eq ?f:name ?r))
)

;;; ######################################################################################
;;; Removes all the duplicate values from the $?list and returns the new list
;;; ######################################################################################
(deffunction remove-duplicates$ ($?list)
	(bind $?result (create$))
	(bind ?end (length$ $?list))
	(progn$ (?n $?list) 
		(if (not (member$ ?n (subseq$ $?list (+ ?n-index 1) ?end))) then
	   	(bind $?result (create$ $?result ?n))))
	$?result
)

;;; ######################################################################################
;;; Checks whether the property ?p has been materialized as a slot in a class or not
;;; ######################################################################################
(deffunction is-property-materialized (?p)
	(bind $?classes (delete-member$ (class-subclasses USER inherit) INITIAL-OBJECT))
	(progn$ (?c $?classes)
		(if (slot-existp ?c ?p inherit) then 
			(return TRUE)))
	(return FALSE)
)

;;; ######################################################################################
;;; These two functions are used in order to retrieve the most specific classes of
;;; a list of classes ($?classes). The most specific classes are the classes of the list
;;; that are not superclasses of other classes of the list. These functions are used
;;; mainly during the dynamic generation of the classes of the oo model, in order to
;;; retrieve the direct superclasses of a particular class
;;; ######################################################################################
(deffunction superclass-of-one (?class $?sub-classes)
	(if (member$ ?class $?sub-classes) then
		(return TRUE)
	else
	  (progn$ (?n $?sub-classes) 
	  	(if (superclassp ?class ?n) then
 	   		(return TRUE)))
	  (return FALSE))
)
	
(deffunction most-specific-classes ($?classes)
	(bind $?result (create$))
	(while (> (length$ $?classes) 0) do
		(bind ?temp (nth$ 1 $?classes))
		(if (not (or 	(superclass-of-one ?temp (rest$ $?classes))
									(superclass-of-one ?temp $?result))) then
			(bind $?result (create$ $?result ?temp)))
		(bind $?classes (rest$ $?classes)))
	(return $?result)
)

;;; ######################################################################################
;;; Removes any restriction class from the list $?classes and returns the new list
;;; ######################################################################################
(deffunction remove-restriction-classes ($?classes)
	(bind $?result (create$))
	(progn$ (?c $?classes)
		(if (not (is-restriction-class ?c)) then
			(bind $?result (create$ $?result ?c))))
	(return $?result)
)

;;; ######################################################################################
;;; Returns a list with the restriction classes of the list $?classes
;;; ######################################################################################
(deffunction get-restriction-classes ($?classes)
	(bind $?result (create$))
	(progn$ (?c $?classes)
		(if (is-restriction-class ?c)	then
			(bind $?result (create$ $?result ?c))))
	(return $?result)
)

;;; ######################################################################################
;;; Returns TRUE if the property ?p is an object property (slot type)
;;; ######################################################################################
(deffunction is-object-property (?p)
	(any-factp ((?f PROPERTY))(and (eq ?f:name ?p) (or	(member$ object ?f:type)
																											(member$ transitive ?f:type)
																											(member$ symmetric ?f:type)
																											(and 	(member$ functional ?f:type)
							 																							(member$ object ?f:type))
																											(and	(member$ inversefunctional ?f:type)
							 																							(member$ object ?f:type)))))
)

;;; ######################################################################################
;;; Returns TRUE if ?p is a data range, i.e. there is a fact of the template DATARANGE
;;; with the same name ?p
;;; ######################################################################################
(deffunction is-datarange(?p)
	(any-factp ((?f DATARANGE))(eq ?f:name ?p))
)

;;; ######################################################################################
;;; Returns a list with the values of the data range ?p (owl:oneOf values)
;;; ######################################################################################
(deffunction get-dataranges (?p)
	(do-for-fact ((?f DATARANGE))(eq ?f:name ?p)
		(return ?f:oneOf))
)

;;; ######################################################################################
;;; Returns a list with the range classes of the property ?p. If there is no range, 
;;; then the owl:Thing is not returned, since the property might be a datatype one.
;;; ######################################################################################
(deffunction get-property-range (?p)
	(do-for-fact ((?f PROPERTY))(eq ?f:name ?p)
		(return ?f:range))
)

;;; ######################################################################################
;;; Returns a list with the domain classes of the property ?p. If the proeprty does not
;;; have a domain class, then the owl:Thing class is returned
;;; ######################################################################################
(deffunction get-property-domain (?p)
	(do-for-fact ((?f PROPERTY))(eq ?f:name ?p)
		(if (eq (length$ ?f:domain) 0) then 
			(return (create$ ?*owl:Thing*))
		else
			(return ?f:domain)))
)

;;; ######################################################################################
;;; Function for mapping some of the most common xsd datatypes on CLIPS types. 
;;; These mappings should be extended to other xsd types as well.
;;; ######################################################################################
(deffunction map-xsd-datatype (?type)
	(switch ?type
		(case ?*xsd:int* then (return INTEGER))
		(case ?*xsd:float* then (return FLOAT))
		(case ?*xsd:short* then (return "INTEGER) (range -32768 32767"))
		(case ?*xsd:byte* then (return "INTEGER) (range -128 127"))
		(case ?*xsd:boolean* then (return "SYMBOL) (allowed-values TRUE true True FALSE False false"))
		(case ?*xsd:string* then (return STRING))
		(case ?*xsd:nonNegativeInteger* then (return "INTEGER) (range 0 ?VARIABLE"))
		(case ?*xsd:Integer* then (return INTEGER))
		(case ?*xsd:integer* then (return INTEGER))
		(case ?*xsd:anyURI* then (return STRING))
		(case ?*rdf:XMLLiteral* then (return STRING))
		(case ?*xsd:positiveInteger* then (return "INTEGER) (range 1 ?VARIABLE"))
		(case ?*xsd:nonPositiveInteger* then (return "INTEGER) (range ?VARIABLE 0"))
		(case ?*xsd:dateTime* then (return STRING))	
		(default (debug warn "Unsupported datatype: " ?type) (return "LEXEME NUMBER")))
)

(deffunction get-clips-type (?type)
	(switch ?type
		(case ?*xsd:int* then (return INTEGER))
		(case ?*xsd:float* then (return FLOAT))
		(case ?*xsd:short* then (return INTEGER))
		(case ?*xsd:byte* then (return INTEGER))
		(case ?*xsd:boolean* then (return SYMBOL))
		(case ?*xsd:string* then (return STRING))
		(case ?*xsd:nonNegativeInteger* then (return INTEGER))
		(case ?*xsd:Integer* then (return INTEGER))
		(case ?*xsd:integer* then (return INTEGER))
		(case ?*xsd:anyURI* then (return STRING))
		(case ?*rdf:XMLLiteral* then (return STRING))
		(case ?*xsd:positiveInteger* then (return INTEGER))
		(case ?*xsd:nonPositiveInteger* then (return INTEGER))
		(case ?*xsd:dateTime* then (return STRING))	
		(default (debug warn "Unsupported datatype: " ?type) (return FALSE)))
)

;;; ######################################################################################
;;; Returns TRUE if all the classes of the list $?classes exist in the oo model 
;;; ######################################################################################
(deffunction classes-existp ($?classes)
	(progn$ (?n $?classes)
		(if (not (class-existp ?n)) then
			(return FALSE)))
	(return TRUE)
)

;;; ######################################################################################
;;; This function returns the order that the two classes ?c1 ?c2 should have in an 
;;; is-a defclass constraint. The ORDER template actually holds the valid order of 
;;; all the classes of the oo model. The class order is determined through a set of rules
;;; (see the main rule file of O-DEVICE)
;;; ######################################################################################
(deffunction determine-class-order (?c1 ?c2)
	(if (any-factp ((?f strong-order))(and (eq ?f:c1 ?c1)(eq ?f:c2 ?c2)))then 
		(return (create$ ?c1 ?c2))
	else (return (create$ ?c2 ?c1)))
)

;;; ######################################################################################
;;; Returns only the *direct* superclasses of the class ?c. This function operates over 
;;; the CLASS facts and not over the oo model.
;;; ######################################################################################
(deffunction get-direct-superclasses-fact(?c)
	(do-for-fact((?f CLASS))(eq ?f:name ?c)
		(return (remove-restriction-classes ?f:subclass)))
	(return (create$))
)

;;; ######################################################################################
;;; Returns *all* the superclasses of the class ?c. This function operates over 
;;; the CLASS facts and not over the oo model.
;;; ######################################################################################
(deffunction get-inherited-superclasses-fact(?c)
	(bind $?result (create$))
	(bind $?superclasses (get-direct-superclasses-fact ?c))
	(while (neq (length$ $?superclasses) 0) do
		(bind ?first (nth$ 1 $?superclasses))
		(bind $?superclasses (rest$ $?superclasses))
		(bind $?result (create$ $?result ?first))
		(bind $?superclasses (create$ $?superclasses (get-direct-superclasses-fact ?first))))
	(return $?result)
)

;;; ######################################################################################
;;; Returns TRUE if the class ?c1 is subclass of the class ?c2. This function operates over 
;;; the CLASS facts and not over the oo model. 
;;; ######################################################################################
(deffunction is-subclass-fact (?c1 ?c2)
	(member$ ?c2 (get-inherited-superclasses-fact ?c1))
)

;;; ######################################################################################
;;; 
;;; ######################################################################################
(deffunction check-string (?value)
	(if (eq (class ?value) STRING) then
		(return (str-cat "\"" ?value "\"")))
)


;;; ######################################################################################
;;; 
;;; ######################################################################################
(deffunction empty-slot (?object ?slot)
	(send ?object (sym-cat put- ?slot) (create$))
)


;;; ######################################################################################
;;; 
;;; ######################################################################################
;(deffunction get-cardinality-code (?slot ?domain)
;	(do-for-fact ((?c CLASS))(eq ?c:name ?domain)
;		(bind $?facts (find-all-facts ((?r RESTRICTION)) (and (eq ?r:restriction cardinality)
;																													(eq ?r:onProperty ?slot)
;																													(eq ?r:value 1)
;																													(is-subclass-fact ?c:name ?r:name))))
;		(return $?facts))
;)
		
;;; ######################################################################################
;;; This function creates a defclass definition and dynamically generates the class
;;; using the build function of CLIPS. 
;;; ?class is the name of the new class
;;; ?superclasses is a list with the class superclasses
;;; ?slots is a list with the slots that the class should have
;;; ######################################################################################
(deffunction define-class (?class ?superclasses ?slots)
	(bind $?superclasses (remove-restriction-classes $?superclasses))
	(bind $?superclasses (most-specific-classes $?superclasses))
	(bind $?slots ?slots)
	(bind ?command (str-cat "(defclass " ?class " (is-a "))
	(if (eq (length$ $?superclasses) 0)	then
		(bind ?command (str-cat ?command ?*owl:Thing* ")"))
	else
		(progn$ (?cl $?superclasses)
			(bind ?command (str-cat ?command ?cl " ")))
		(bind ?command (str-cat ?command ")"))) ;end of (is-a)	
	(progn$ (?slot $?slots)
		(if (is-object-property ?slot) then
			(bind ?command (str-cat ?command "(multislot " ?slot " (type INSTANCE)(allowed-classes "))
			(bind $?range (get-property-range ?slot))
			(if (eq (length$ $?range) 0) then 
				(bind $?range (create$ ?*owl:Thing*)))
			(bind ?command (str-cat ?command (nth$ 1 $?range) "))"))
		else ;datatype property
			(bind ?command (str-cat ?command "(multislot " ?slot ))
			(bind $?range (get-property-range ?slot))
			(if (or	(eq (length$ $?range) 0)
							(> (length$ $?range) 1)) then 
				(bind ?command (str-cat ?command ")"))
			else
				(if (is-datarange (nth$ 1 $?range)) then
					(bind $?drs (get-dataranges (nth$ 1 $?range)))
					(bind ?command (str-cat ?command " (allowed-values "))
					(progn$ (?val $?drs)
						(bind ?command (str-cat ?command " " (check-string ?val))))
					(bind ?command (str-cat ?command "))"))	
				else		
					(bind ?range (map-xsd-datatype (nth$ 1 $?range)))
					(bind ?command (str-cat ?command "(type " ?range "))"))))))
	(bind ?command (str-cat ?command ")"))
	(build ?command)
	(assert (DEFCLASS (code ?command)))	
	(update)	
	(return TRUE)
)
	
;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:hasValue restriction
;;; ######################################################################################
(deffunction is-hasValue-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction hasValue)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:allValuesFrom restriction
;;; ######################################################################################
(deffunction is-allValuesFrom-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction allValuesFrom)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:someValuesFrom restriction
;;; ######################################################################################
(deffunction is-someValuesFrom-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction someValuesFrom)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:cardinality restriction
;;; ######################################################################################
(deffunction is-cardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction cardinality)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:qualifiedCardinality restriction
;;; ######################################################################################
(deffunction is-qualifiedCardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction qualifiedCardinality)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:minCardinality restriction
;;; ######################################################################################
(deffunction is-minCardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction minCardinality)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:minQualifiedCardinality restriction
;;; ######################################################################################
(deffunction is-minQualifiedCardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction minQualifiedCardinality)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:maxCardinality restriction
;;; ######################################################################################
(deffunction is-maxCardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction maxCardinality)))
)

;;; ######################################################################################
;;; Returns TRUE if ?r is an owl:maxQualifiedCardinality restriction
;;; ######################################################################################
(deffunction is-maxQualifiedCardinality-restriction (?r)
	(any-factp ((?f RESTRICTION)) (and (eq ?f:name ?r)(eq ?f:restriction maxQualifiedCardinality)))
)

;;; ######################################################################################
;;; Returns the value of the restriction ?r
;;; ######################################################################################
(deffunction get-restriction-value (?r)
	(do-for-fact ((?f RESTRICTION)) (eq ?f:name ?r)
		(return ?f:value))
)

;;; ######################################################################################
;;; Returns the property that the restriction ?r refers to
;;; ######################################################################################
(deffunction get-restriction-property (?r)
	(do-for-fact ((?f RESTRICTION)) (eq ?f:name ?r)
		(return ?f:onProperty))
)

;;; ######################################################################################
;;; Returns the class that the restriction ?r refers to
;;; ######################################################################################
(deffunction get-restriction-onClass (?r)
	(do-for-fact ((?f RESTRICTION)) (eq ?f:name ?r)
		(return ?f:onClass))
)

;;; ######################################################################################
;;; Returns TRUE if ?c is an anonymous class. The j2cf module adds in front of each
;;; anonymous class (bnode) the string '##'
;;; ######################################################################################
(deffunction is-anonymous-class (?c)
	(any-factp ((?f CLASS))(and (eq ?f:name ?c) (eq (str-index ## ?c) 1)))
)

;;; ######################################################################################
;;; Returns TRUE if ?c is a named class, i.e. it is neither an anonumoys nor a restriction
;;; class
;;; ######################################################################################
(deffunction is-named-class (?c)
	(and	(not (is-restriction-class ?c))
				(not (is-anonymous-class ?c)))
)

;;; ######################################################################################
;;; Returns TRUE if ?c is a class
;;; ######################################################################################
(deffunction is-class (?c)
	(any-factp ((?f CLASS))(eq ?f:name ?c))
)

;;; ######################################################################################
;;; Returns TRUE if ?c is a delegator class (delegator slot)
;;; ######################################################################################
(deffunction is-delegator (?c)
	(if (is-restriction-class ?c) then
		(return TRUE))
	(any-factp ((?f CLASS))(and (eq ?f:name ?c)(eq ?f:delegator TRUE)))
)

;;; ######################################################################################
;;; Returns TRUE if the list $?classes contains at least one delegator class
;;; ######################################################################################
(deffunction delegator-exists$ ($?classes)
	(progn$ (?cl $?classes)
		(if (is-delegator ?cl) then 
			(return TRUE)))
	(return FALSE)
)

;;; ######################################################################################
;;; Returns the delegator class of the set of equivalent classes that the class ?c is
;;; member of. Each equivalent class set contains only a single delegator class.
;;; ######################################################################################
(deffunction get-delegator (?c)
	(if (is-delegator ?c) then 
		(return ?c)
	else
		(do-for-fact ((?f CLASS))(and (member$ ?c ?f:equivalent) (eq ?f:delegator TRUE))
			(return ?f:name)))
	(if (any-factp ((?f goal))TRUE) then
		(debug error "Cannot find the delegator of class " ?c))
)

;;; ######################################################################################
;;; Checks if all the classes of the list $?classes are delegators
;;; ######################################################################################
(deffunction all-delegators$ ($?classes)
	(progn$ (?n $?classes)
		(if (not (is-delegator ?n)) then 
			(return FALSE)))
	(return TRUE)
)

;;; ######################################################################################
;;; Checks if the property ?p exists. This function operates over the PROPERTY facts.
;;; ######################################################################################
(deffunction property-exists (?p)
	(any-factp ((?f PROPERTY)) (eq ?f:name ?p))
)
				
;;; ######################################################################################
;;; Returns TRUE if the class ?c has not any subclass in the list $?classes. 
;;; This function operates over the CLASS facts and not over the oo model. 
;;; ######################################################################################
(deffunction has-no-subclass (?c $?classes)
	(progn$ (?i $?classes)
		(if (is-subclass-fact ?i ?c) then 
			(return FALSE)))
	(return TRUE)
)

;;; ######################################################################################
;;; Returns the most specific classes of the classes in the set $?classes. 
;;; This function operates over the CLASS facts and not over the oo model.
;;; ######################################################################################
(deffunction most-specific-classes-fact ($?classes)
	(bind $?result (create$))
	(progn$ (?c $?classes)
		(if (has-no-subclass ?c (delete-member$ $?classes ?c)) then 
			(bind $?result (create$ $?result ?c))))
	(return $?result)
)

;;; ######################################################################################
;;; Returns a class that has as superclasses the classes of the list $?classes.
;;; This function operates over the CLASS facts and not over the oo model.
;;; ######################################################################################
(deffunction class-with-superclasses ($?classes)
	(bind ?len-classes (length$ $?classes))	
	(do-for-fact ((?f CLASS))	(and	(is-anonymous-class ?f:name)
																	(subsetp $?classes ?f:subclass)
																	(eq ?len-classes (length$ ?f:subclass)))
		(return (get-delegator ?f:name)))
	(return FALSE)
)

(deffunction is-materialized-class (?c)
	(any-factp((?f CLASS))(eq ?f:materialized TRUE))
)

(deffunction all-materialized ($?classes)
	(progn$ (?n $?classes)
		(if (not (is-materialized-class ?n)) then 
			(return FALSE)))
	(return TRUE)
)

;;; ######################################################################################
;;; Copies all the slot values of the ?old-instance to the ?new-instance
;;; ######################################################################################
(deffunction shallow-copy (?old-instance ?new-instance)
	(bind ?old-class (class ?old-instance))
	(bind $?slots (class-slots ?old-class inherit))
	(progn$ (?slot $?slots)
		(if (neq (nth$ 5 (slot-facets ?old-class ?slot)) SHR)	then
			(bind ?value (funcall send ?old-instance (sym-cat get- ?slot)))
			(funcall send ?new-instance (sym-cat put- ?slot) ?value)))
	(return TRUE)
)

;;; ######################################################################################
;;; Computes the difference between two lists
;;; ######################################################################################
(deffunction difference$ (?l1 ?l2)
	(bind $?result (create$))
	(progn$ (?n ?l1)
		(if (not (member$ ?n ?l2)) then
	  (bind $?result (create$ $?result ?n))))
	$?result
)

;;; ######################################################################################
;;; Checks if the two objects have the same values in their common slots
;;; ######################################################################################
(deffunction is-shallow-copied (?o1 ?o2)	
	(if (neq (class ?o1)(class ?o2)) then
		(return FALSE))
	(bind $?slots (class-slots (class ?o1) inherit))
	(progn$ (?slot $?slots)
	 	(if (property-exists ?slot) then
			(bind $?v1 (send ?o1 (sym-cat get- ?slot)))   	
			(bind $?v2 (send ?o2 (sym-cat get- ?slot)))
			(if (neq (length$ (difference$ $?v1 $?v2)) 0) then 
				(return FALSE))))
	(return TRUE)
)

;;; ######################################################################################
;;; Custom function for generating objects. This function is used by O-DEVICE
;;; in order to generate the objects based on the open-world semantics of OWL.
;;; This function should be always used, instead of the built-in (make-instance) function,
;;; in order to preserve the consistency of the oo model, in terms of the OWL semantics.
;;; ######################################################################################
(deffunction owl-make-instance (?object ?class)
	(if (not (instancep ?object)) then 
		(bind ?object (symbol-to-instance-name ?object)))
	(if (not (class-existp ?class))	then
		(debug error "Class " ?class " does not exist")
		(return))
	(bind ?class (get-delegator ?class))
	(if (not (instance-existp ?object))	then			
		(make-instance ?object of ?class)
	else
		(bind ?existing-class (class ?object))
		(if (or (eq ?existing-class ?class)
						(subclassp ?existing-class ?class)) then 
			(return TRUE)
		else
			(if (subclassp ?class ?existing-class) then
				(bind ?temp-instance (make-instance of ?class))
				(shallow-copy ?object ?temp-instance)
				(duplicate-instance ?temp-instance to (instance-name ?object))
				(send ?temp-instance delete)
			else
				(bind ?inter-class (class-with-superclasses (create$ ?class ?existing-class)))
				(if (neq ?inter-class FALSE) then
					(owl-make-instance ?object ?inter-class)
				else
					(bind ?new-class (create-unique-class-name))
					(define-class ?new-class (determine-class-order ?class ?existing-class) (create$))
					(assert (CLASS (name ?new-class) (materialized TRUE)(delegator TRUE)
						(subclass (create$ ?class ?existing-class))(intersection (create$ ))))
					(owl-make-instance ?object ?new-class) 									
					(assert (strong-order (c1 ?new-class)(c2 ?class)))
					(assert (strong-order (c1 ?new-class)(c2 ?existing-class)))))))
)

(deffunction owl-make-instance-run (?object ?class)
	(owl-make-instance ?object ?class)
	(run)
)

;;; ######################################################################################
;;; Returns all the OWL-specific classes of the oo model
;;; ######################################################################################
(deffunction get-owl-classes ()
	(create$ ?*owl:Thing* (class-subclasses ?*owl:Thing* inherit))
)

;;; ######################################################################################
;;; Returns TRUE if the object ?obj has the class type ?class
;;; ######################################################################################
(deffunction instance-has-class-type (?obj ?class)
	(if (not (instance-existp ?obj)) then
		(return FALSE))
	(any-instancep ((?o ?class))(eq ?o (instance-name ?obj)))
)

;;; ######################################################################################
;;; The following two functions are used in order to insert the ?value in the ?slot
;;; for the ?object. Similarly to the owl-make-instance function, this function should 
;;; be always used in order to preserve the consistency of the oo model in terms of the
;;; OWL semantics
;;; ######################################################################################
(deffunction owl-slot-insert (?object ?slot ?value)
	(if (is-object-property ?slot) then 
		(bind ?range (nth$ 1 (get-property-range ?slot)))
		(if (eq ?range nil) then 
			(bind ?range ?*owl:Thing*))
		(bind $?temp (funcall send ?object (sym-cat get- ?slot)))
		(if (not (member$ ?value $?temp))	then
			(owl-make-instance ?value ?range)
			(funcall send ?object (sym-cat put- ?slot) (create$ $?temp ?value)))
	else
		(bind $?temp (funcall send ?object (sym-cat get- ?slot)))
		(if (not (member$ ?value $?temp))	then
			(funcall send ?object (sym-cat put- ?slot) (create$ $?temp ?value))))
)

(deffunction owl-insert-value (?object ?slot ?value)
	(if (not (property-fact-exists ?slot)) then
		(debug error "Property " ?slot " does not exist")
		(return))
	(if (not (instancep ?object)) then 
		(bind ?object (symbol-to-instance-name ?object)))
	(if (not (instance-existp ?object)) then
		(debug error "Object " ?object " does not exist")
		(return)
		;(owl-make-instance ?object ?*owl:Thing*)
		;(owl-insert-value ?object ?slot ?value)
	else
		(if (not (slot-existp (class ?object) ?slot inherit))	then 
			(debug warn "The object " ?object " has no access to the slot " ?slot " (redefining it...)")
			;(return TRUE)
			(owl-make-instance ?object (nth$ 1 (get-property-domain ?slot)))
			(owl-insert-value ?object ?slot ?value)
		else
			(if (is-object-property ?slot) then 
				(if (not (instancep ?value)) then
					(bind ?value (symbol-to-instance-name ?value)))
				(bind ?range (nth$ 1 (get-property-range ?slot)))
				(if (eq ?range nil) then 
					(bind ?range ?*owl:Thing*))
				(if (not (instance-existp ?value)) then 
					(debug warn "The object " ?value " does not exist (redefining it ...)")
					;(return TRUE)
					(owl-make-instance ?value ?range)
					(owl-slot-insert ?object ?slot ?value)
				else 
					(if (not (instance-has-class-type ?value ?range))	then
						(owl-make-instance ?value ?range))
						(owl-slot-insert ?object ?slot ?value))
			else 
				(owl-slot-insert ?object ?slot ?value))))
	TRUE
)

(deffunction owl-insert-value-run (?object ?slot ?value)
	(owl-insert-value ?object ?slot ?value)
	(run)
)

(deffunction owl-insert-values$ (?object ?slot $?values)
	(progn$ (?value $?values)
		(owl-insert-value ?object ?slot ?value))
)

(deffunction owl-insert-values$-run (?object ?slot $?values)
	(owl-insert-values$ ?object ?slot $?values)
	(run)
)

;;; ######################################################################################
;;; Returns a string with the instance-based representation of the value ?x ([])
;;; ######################################################################################
(deffunction instance-string (?x)
	(if (instancep ?x) then
		(return (str-cat [ ?x ]))
	else
		(return ?x))
)	
	
;;; ######################################################################################
;;; Returns TRUE if all the values of the list $?values are of type ?type
;;; ######################################################################################
(deffunction all-values-are-of-type (?values ?type)
	(bind $?values ?values)
	(if (is-restriction-class ?type) then
		(debug error "O-DEVICE does not support restriction classes as all-values-from values.")
		(return))
	(if (class-existp ?type) then
		(progn$ (?obj $?values)
			(if (not (instance-has-class-type ?obj ?type)) then 
				(return FALSE)))
		(return TRUE)
	else 
		(bind ?xsd-type (get-clips-type ?type))
		(if (neq ?xsd-type FALSE) then 				
			(progn$ (?val $?values)
				(if (neq (class ?val) ?xsd-type) then 
					(return FALSE)))
			(return TRUE)
		else
			(return FALSE)))
)

;;; ######################################################################################
;;; Returns TRUE if there is at least one value of type ?type in the list $?values
;;; ######################################################################################
(deffunction some-values-are-of-type (?values ?type)
	(bind $?values ?values)
	(if (is-restriction-class ?type) then
		(debug error "O-DEVICE does not support restriction classes as some-values-from values.")
		(return))
	(if (class-existp ?type) then 
		(progn$ (?obj $?values)
			(if (instance-has-class-type ?obj ?type) then 
				(return TRUE)))
		(return FALSE)
	else 
		(bind ?xsd-type (get-clips-type ?type))
		(if (neq ?xsd-type FALSE) then 					
			(progn$ (?val $?values)
				(if (neq (class ?val) ?xsd-type) then 
					(return TRUE)))
			(return FALSE)
		else
			(return FALSE)))
)

;;; ######################################################################################
;;; Returns TRUE if the ?object satisfies the ?restriction
;;; ######################################################################################
(deffunction satisfy-restriction (?object ?restriction)
	(bind ?property (get-restriction-property ?restriction))
	(bind ?value (get-restriction-value ?restriction))
	(if (not (slot-existp (class ?object) ?property inherit)) then
		(return FALSE))
	(bind $?values (funcall send ?object (sym-cat get- ?property)))
	(if (is-hasValue-restriction ?restriction) then
		(if (is-object-property ?property) then
			(bind ?value (symbol-to-instance-name ?value))
			(bind $?sames (create$ ?value (send ?value (sym-cat get- ?*owl:sameAs*))))
			(progn$ (?t $?sames)
				(if (member$ ?t $?values) then
					(return TRUE)))
			(return FALSE)
		else 
			(return (member$ ?value $?values)))
	else
		(if (is-allValuesFrom-restriction ?restriction) then 
			(return (all-values-are-of-type $?values ?value))
		else (if (is-someValuesFrom-restriction ?restriction) then
			(return (some-values-are-of-type $?values ?value))
		else (if (is-cardinality-restriction ?restriction)	then
			(if (= (length$ $?values) ?value) then 
				(return TRUE)
			else 
				(return FALSE))
		else (if (is-minCardinality-restriction ?restriction) then
			(if (>= (length$ $?values) ?value) then 
				(return TRUE)
			else 
				(return FALSE))
		else (if (is-maxCardinality-restriction ?restriction) then
			(if (<= (length$ $?values) ?value) then 
				(return TRUE)
			else 
				(return FALSE))))))))
	(return FALSE)
)

;;; ######################################################################################
;;; Returns TRUE if all the $?slots exist in the ?class
;;; ######################################################################################
(deffunction more-qualified-values (?values ?onClass ?cardinality)
	(bind $?values ?values)
	(bind ?num 0)
	(progn$ (?obj $?values)
		(if (instance-has-class-type ?obj ?onClass) then
			(bind ?num (+ ?num 1))))
	(> ?num ?cardinality)
)


;;; ######################################################################################
;;; Returns TRUE if all the $?slots exist in the ?class
;;; ######################################################################################
(deffunction slots-existp (?class $?slots)
	(progn$ (?slot $?slots)
		(if (not (slot-existp ?class ?slot inherit)) then
			(return FALSE)))
	(return TRUE)
)

;;; ######################################################################################
;;; Exchanges the slot values between the object ?i1 and ?i2. This function is used
;;; for making identical objects to have the same values in their slots. Before copying
;;; the values, the function makes the instances to belong to the same class.
;;; ######################################################################################
(deffunction exchange-values (?i1 ?i2)
	(owl-make-instance ?i1 (class ?i2))
	(owl-make-instance ?i2 (class ?i1))
	(bind $?slotsA (class-slots (class ?i1) inherit))
	(progn$ (?slot $?slotsA)
		(if (neq (nth$ 5 (slot-facets (class ?i1) ?slot)) SHR) then
			(bind $?valuesA (funcall send ?i1 (sym-cat get- ?slot)))
			(progn$ (?value $?valuesA)
				(owl-insert-value ?i2 ?slot ?value))))
)

;;; ######################################################################################
;;; Generates the fact-based representation of the triples by calling the ARP Parser
;;; ######################################################################################
(deffunction create-facts (?uri)
	(bind ?command (str-cat "javaw -jar " ?*odevice-folder* "j2cf.jar"))
	(bind ?command (str-cat ?command " -onto " ?uri))	
	(bind ?command (str-cat ?command " -out " ?*triple-facts*))
	(bind ?command (str-cat ?command " -abbr " (lowcase ?*abbr*)))
	(bind ?command (str-cat ?command " -imports " (lowcase ?*imports*)))
	;(printout t ?command crlf)
	(system ?command)
)

;;; ######################################################################################
;;; Returns the number of the facts that have been generated for the ontology
;;; ######################################################################################
(deffunction triple-counter ()
	(open ?*triple-facts* in "r")
	(bind ?counter 0)
	(bind ?line (readline in))
	(while (neq ?line EOF) do
		(bind ?counter (+ ?counter 1))
		(bind ?line (readline in)))
	(close in)
	(return ?counter)
)

;;; ######################################################################################
;;; The main function for loading an ontology in O-DEVICE. 
;;; ?uri is the physical uri of the ontology or it can be a local file path
;;; (e.g. file:c:\\ontology.owl)
;;; ######################################################################################
(defmethod load-ontology (($?ontos STRING))
	(debug info "Loading ontology from " (implode$ $?ontos))
	(create-facts (implode$ $?ontos))
	(debug info "Number of facts created: " (triple-counter))
)

;;; ######################################################################################
;;; Saves the rules that have been generated by O-DEVICe in the file ?*rule-file*
;;; ######################################################################################
(deffunction save-rules*()
	(open ?*rule-file* out "w")
	(do-for-all-facts ((?r rule))TRUE
		(printout out ?r:code crlf)	
		(retract ?r))
	(close out)
)

;;; ######################################################################################
;;; Saves the facts in the file ?*fact-file*
;;; ######################################################################################
(deffunction save-facts* ()	
	(do-for-fact ((?f goal))TRUE
		(retract ?f))
	(save-facts ?*fact-file* visible)
)

;;; ######################################################################################
;;; Saves the instances in the file ?*object-file*
;;; ######################################################################################
(deffunction save-instances*()
	(save-instances ?*object-file* visible)
)

;;; ######################################################################################
;;; Saves the classes in the file ?*class-file*
;;; ######################################################################################
(deffunction save-classes* ()
	(open ?*class-file* out "w")
	(do-for-all-facts ((?f DEFCLASS))TRUE
		(printout out ?f:code crlf)
		(retract ?f))
	(close out)
)

;;; ######################################################################################
;;; Saves the current instance of O-DEVICE
;;; ######################################################################################
(deffunction save-o-device()
	(save-classes*)
	(save-instances*)
	(save-rules*)
	(save-facts*)
)

;;; ######################################################################################
;;; Generates the oo model and the set of inference rules that correspond to the
;;; loaded ontology. Normally, after the execution of this function, the current
;;; instance of CLIPS/O-DEVICE is of no importance and it should be deleted.
;;; ######################################################################################
(deffunction prepare()
	(run)
	(save-o-device)
)

;;; ######################################################################################
;;; Prints the prefix-to-namespace mappings
;;; ######################################################################################
(deffunction printPrefixNsMap()
	(do-for-all-facts ((?f PrefixNsMap))TRUE
		(printout t ?f:prefix " -> " ?f:namespace crlf))
)

;;; ######################################################################################
;;; Returns the symbol that derives after the substitution of the given prefix 
;;; with the corresponding namespace, 
;;; e.g. (ns owl:Class) -> owl:Class
;;; ######################################################################################
(deffunction ns (?name)
	(bind ?instance FALSE)
	(if (instancep ?name) then 
		(bind ?instance TRUE))
	(bind ?index (str-index ":" ?name))
	(if (not ?index) then 
		(debug error "Cannot determine the prefix of the given name: " ?name)
		(return FALSE))
	(bind ?prefix (string-to-field (sub-string 1 (- ?index 1) ?name)))
	(bind ?n (string-to-field (sub-string (+ ?index 1) (str-length ?name) ?name)))
	(bind $?fact (find-fact ((?f PrefixNsMap))(eq ?f:prefix ?prefix)))
	(if (eq (length$ $?fact) 0) then 
		(debug error "Cannot find the prefix " ?prefix)
		(return))
	(bind ?fact (nth$ 1 $?fact))
	(bind ?ns (fact-slot-value ?fact namespace))
	(if ?instance then 
		(return (symbol-to-instance-name (sym-cat ?ns ?n)))
	else
		(return (sym-cat ?ns ?n)))
)


;;; ######################################################################################
;;; Registers a new prefix-namespace mapping
;;; ######################################################################################
(deffunction mapPrefixNs (?prefix ?ns)
	(if (any-factp ((?f PrefixNsMap))(eq ?f:prefix ?prefix)) then 
		(debug error "The prefix " ?prefix " is already registered.")
		(return FALSE))
	(assert (PrefixNsMap (prefix ?prefix)(namespace ?ns)))
)



;NOTES
;- In order to delete the classes, all the constructs that refer to them
;  should be also deleted, such as the rules, deffunctions, etc... O-DEVICE's 
;  constructs are known ($rules.clp, functions, etc) and they can be re-loaded. 
;  But what about the ones of BES? An easy approach is to clear CLIPS, but is 
;  it any knowledge that should not be lost??

;- Even if some instances do not exist, some other instances may have them 
;  as slot values. The (clean-instances) function removes such instance values

;- This functions do not deal with the deletion of classes. They only
;  handle the backup and deletion of instances. 
;  ONLY THE INSTANCES OF THE CLASS HIERARCHY OF OWL:THING ARE BACKED-UP!

;;; ######################################################################################
;;; Builds the index file of the instances. The index is built only for the instances
;;; that are direct or indirect objects of the class owl:Thing 
;;; ######################################################################################
(deffunction build-instance-index ()
	(open ?*ins-idx* out "w")
	(bind ?counter 1)
	(do-for-all-instances ((?obj ?*owl:Thing*))TRUE
		(printout out (str-cat "0MBEGIN-ENTRY-" ?counter) crlf)
		(bind ?counter (+ ?counter 1))
		(printout out ?obj crlf)
		(printout out "END-ENTRY" crlf)
		(printout out "1MBEGIN-ENTRY-TYPE" crlf)
		(printout out (class ?obj) crlf)
		(printout out "END-ENTRY" crlf)
		(printout out "2MBEGIN-ENTRY-SLOTS" crlf)
		(bind $?slots (class-slots (class ?obj) inherit))
		(progn$ (?slot $?slots)
			(printout out ?slot "###"))
		(printout out crlf "END-ENTRY" crlf)
		(printout out "3IBEGIN-ENTRY-CODE" crlf)
		(printout out "(make-instance " ?obj " of " (class ?obj) crlf)
		(progn$ (?slot $?slots)
			(printout out "(" ?slot " " (implode$ (send ?obj (sym-cat get- ?slot))) " )" crlf))
		(printout out ")" crlf)
		(printout out "END-ENTRY" crlf))
	(close out)
)

;;; ######################################################################################
;;; Calls the build-instance-index function and deletes the instances
;;; ######################################################################################
(deffunction backup-instances()
	(toss ?*ins-idx*)
	(build-instance-index)
	(do-for-all-instances((?obj ?*owl:Thing*))TRUE
		(send ?obj delete))
)

;;; ######################################################################################
;;; Checks if there are objects that have non-existing instance values. This function
;;; checks all the instances of the KB of CLIPS and not only the instances of the
;;; class owl:Thing
;;; ######################################################################################
(deffunction clean-instances()
	(bind $?total (create$))
	(do-for-all-instances ((?obj USER))TRUE
		(bind $?slots (class-slots (class ?obj) inherit))
		(progn$ (?slot $?slots)
			(bind $?values (send ?obj (sym-cat get- ?slot)))
			(bind $?remove (create$))
			(progn$ (?v $?values)
				(if (and (instancep ?v)(not (instance-existp ?v))) then
					(bind $?remove (create$ $?remove ?v))))
			(if (> (length$ $?remove) 0) then
				(send ?obj (sym-cat put- ?slot) (difference$ $?values $?remove)))
			(bind $?total (create$ $?total $?remove))))  
	(remove-duplicates$ $?total)
)

;;; ######################################################################################
;;; Checks if all the slots of a backed-up instance are valid. If there is an 
;;; object that refers to a slot that does not exist, then the creation
;;; of this instance is skipped by the restore-inastance* function
;;; ######################################################################################
(deffunction valid-slots (?slots-string ?class)
	(bind $?slots (create$))
	(while TRUE do
		(bind ?index (str-index ### ?slots-string))
		(if (eq ?index FALSE) then
			(break))
		(bind $?slots (create$ $?slots (string-to-field (sub-string 1 (- ?index 1) ?slots-string))))
		(bind ?slots-string (sub-string (+ ?index 3) (str-length ?slots-string) ?slots-string)))
	(progn$ (?slot $?slots)
		(if (not (slot-existp ?class ?slot inherit)) then
			(return FALSE)))
	TRUE
)
	
;;; ######################################################################################
;;; Restores the instances that have been indexed previously. If the class type
;;; of an instance does not exist, or if there is at least one slot that is not accessible
;;; by the instance, then the instance is skipped. The function returns
;;; a list with the skipped instances. This list contains the indices of the instances
;;; and not the actual instance names, in order to be able to use the index file
;;; for retrieving relevant information. For example, the instance-name of 
;;; the instance with index ?i get be retrieved by calling the function
;;; (get-instance-name-from-external-file ?i). The indices are integer numbers > 0.
;;; ######################################################################################
(deffunction restore-instances*()
	(bind $?fail (create$))
	(object-pattern-match-delay	
		(fetch ?*ins-idx*)
		(bind ?counter 1)
		(while TRUE do
			(bind ?name (string-to-field (get-region ?*ins-idx* ?counter)))
			(bind ?type (string-to-field (get-region ?*ins-idx* type)))
			(bind ?slots (get-region ?*ins-idx* slots))
			(bind ?code (get-region ?*ins-idx* code))
			(if (eq ?type EOF) then
				(break))
			(if (and (class-existp ?type)(valid-slots ?slots ?type)) then
				(eval ?code)
			else
				(bind $?fail (create$ $?fail ?counter)))
			(bind ?counter (+ ?counter 1))
			(get-region ?*ins-idx* ^)
			(get-region ?*ins-idx* ^)
			(get-region ?*ins-idx* ^))
		(debug warn "" (length$ (clean-instances)) " instance values have been removed."))
	(debug warn "" (length$ $?fail) " instances have been skipped.")
	(return $?fail)
)

;;; ######################################################################################
;;; Returns the CLIPS code as a string for creating the instance with the index ?index.
;;; ######################################################################################
(deffunction get-instance-code-from-external-file (?index)
	(bind ?code (get-region ?*ins-idx* ?index type slots code))
	(get-region ?*ins-idx* ^)
	(get-region ?*ins-idx* ^)
	(get-region ?*ins-idx* ^)
	(return ?code)
)

;;; ######################################################################################
;;; Returns the CLIPS code as a string for creating the instance with the index ?index.
;;; ######################################################################################
(deffunction get-instance-slots-from-external-file (?index)
	(bind ?slots (get-region ?*ins-idx* ?index type slots))
	(get-region ?*ins-idx* ^)
	(get-region ?*ins-idx* ^)
	(get-region ?*ins-idx* ^)
	(return ?slots)
)

;;; ######################################################################################
;;; Returns the class type as symbol of the instance with index ?index
;;; ######################################################################################
(deffunction get-instance-type-from-external-file (?index)
	(bind ?type (get-region ?*ins-idx* ?index type))
	(get-region ?*ins-idx* ^)
	(get-region ?*ins-idx* ^)
	(return (string-to-field ?type))
)

;;; ######################################################################################
;;; Returns the instance name ([]) of the instance with index ?index
;;; ######################################################################################
(deffunction get-instance-name-from-external-file (?index)
	(bind ?name (get-region ?*ins-idx* ?index))
	(get-region ?*ins-idx* ^)
	(return (string-to-field ?name))
)

(defglobal ?*rule-files* = (create$))
;;; ######################################################################################
;;; Loads a rule file in O-DEVICE.
;;; *NOTE* The file should contain only rules. Any other construct can be load
;;; by calling directly the CLIPS native functions.
;;; ######################################################################################
(deffunction load-rules (?rule-file)
	(bind ?*rule-files* (create$ ?*rule-files* ?rule-file))
	(load* ?rule-file)
)
	
