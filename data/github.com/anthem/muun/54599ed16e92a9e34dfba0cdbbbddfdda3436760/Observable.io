Observable := Object clone do (
	Observation := Object clone do (
		name ::= ""

		init := method(
			self observers := List clone
		)

		withName := method(name,
			return self clone setName(name)
		)

		prependObserver := method(blk,
			observers prepend(blk)
		)

		appendObserver := method(blk,
			observers append(blk)
		)

		removeObserver := method(blk,
			observers remove(blk)
		)

		clearObservers := method(
			self observers empty()
		)

		notifyObservers := method(
			self observers foreach(blk, blk performWithArgList("call", call message argsEvaluatedIn(call sender)))
		)
	)
		
	addObservation := method(
		for(argn, 0, call argCount - 1,
			name := call argAt(argn) asString
			self setSlot(name, Observation withName(name))
		)
	)
	
	observationNamed := method(name,
		return self getSlot(name)
	)
)
