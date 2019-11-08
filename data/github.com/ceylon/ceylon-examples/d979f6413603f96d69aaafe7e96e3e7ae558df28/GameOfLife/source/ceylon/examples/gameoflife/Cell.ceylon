"Represents a Cell in a Matrix.
     It has a state and a nextState, both of which
     determine if the cell is dead, coming to life,
     dying, or dead."
by("Enrique Zamudio")
shared class Cell(index) {
    "The index of the Cell inside its Matrix."
	shared Integer index;
    "The current state of the Cell."
	shared variable Boolean state = false;
    "The next state of the Cell."
	variable Boolean ns = false;

	shared Boolean nextState { return ns; }
	assign nextState {
		ns = nextState;
	}

    "Evaluates the next state of the Cell, which
         depends on the states of its neighboring cells."
	shared void evaluateNextState([Cell+] neighbors) {
		value alives = neighbors.count((Cell c) => c.state);
		if (state) {
			ns = alives > 1 && alives < 4;
		} else {
			ns = alives == 3;
		}
	}

	"Copies the next state to the current state."
	shared void evolve() {
		state = ns;
	}

	shared actual String string =>
		"Cell: ``state then 1 else 0``";
}
