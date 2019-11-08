
class Board {
    new make(Int rows, Int columns) {
        this.rows = rows;
        this.columns = columns;
        this.cells.fill(0, rows * columns);
    }

    Void rotate(Int n) {
        rotates = (rotates + n) % 4;
    }

    Int get(Int i, Int j) {
        t := derotateCoordinates(i, j);
        return cells[t[0] * columns + t[1]];
    }

    Void set(Int i, Int j, Int value) {
        t := derotateCoordinates(i, j);
        cells[t[0] * columns + t[1]] = value;
    }

    Int getRows() {
        return rotates % 2 == 0 ? rows : columns;
    }

    Int getColumns() {
        return rotates % 2 == 0 ? columns : rows;
    }

    Int[][] toList() {
        result := Int[][,];

        for (i := 0; i < getRows(); ++i) {
            row := Int[,];

            for (j := 0; j < getColumns(); ++j) {
                row.push(get(i, j));
            }

            result.push(row);
        }

        return result;
    }

    private Int[] derotateCoordinates(Int i, Int j) {
        if (rotates != 0) {
            rotateCoordinates :=
                rotates > 0
                ? Board#rotateCoordinatesLeft.func
                : Board#rotateCoordinatesRight.func;
            t := [i, j, getRows, getColumns];

            for (k := 0; k < rotates.abs; ++k) {
                t = rotateCoordinates(this, t);
            }

            return [t[0], t[1]];
        }

        return [i, j];
    }

    private Int[] rotateCoordinatesRight(Int[] t) {
        i := t[0];
        j := t[1];
        r := t[2];
        c := t[3];
        return [j, r - i - 1, c, r];
    }

    private Int[] rotateCoordinatesLeft(Int[] t) {
        i := t[0];
        j := t[1];
        r := t[2];
        c := t[3];
        return [c - j - 1, i, c, r];
    }


    private Int rows;
    private Int columns;
    private Int[] cells := Int[,];
    private Int rotates := 0;
}
