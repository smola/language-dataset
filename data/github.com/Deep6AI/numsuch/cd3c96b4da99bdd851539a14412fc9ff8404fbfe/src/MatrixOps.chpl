use LinearAlgebra,
    LinearAlgebra.Sparse,
    Time,
    NumSuch,
    Random;

config param batchsize = 10000;

/*
 A matrix of data with named rows and columns.
 */
class NamedMatrix {
  var D: domain(2),
      SD = CSRDomain(D),
      X: [SD] real,  // the actual data
      rows: BiMap = new BiMap(),
      cols: BiMap = new BiMap();

   proc init() {
     this.complete(); // NAMEDMATRIX DOESNT EXPLICITLY INHERIT ANYTHING SO IS THIS NECESSARY??
   }

   proc init(X) {
     this.D = {X.domain.dim(1), X.domain.dim(2)};
  //   this.SD = X.domain;
     this.complete();
     this.loadX(X);
   }

   proc init(X, names: [] string) {
     this.init(X);
     try! this.setRowNames(names);
     try! this.setColNames(names);
   }

   proc init(X, rownames: [] string, colnames: [] string) {
     this.init(X);
     try! this.setRowNames(rownames);
     try! this.setColNames(colnames);
   }

   proc init(rownames: [] string, colnames: [] string) {
     this.D = {1..rownames.size, 1..colnames.size};
     this.complete();
     try! this.setRowNames(rownames);
     try! this.setColNames(colnames);
   }

   proc init(N: NamedMatrix) {
     this.init(N.X);
     this.rows = N.rows;
     this.cols = N.cols;
   }

   proc init(X, rows: BiMap, cols: BiMap) {
     this.init(X);
     this.rows = rows;
     this.cols = cols;
   }
}


/*
Returns the number of rows in the matrix frame
 */
proc NamedMatrix.nrows() {
  return X.domain.dim(1).size;
}

/*
Returns the number of columns in the matrix frame
 */
proc NamedMatrix.ncols() {
  return X.domain.dim(2).size;
}

/*
 Finds the max along the row indicated by row number
 */

proc NamedMatrix.rowMax(i: int) {
  return aMax(this.X, 1)[i];
}
proc NamedMatrix.rowMax(s: string) {
  return this.rowMax(this.rows.get(s));
}


proc NamedMatrix.rowMax() {
  return aMax(this.X, 1);
}

proc NamedMatrix.colMax(i: int) {
  return aMax(this.X, 2)[i];
}
proc NamedMatrix.colMax(s: string) {
  return this.colMax(this.cols.get(s));
}


proc NamedMatrix.colMax() {
  return aMax(this.X, 2);
}

proc NamedMatrix.rowArgMax(i: int) {
  return argMax(this.X, 1)[i];
}
proc NamedMatrix.rowArgMax(s: string) {
  return this.rowArgMax(this.rows.get(s));
}

proc NamedMatrix.rowArgMax() {
  return argMax(this.X, 1);
}

proc NamedMatrix.colArgMax(i: int) {
  return argMax(this.X, 2)[i];
}
proc NamedMatrix.colArgMax(s: string) {
  return this.colArgMax(this.cols.get(s));
}

proc NamedMatrix.colArgMax() {
  return argMax(this.X, 2);
}


proc NamedMatrix.rowMin(i: int) {
  return aMin(this.X, 1)[i];
}
proc NamedMatrix.rowMin(s: string) {
  return this.rowMin(this.rows.get(s));
}

proc NamedMatrix.rowMin() {
  return aMin(this.X, 1);
}

proc NamedMatrix.colMin(i: int) {
  return aMin(this.X, 2)[i];
}
proc NamedMatrix.colMin(s: string) {
  return this.colMin(this.cols.get(s));
}


proc NamedMatrix.colMin() {
  return aMin(this.X, 2);
}

proc NamedMatrix.rowArgMin(i:int) {
  return argMin(this.X, 1)[i];
}
proc NamedMatrix.rowArgMin(s: string) {
  return this.rowArgMin(this.rows.get(s));
}


proc NamedMatrix.rowArgMin() {
  return argMin(this.X, 1);
}

proc NamedMatrix.colArgMin(i) {
  return argMin(this.X, 2)[i];
}
proc NamedMatrix.colArgMin(s: string) {
  return this.colArgMin(this.cols.get(s));
}


proc NamedMatrix.colArgMin() {
  return argMin(this.X, 2);
}

/* We don't have a good matPlus for Q + E yet, so doing this element
  by element. kinda sucks.
 */
proc NamedMatrix.matPlus(Y: NamedMatrix) {
  forall ij in Y.SD {
    this.X[ij] += Y.X[ij];
  }
}

/*
 Multiplies the underlying matrix in place.
 */
proc NamedMatrix.eTimes(a: real) {
  this.X = a * this.X;
}

/*
 Prints the matrix with row / column headers, not recommended for large matrices.
 Also width of entries should not exceed about 8 places.
 */
proc NamedMatrix.pprint() {
  write("     ");
  for c in this.cols.sorted() {
    write("       %5s  |".format(c[1]));
  }
  write("\n");
  for r in this.rows.sorted() {
    write(" %3s ".format(r[1]));
    for c in this.cols.sorted() {
      write(" %13.3dr ".format(this.get(r[2], c[2])));
    }
    write("\n");
  }

}


//proc NamedMatrix.argRowMax()


/*
Loads the data from X into the internal array, also called X.  We call them all X to keep it clear.

:arg real[]: Array representing the matrix
 */
proc NamedMatrix.loadX(X:[], shape: 2*int =(-1,-1)) {
  if shape(1) > 0 && shape(2) > 0 {
    this.D = {1..shape(1), 1..shape(2)};
  }
  for (i,j) in X.domain {
    this.SD += (i,j);
    this.X(i,j) = X(i,j);
  }
}


/*
Sets the row names for the matrix X
 */
proc NamedMatrix.setRowNames(rn:[]) throws {
  //if (X.domain.dim(1).size > 0) && (rn.size != X.domain.dim(1).size) {
  //if (1 > 0) && (rn.size != X.domain.dim(1).size) {
  if rn.size != X.domain.dim(1).size {
    const err = new DimensionMatchError(expected = X.domain.dim(1).size, actual=rn.size);
    throw err;
  } else {
    for i in 1..rn.size {
      this.rows.add(rn[i]);
    }
  }
  return this.rows;
}

/*
Sets the column names for the matrix X
*/
proc NamedMatrix.setColNames(cn:[]) throws {
  if cn.size != X.domain.dim(2).size {
    const err = new DimensionMatchError(expected = X.domain.dim(2).size, actual=cn.size);
    throw err;
  } else {
    for i in 1..cn.size {
      this.cols.add(cn[i]);
    }
  }
  return this.cols;
}

/*
 Gets the value of the (i,j) entry of the matrix X in the NamedMatrix
 */
proc NamedMatrix.get(i: int, j: int) {
   return this.X(i,j);
 }

 /*
 Get the values using the named rows and columns
  */
proc NamedMatrix.get(f: string, t: string) {
    return this.X(rows.get(f), cols.get(t));
 }

/*
 Set the value of this.X(i,j) by index row
 */
proc NamedMatrix.set(i: int, j: int, w: real) {
    if this.SD.member((i,j)) {
      this.X(i,j) = w;
    } else {
      this.SD += (i,j);
      this.X(i,j) = w;
    }
    return w;
}

/*
Set the values using the row and column names
*/
proc NamedMatrix.set(f: string, t: string, w: real) {
  return this.set(rows.get(f), cols.get(t), w);
}

/*
Update the value in X(i,j) with `w`
 */
proc NamedMatrix.update(i: int, j: int, w: real) {
  const x = this.get(i,j);
  return this.set(i,j, x + w);
}

/*
Update the value in X(i,j) with `w` using the row column names. This adds
`w` to the current value. To replace the current value, use `.set()`
 */
proc NamedMatrix.update(f: string, t: string, w: real) {
  const x = this.get(rows.get(f),cols.get(t));
  return this.set(rows.get(f),cols.get(t), x + w);
}

/*
 Removes the matrix entry `(i,j)` from the matrix and the underlying domain
 by row, column ids
 */
proc NamedMatrix.remove(i: int, j:int) {
  this.SD -= (i,j);
}

/*
 Removes the matrix entry `(i,j)` from the matrix and the underlying domain
 by row, column names
 */
proc NamedMatrix.remove(i: string, j:string) {
  this.remove(this.rows.get(i), this.cols.get(j));
}

/*
Find the number of non-zeroes in the matrix
 */
proc NamedMatrix.nnz() {
  var i: int = 0;
  for ij in this.X.domain {
    i += 1;
  }
  return i;
}

/*
Calculates the sparsity of the matrix: Number of entries / frame size
 */
proc NamedMatrix.sparsity() {
  const d = this.X.shape[1]:real * this.X.shape[2]: real;
  return this.nnz():real / d;
}

/*
 Takes a index (i,j) tuple and turns it into the sequential entry number on the
 given matrix.
 */
proc NamedMatrix.grid2seq(ij: 2*int) {
  return this.ncols() * ij[1] + ij[2] - this.ncols();
}
/*
 Takes a index values `i,j` and turns it into the sequential entry number on the
 given matrix.
 */
proc NamedMatrix.grid2seq(i: int, j: int) {
  return this.grid2seq((i,j));
}


/*
 Takes a sequential value and gives the row / column indices of that entry
 */
proc NamedMatrix.seq2grid(k: int) {
  return (divfloor(k, this.ncols())+1, k % this.ncols());
}

proc NamedMatrix.ntropic(N: NamedMatrix) {
  var T: NamedMatrix = new NamedMatrix(X = tropic(this.X, N.X), this.rows, N.cols);
  return T;
}

/*
 Multiplies the current NamedMatrix `X` against the argument `Y`, but first it aligns
 the names of `X.cols` with `Y.rows`.  Returns an appropriately named NamedMatrix
 :arg NamedMatrix Y:
 */
proc NamedMatrix.ndot(N: NamedMatrix) {
  var C: NamedMatrix = new NamedMatrix(X = dot(this.X,N.X), this.rows, N.cols);
  return C;
}


/*
 Routine to take two NamedMatrices and multiply them along just the row/column
 intersections.  If X has cols [red yellow blue green] and Y has rows [red blue green]
 this function produces a NamedMatrix with rows [red blue green]
 */
proc NamedMatrix.alignAndMultiply(Y: NamedMatrix) {
    var rcOverlap: domain(string) = this.cols.keys & Y.rows.keys;

    var xSD: sparse subdomain(this.D) dmapped CS(),
        ySD: sparse subdomain(Y.D) dmapped CS(),
        xX: [xSD] real,  // the actual data
        yX: [ySD] real,  // the actual data
        xrows: BiMap = new BiMap(),
        yrows: BiMap = new BiMap(),
        xcols: BiMap = new BiMap(),
        ycols: BiMap = new BiMap();

    for (left_row, right_col) in zip(this.D.dim(2), Y.D.dim(1)) {
      for rc in rcOverlap {
        const j = this.cols.get(rc);
        if this.SD.member(left_row, j) {
          xSD += (left_row, j);
          xX[left_row, j] = this.X[left_row, j];
        }
        if Y.SD.member(j, right_col) {
          ySD += (j, right_col);
          yX[j, right_col] = Y.X[j, right_col];
        }
      }
    }
    const z = xX.dot(yX);
    var n = new NamedMatrix(X=z);
    n.rows = this.rows;
    n.cols = Y.cols;
    return n;
}

/*
 Designed to be similar to SciPy's `amax() <https://docs.scipy.org/doc/numpy/reference/generated/numpy.amax.html>`_
 Useful on sparse matrices

 We'll get around to documenting here it eventually
 */
proc aMax(X:[], axis = 0) {
  var is: domain(1) = 1..max(X.domain.dim(1).size,X.domain.dim(2).size);
  var sis: sparse subdomain(is);
  var maxima: [sis] real;
  if axis == 0 {
    var wDom: domain(1) = 1..X.domain.size;
    var ws = reshape(X, wDom);
    sis += 1;
    maxima[1] = (max reduce ws);
  }
  if axis == 2 {
    is = X.domain.dim(1);
    for i in is {
      var wDom: domain(1);
      var ws: [wDom] real;
      for a in transpose(X).domain.dimIter(2, i) {
        sis += i;
        ws.push_back(X(a,i));
      }
      if sis.member(i) {
        maxima[i] = max reduce ws;
      }
    }
  } else if axis == 1 {
    is = X.domain.dim(2);
    for i in is {
      var wDom: domain(1);
      var ws: [wDom] real;
      for a in X.domain.dimIter(2, i) {
        sis += i;
        ws.push_back(X(i,a));
      }
      if sis.member(i) {
        maxima[i] = max reduce ws;
      }
    }
  }
  return maxima;
}

/*
 Designed to be similar to SciPy's `amin() <https://docs.scipy.org/doc/numpy/reference/generated/numpy.amin.html>`_
 Useful on sparse matrices.

 We'll get around to documenting here it eventually
 */
proc aMin(X:[], axis: int) {
  var is: domain(1) = 1..max(X.domain.dim(1).size,X.domain.dim(2).size);
  var sis: sparse subdomain(is);
  var minima: [sis] real;
  if axis == 0 {
    var wDom: domain(1) = 1..X.domain.size;
    var ws = reshape(X, wDom);
    sis += 1;
    minima[1] = (min reduce ws);
  }
  if axis == 2 {
    is = X.domain.dim(1);
    for i in is {
      var wDom: domain(1);
      var ws: [wDom] real;
      for a in transpose(X).domain.dimIter(2, i) {
        sis += i;
        ws.push_back(X(a,i));
      }
      if sis.member(i) {
        minima[i] = min reduce ws;
      }
    }
  } else if axis == 1 {
    is = X.domain.dim(2);
    for i in is {
      var wDom: domain(1);
      var ws: [wDom] real;
      for a in X.domain.dimIter(2, i) {
        sis += i;
        ws.push_back(X(i,a));
      }
      if sis.member(i) {
        minima[i] = min reduce ws;
      }
    }
  }
  return minima;
}


/*
Provides the "where" of the min along an axis in a sparse matrix
 */
proc argMin(X:[], axis: int) {
  var is: domain(1) = 1..max(X.domain.dim(1).size,X.domain.dim(2).size);
  var sis: sparse subdomain(is);
  var minima: [sis] 2*int;
  if axis == 0 {
    sis += 1;
    minima[1] = miniLoc_(axis == 0, id = 1, X);
  } else if axis == 2 {
    for i in is {
      var idm = miniLoc_(1,i,X);
      if idm != (0,0) {
        sis += i;
        minima[i] = idm;
      }
    }
  } else if axis == 1 {
    for i in is {
      var idm = miniLoc_(2,i,X);
      if idm != (0,0) {
        sis += i;
        minima[i] = idm;
      }
    }
  }
  return minima;
}

/*
 Provides the "where" of a max along a given axis in a sparse matrix
 */
proc argMax(X:[], axis: int) {
  var is: domain(1) = 1..max(X.domain.dim(1).size,X.domain.dim(2).size);
  var sis: sparse subdomain(is);
  var maxima: [sis] 2*int;
  if axis == 0 {
    sis += 1;
    maxima[1] = maxiLoc_(axis == 0, id = 1, X);
  } else if axis == 2 {
    for i in is {
      var idm = maxiLoc_(1,i,X);
      if idm != (0,0) {
        sis += i;
        maxima[i] = idm;
      }
    }
  } else if axis == 1 {
    for i in is {
      var idm = maxiLoc_(2,i,X);
      if idm != (0,0) {
        sis += i;
        maxima[i] = idm;
      }
    }
  }
  return maxima;
}


// HELPER FUNCTIONS SHOULD NOT BE EXPOSED TO USER
proc miniLoc_(axis:int, id, X:[]) {
  var idm: (int,int),
      minimum: real = 1000000;
  if axis == 0 {
    for (i,j) in X.domain {
      if X(i,j) < minimum {
        minimum = X(i,j);
        idm = (i,j);
      }
    }
  }
  if axis == 1 {
    for a in transpose(X).domain.dimIter(2, id) {
      if X(a,id) < minimum {
        minimum = X(a,id);
        idm = (a,id);
      }
    }
  }
  if axis == 2 {
    for a in X.domain.dimIter(2, id) {
      if X(id,a) < minimum {
        minimum = X(id,a);
        idm = (id,a);
      }
    }
  }
  return idm;
}

proc maxiLoc_(axis:int, id, X:[]) {
  var idm: (int,int),
      maximum: real;
  if axis == 0 {
    for (i,j) in X.domain {
      if X(i,j) > maximum {
        maximum = X(i,j);
        idm = (i,j);
      }
    }
  }
  if axis == 1 {
    for a in transpose(X).domain.dimIter(2, id) {
      if X(a,id) > maximum {
        maximum = X(a,id);
        idm = (a,id);
      }
    }
  }
  if axis == 2 {
    for a in X.domain.dimIter(2, id) {
      if X(id,a) > maximum {
        maximum = X(id,a);
        idm = (id,a);
      }
    }
  }
  return idm;
}


proc tropic(A:[],B:[]) {
  var dom: domain(2) = {A.domain.dim(1),B.domain.dim(2)};
  var sps = CSRDomain(dom);
//  var BT = transpose(B);
  var T: [sps] real;
  for (i,j) in dom {
    var t: Timer;
    t.start();
    var mini = 100000000: real;
    if A.domain.member((i,j)) {
      mini = min(mini,A(i,j));
    }
    if B.domain.member((i,j)) {
      mini = min(mini,B(i,j));
    }
    for w in A.domain.dimIter(2,i) {
      if B.domain.member((w,j)) {
        mini = min(mini,A(i,w) + B(w,j));
      }
    }
    t.stop();
    if mini < 100000000 {
      sps += (i,j);
      T(i,j) = mini;
      writeln((i,j));
      writeln("",t.elapsed());
    }
  }
  return T;
}

proc tropic_(A:[],B:[]) {
  var dom: domain(2) = {A.domain.dim(1),B.domain.dim(2)};
  var sps = CSRDomain(dom);
  var BT = transpose(B);
  var T: [sps] real;
  for (i,j) in dom {
    var mini = 100000000: real;
    var wdom: domain(1);
    var wids: [wdom] int;
    for w in A.domain.dimIter(2,i) {
      wids.push_back(w);
    }
    //writeln(wids);
    for w2 in BT.domain.dimIter(2,j) {
      wids.push_back(w2);
    }
    var wDom: domain(1);
    var witness: [wDom] real;
    for w in wids {
      if A(i,w) > 0 && BT(j,w) > 0 {
        witness.push_back(A(i,w) + BT(j,w));
      }
    }
    if A.domain.member((i,j)) {
      witness.push_back(A(i,j));
    }
    if B.domain.member((i,j)) {
      witness.push_back(B(i,j));
    }
    mini = min reduce witness;
    if mini < 1000000000 {
      sps += (i,j);
      T(i,j) = mini;
    }
  }
  return T;
}


proc sparseEq(A:[], B:[]) {
  var s: bool = true;
  forall (i,j) in A.domain with (&& reduce s) {
    s &&= A(i,j) == B(i,j);
  }
  return s;
}


/*
proc adjacencyPolynomial(A:[]) {

}*/



proc tropicLimit(A:[] real,B:[] real): A.type {
 var R = tropic(A,B);
 if A.domain == R.domain {
   if sparseEq(R,A) {
     return R;
   } else {
     return tropicLimit(R,B);
   }
 } else {
   return tropicLimit(R,B);
 }
}



/*
 Build a random sparse matrix.  Good for testing;
 */
proc generateRandomSparseMatrix(size: int, sparsity: real) {
  const D: domain(2) = {1..size, 1..size};
  var SD: sparse subdomain(D) dmapped CS();
  var R: [SD] real;
  var da: domain(1) = {1..size};
  var array: [da] int = 1..size;
  var dom: domain(1) = {1..0};
  var indices: [dom] 2*int;
  var N = floor(size*(1-sparsity)): int;
  forall i in array {
    forall j in array {
      indices.push_back((i,j));
    }
  }
  shuffle(indices);
  var sparseids = indices[1..N];
  SD.bulkAdd(sparseids);
  forall (i,j) in sparseids {
    R(i,j) = 1;
  }
  //ndices = zip(array1, array2)
  return R;
}


/*
 General sparsity function for any `real` matrix.
 */
proc sparsity(X) {
  const d = X.shape[1]:real * X.shape[2]: real;
  var i: real = 0.0;
  for ij in X.domain {
    i += 1.0;
  }
  return i / d;
}

proc elemMult(A: [], B: []) {
  var dom: domain(2) = {A.domain.dim(1),A.domain.dim(2)};
  var sps = CSRDomain(dom);
  var C: [sps] real;
  for (i,j) in A.domain {
    if B.domain.member((i,j)) {
      sps += (i,j);
      C(i,j) = A(i,j) * B(i,j);
    }
  }
  return C;
}

/*
 Error classes
 */
class NumSuchError : Error {
  proc init() {
    super.init();
    this.complete();
  }
  proc message() {
    return "Generic NumSuch Error";
  }
}

proc identityMat(n: int) {
  var dom: domain(2) = {1..n,1..n};
  var sps = CSRDomain(dom);
  for i in 1..n {
    sps += (i,i);
  }
  var I: [sps] real;
  forall ij in sps {
    I(ij) = 1;
  }
  return I;
}

proc ones(d: domain(2), v: real=1.0) {
  var m: [d] real = v;
  return m;
}

/*
Only works on dense matrices at the moment
*/
proc rowSums(X: []) {
  var v:[X.domain.dims()(1)] real;
  for i in v.domain {
    v[i] = + reduce X[i,..];
  }
  return v;
}

/*
Only works on dense matrices at the moment
*/
proc colSums(X: []) {
  var v:[X.domain.dims()(2)] real;
  for i in v.domain {
    v[i] = + reduce X[..,i];
  }
  return v;
}



/*
 Used to indidcate dimension mismatches on NamedMatrices and vectors
 */
class DimensionMatchError : NumSuchError {
  var expected: int,
      actual: int;

  proc init(expected: int, actual:int) {
    super.init();
    this.complete();
    this.expected = expected;
    this.actual = actual;
  }

  proc message() {
    return "Error matching dimensions.  Expected: " + this.expected + " Actual: " + this.actual;
  }
}

/*
 Concatenates two vectors (x, y) -> [x y]
 */
proc concat(x:[], y:[]) {
  const d:int = x.size + y.size;
  var v:[1..d] x.eltType;

  var k = 1;
  for i in x.domain {
    v[k] = x[i];
    k += 1;
  }
  for i in y.domain {
    v[k] = y[i];
    k += 1;
  }
  return v;
}

/*
 Concatenates a matrix and a vector, adding the vector
 as extra columns

[ [1 2]
 [3 4] ], [ 5, 6]   ->

 [[1 2 5 6]
  [3 4 5 6]]
 */
proc concatRight(X:[], y:[]) {
    const h = X.shape[1],
          w = X.shape[2] + y.size;
    var A: [1..h, 1..w] X.eltType;

    for i in 1..h {
      const r = concat(X[i,..], y);
      A[i,..] = r;
    }
    return A;
}
