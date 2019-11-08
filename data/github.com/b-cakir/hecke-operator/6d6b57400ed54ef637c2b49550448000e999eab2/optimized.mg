/*****************************************************************************

Optimized Version

The following program computes a transformation matrix of a Hecke operator T_P
for a polynomial P in R = F_q[T] with q = p^n, which operates on the vector
space C_h(Gamma, X) of all Gamma-equivariant harmonic cocycles with values in
a finite-dimensional vector space X over a field of prime characteristic,
where Gamma is a congruence subgroup of GL_2(R) with regards to a polynomial N
in R.

For this purpose, there essentially are four steps. The first part of the
program computes the quotient graph Gamma \ BTT where BTT denotes the
Bruhat-Tits tree. Then, we write a function "cocycle" which will calculate the
value of a Gamma(N)- or Gamma_1(N)-equivariant cocycle on an arbitrary edge of
the BTT. In the third part of the program, we are interested in a function
which can compute a basis for C_h(Gamma, X) if Gamma is Gamma_0^1(N),
Gamma_0(N), or SL_2(R). Finally, we conclude the program in the forth part
with the function "operator".

Throughout the whole program, I will be regularly citing my master thesis,
where all major algorithms are detailed.

Update Note: In the main.mg program the operation is implemented by
matrix multiplication, but this version realizes it through a vector space
homomorphism (see function "operation"). Additionally, the last function
"operator" saves computed graphs in a list (see function description for
further information).

Copyright (C) 2018 Burak Cakir

*****************************************************************************/

/*
 * Part 1: The Quotient Graph Gamma \ BTT
 */

/*
 * The aim of the first part is to calculate the quotient graph Gamma \ BTT
 * where Gamma is a congruence subgroup of GL_2(R) and BTT the Bruhat-Tits
 * tree. For this purpose, we first implement a system of representative of
 * Gamma \ GL_2(R). Afterwards, we compute the quotient graph.
 *
 * Throughout the program, let R := F_q[T] with q = p^n. A detailed
 * explanation of Part 1 can be found in Section 2.6 of my master thesis.
 */
 
/*
 * The function "lift" takes two polynomials x neq 0 and y with
 * gcd(x, y, N) = 1 and returns a polynomial t such that
 * gcd(x, y + t * N) = 1.
 */

function lift(x, y)
	e := 1;
	t := 0;
	g := Gcd(x, y);
	q := Factorization(x);
	n := #q;
	
	for i := 1 to n do
		e := e * q[i][1];
	end for;
	
	for i := 1 to n do
		p := e div q[i][1];
		a, b, c := Xgcd(p, q[i][1]);
		
		if g mod q[i][1] eq 0 then
			t := t + b * p;
		end if;
	end for;
	
	return t;
end function;

/*************** Representative system for Gamma(N) \ GL_2(R) ****************/

/*
 * The following function compute a system of representatives for
 * Gamma(N) \ GL_2(R), according to the algorithm in Lemma 2.28 and
 * Theorem 2.29
 */

function gammasystem(R, N)
	F := BaseRing(R);
	Q := quo<R|N>;
	list := [];
	sl := {};
	
	for a, b, c, d in Q do
		if a * d - b * c eq 1 then
			Include(~sl, Matrix(Q, 2, 2, [a, b, c, d]));
		end if;
	end for;
	
	for M in sl do
		a := R ! M[1][1];
		b := R ! M[1][2];
		c := R ! M[2][1];
		d := R ! M[2][2];
		
		if a * d - b * c eq 1 then
			for x in F do
				if x ne 0 then
					A := Matrix(R, 2, 2, [x, 0, 0, 1]);
					B := Matrix(R, 2, 2, [a, b, c, d]);
					Append(~list, A * B);
				end if;
			end for;
		else
			r := (a * d - b * c - 1) div N;
			
			if c ne 0 then
				t := lift(c, d);
				d := d + t * N;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r + a * t) * g * N;
				b_new := b + (r + a * t) * f * N;
			else
				s := lift(d, c);
				c := c + s * N;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r - b * s) * g * N;
				b_new := b + (r - b * s) * f * N;
			end if;
			
			for x in F do
				if x ne 0 then
					A := Matrix(R, 2, 2, [x, 0, 0, 1]);
					B := Matrix(R, 2, 2, [a_new, b_new, c, d]);
					Append(~list, A * B);
				end if;
			end for;
		end if;
	end for;
	
	return list;
end function;

/************** Representative system for Gamma_1(N) \ GL_2(R) ***************/

function gamma1system(R, N)
	F := BaseRing(R);
	Q := quo<R|N>;
	list := [];
	
	for x, y in Q do
		u := R ! x;
		v := R ! y;
		e := Gcd(Gcd(u, v) mod N, N);
		
		if e eq 1 then
			if Gcd(u, v) eq 1 then
				e, r, s := Xgcd(u, v);
				A := Matrix(R, 2, 2, [s, -r, u, v]);
			else
				if u ne 0 then
					t := lift(u, v);
					v := v + t * N;
					e, r, s := Xgcd(u, v);
					A := Matrix(R, 2, 2, [s, -r, u, v]);
				else
					t := lift(v, u);
					u := u + t * N;
					e, r, s := Xgcd(u, v);
					A := Matrix(R, 2, 2, [s, -r, u, v]);
				end if;
			end if;
			
			for z in F do
				if z ne 0 then
					B := Matrix(R, 2, 2, [z, 0, 0, 1]);
					Append(~list, A * B);
				end if;
			end for;
		end if;
	end for;
	
	return list;
end function;

/************* Representative system for Gamma_0^1(N) \ GL_2(R) **************/

function gamma01system(R, N)
	F := BaseRing(R);
	Q := quo<R|N>;
	PS := ProjectiveSpace(Q,1);
	list := [];
	p1 := [PS|];
	
	for x in car<Q, Q> do
		x1, x2 := [x[1], x[2]] in PS;
		
		if x1 eq true then
			if x2 notin p1 then
				Append(~p1, x2);
			end if;
		end if;
	end for;
	
	for x in p1 do
		u := R ! x[1];
		v := R ! x[2];
		e, r, s := Xgcd(u, v);
		
		if e eq 1 then
			A := Matrix(R, 2, 2, [s, -r, u, v]);
		else
			if u ne 0 then
				t := lift(u, v);
				v := v + t * N;
				e, r, s := Xgcd(u, v);
				A := Matrix(R, 2, 2, [s, -r, u, v]);
			else
				t := lift(v, u);
				u := u + t * N;
				e, r, s := Xgcd(u, v);
				A := Matrix(R, 2, 2, [s, -r, u, v]);
			end if;
		end if;
		
		for z in F do
			if z ne 0 then
				B := Matrix(R, 2, 2, [z, 0, 0, 1]);
				Append(~list, A * B);
			end if;
		end for;
	end for;
	
	return list;
end function;

/************** Representative system for Gamma_0(N) \ GL_2(R) ***************/

function gamma0system(R, N)
	F := BaseRing(R);
	Q := quo<R|N>;
	PS := ProjectiveSpace(Q,1);
	list := [];
	p1 := [PS|];
	
	for x in car<Q, Q> do
		x1, x2 := [x[1], x[2]] in PS;
		
		if x1 eq true then
			if x2 notin p1 then
				Append(~p1, x2);
			end if;
		end if;
	end for;
	
	for x in p1 do
		u := R ! x[1];
		v := R ! x[2];
		e, r, s := Xgcd(u, v);
		
		if e eq 1 then
			Append(~list, Matrix(R, 2, 2, [s, -r, u, v]));
		else
			if u ne 0 then
				t := lift(u, v);
				v := v + t * N;
				e, r, s := Xgcd(u, v);
				Append(~list, Matrix(R, 2, 2, [s, -r, u, v]));
			else
				t := lift(v, u);
				u := u + t * N;
				e, r, s := Xgcd(u, v);
				Append(~list, Matrix(R, 2, 2, [s, -r, u, v]));
			end if;
		end if;
	end for;
	
	return list;
end function;

/************** Representative system for Gamma_0(N) \ GL_2(R) ***************/

function gl2sl2(R)
	F := BaseRing(R);
	list := [];
	
	for x in F do
		if x ne 0 then
			Append(~list, Matrix(R, 2, 2, [x, 0, 0, 1]));
		end if;
	end for;
	
	return list;
end function;

/*
 * Before we introduce a function which calculates the quotient graphs, we
 * need some auxiliary functions.
 */

/*
 * The following function computes all polynomials in R[T] with degree =< n.
 */

function polynomials(R, n)
	F := BaseRing(R);
	T := Name(R, 1);
	set := {R|};
	
	for x in F do
		Include(~set, x);
	end for;
	
	if n eq 0 then
		return set;
	else
		for i := 1 to n do
			set_temp := set;
			for j in F do
				for k in set_temp do
					Include(~set, j*T^i + k);
				end for;
			end for;
		end for;
		
		return set;
	end if;
end function;

/*
 * "stabilizer" provides the group G_n, which is the stabilizer of the vertex
 * Lambda_n in GL_2(R).
 */

function stabilizer(R, n)
	F := BaseRing(R);
	set := {};
	polset := polynomials(R, n);
	
	if n eq 0 then
		for a, b, c, d in F do
			if a * d - b * c ne 0 then
				Include(~set, Matrix(R, 2, 2, [a, b, c, d]));
			end if;
		end for;
	else
		for b in polset do
			for a, d in F do
				if a ne 0 and d ne 0 then
					Include(~set, Matrix(R, 2, 2, [a, b, 0, d]));
				end if;
			end for;
		end for;
	end if;
	
	return set;
end function;

/*
 * Next, we define a function, which checks if a matrix x is in one of the
 * congruence subgroups of interest. The argument type sets the congruence
 * subgroup, 0 meaning Gamma(N), 1 for Gamma_1(N), 2 for Gamma_0^1(N),
 * 3 for Gamma_0(N), and 4 for SL_2(R).
 */

function inGamma(R, N, type, x)
	a := R ! x[1][1];
	b := R ! x[1][2];
	c := R ! x[2][1];
	d := R ! x[2][2];
	
	if type eq 0 then
		if (a mod N eq 1) and (b mod N eq 0) and (c mod N eq 0) and (d mod N eq 1) then
			return true;
		end if;
	elif type eq 1 then
		if (a mod N eq 1) and (c mod N eq 0) and (d mod N eq 1) then
			return true;
		end if;
	elif type eq 2 then
		if (c mod N eq 0) and (Determinant(x) eq 1) then
			return true;
		end if;
	elif type eq 3 then
		if (c mod N eq 0) then
			return true;
		end if;
	elif type eq 4 then
		if Determinant(x) eq 1 then
			return true;
		end if;
	end if;
	
	return false;
end function;

/*
 * Finally, the following function "graph" calculates the quotient graph. The
 * argument type sets the congruence subgroups, 0 meaning Gamma(N), 1 for
 * Gamma_1(N), 2 for Gamma_0^1(N), 3 for Gamma_0(N), and 4 for SL_2(R).
*/

function graph(R, N, type)
	
	if type eq 0 then
		reps := gammasystem(R, N);
	elif type eq 1 then
		reps := gamma1system(R, N);
	elif type eq 2 then
		reps := gamma01system(R, N);
	elif type eq 3 then
		reps := gamma0system(R, N);
	elif type eq 4 then
		reps := gl2sl2(R);
	end if;
	
	string := "graph G {\n  node [shape=point width=0.1];\n";
	ActualNumber := [];
	
	n := Degree(N) + 1;	// number of columns
	m := #reps;		// number of rows
	
	// Let A be a (m * n)x(m * n)-matrix.
	
	A := ZeroMatrix(IntegerRing(), m * n, m * n);
	
	// Set the value for the initial situation where we have m copies of
	// GL_2(R) \backslash \mathcal{T}.
	
	for i := 1 to m do
		for j := 1 to n - 1 do
			A[i + (j - 1) * m][i + j * m] := 1;
			A[i + j * m][i + (j - 1) * m] := -1;
		end for;
	end for;
	
	relevant := [true : z in [1..m*n]];
	
	// Edges between stage 0 and 1
	
	list := [];
	
	for i := 1 to m do
		Include(~list, < i , m + i >);
	end for;
	
	// Identify the edges between stage 0 and 1.
	
	stab0 := stabilizer(R, 0);
	stab1 := stabilizer(R, 1);
	stab01 := stab0 meet stab1;
	
	for i := 2 to m do
		if relevant[i] then
			for j := 1 to i - 1 do
				if relevant[j] then
					for g in stab01 do
						if inGamma(R, N, type, reps[j] * g * reps[i]^(-1)) then
							for l := 0 to n - 1 do
								relevant[i + l * m] := false;
							end for;
							
							list[i] := < 0, 0 >;
							
							break j;
						end if;
					end for;
				end if;
			end for;
		end if;
	end for;
	
	// Identify vertices of stage 0.
	
	for i := 2 to m do
		if relevant[i] then
			for j := 1 to i - 1 do
				if relevant[j] then
					for g in stab0 do
						if inGamma(R, N, type, reps[j] * g * reps[i]^(-1)) then
							relevant[i] := false;
							
							A[i + m][i] := 0;
							A[j][i + m] := 1;
							A[i + m][j] := -1;
							
							list[i][1] := j;
							
							break j;
						end if;
					end for;
				end if;
			end for;
		end if;
	end for;
	
	// Identify vertices of stage \geq 1 (and the corresponding edges).
	
	for l := 1 to n - 1 do
		stab := stabilizer(R, l);
		
		for i := 2 to m do
			if relevant[i + l * m] then
				for j := 1 to i - 1 do
					if relevant[j + l * m] then
						for g in stab do
							if inGamma(R, N, type, reps[j] * g * reps[i]^(-1)) then
								for k := 1 to m do
									if A[k + (l - 1) * m][i + l * m] ne 0 then
										A[k + (l - 1) * m][j + l * m] := A[k + (l - 1) * m][j + l * m] + A[k + (l - 1) * m][i + l * m];
										A[j + l * m][k + (l - 1) * m] := A[j + l * m][k + (l - 1) * m] + A[i + l * m][k + (l - 1) * m];
										A[k + (l - 1) * m][i + l * m] := 0;
									end if;
								end for;
								
								for h := 0 to n - 1 - l do
									relevant[i + (l + h) * m] := false;
								end for;
								
								if l eq 1 then
									list[i][2] := j + l * m;
								end if;
								
								break j;
							end if;
						end for;
					end if;
				end for;
			end if;
		end for;
	end for;
	
	// Erase the rows and columns of vertices which do not exist anymore.
	
	x := 0;
	
	for i := 1 to m * n do
		if not relevant[i] then
			RemoveRow(~A, i - x);
			RemoveColumn(~A, i - x);
			x := x + 1;
		else
			ActualNumber := Include(ActualNumber, i);
		end if;
	end for;
	
	number := NumberOfRows(A);
	
	Graph := MultiDigraph< number | >;
	
	B := A;
	
	counter := 0;
	
	for i := 1 to number do
		AssignLabel(~Graph, VertexSet(Graph).i, Sprintf("%o", ActualNumber[i]));
		
		for j := 1 to number do
			while B[i][j] gt 0 do
				for k := 1 to m do
					if list[k][1] eq ActualNumber[i] and list[k][2] eq ActualNumber[j] then
						AddEdge(~Graph, VertexSet(Graph).i, VertexSet(Graph).j, Sprintf("%o", k));
						counter := 1;
						list[k] := < 0, 0 >;
						break k;
					end if;
				end for;
				
				if counter ne 1 then
					AddEdge(~Graph, VertexSet(Graph).i, VertexSet(Graph).j);
				else
					counter := 0;
				end if;
				
				B[i][j] := B[i][j] - 1;
				line := Sprintf("  %o -- %o;\n", i, j);
				string := string cat line;
			end while;
		end for;
	end for;
	
	string := string cat "}\n\n";
	
	return Graph, reps, string;
end function;

/*
 * Part 2: Evaluating a Cocycle
 */

/*
 * The following program computes the value of a Gamma-equivariant harmonic
 * cocycle on an edge of the Bruhat-Tits tree. Throughout the program, let
 * R := F_q[T] with q = p^n and K := Quot(R).
 *
 * Keep in mind, this part is dependent on the previous part. For
 * further information on the exact approach, please consider Section 3.5 of
 * my master thesis, which I will be refering to quite often in the following
 * lines.
 */

/*
 * Before we begin, we need to construct a valuation map for elements in K and
 * a function "series", which writes an element of K as a finite sum in (1/T).
 * In my master thesis, I explain why I avoid using "LaurentSeriesRing" of
 * Magma and its functions "Valuation" and "Coefficient".
 *
 * If necessary, extend the precision in "C := LaurentSeriesRing(F, n + 100)".
 */

function valuation(x)
	if x eq 0 then
		return Infinity;
	else
		return Degree(Denominator(x)) - Degree(Numerator(x));
	end if;
end function;

function series(R, x, n)
	F := BaseRing(R);
	K := FieldOfFractions(R);
	C := LaurentSeriesRing(F, n + 100);
	T := Name(K, 1); pi := Name(C, 1);
	emb := hom< K -> C | 1/pi >;
	
	y := K ! 0;
	
	if x eq 0 then
		return x;
	else
		val := Valuation(emb(x));
		for i := val to n do
			y := y + Coefficient(emb(x), i) * (1/T)^i;
		end for;
	end if;
	
	return y;
end function;

/*
 * The function "normal_form" computes the representative of a vertex A with
 * entries in K. (see proof of Lemma 1.14)
 */

function normal_form(R, A)
	K := FieldOfFractions(R);
	T := Name(K, 1);
	
	// The matrix A has to be invertible.
	
	if not IsInvertible(A) then
		return "The matrix A has to be invertible.";
	end if;
	
	// First, achieve v(c) ge v(d).
	
	if A[2][1] eq 0 then
		A := A;
	elif A[2][2] eq 0 then
		A := A * Matrix(K, 2, 2, [0, 1, 1, 0]);
	elif valuation(A[2][1]) lt valuation(A[2][2]) then
		A := A * Matrix(K, 2, 2, [0, 1, 1, 0]);
	end if;
	
	// In the next step, get a matrix [pi^n, y, 0, 1].
	
	A := A * Matrix(K, 2, 2, [1, 0, -A[2][1]/A[2][2], 1]);
	A := A * Matrix(K, 2, 2, [1/A[2][2], 0, 0, 1/A[2][2]]);
	
	n := valuation(A[1][1]);
	
	// Determine y, which is given mod pi^n.
	
	if A[1][2] eq 0 then
		return Matrix(K, 2, 2, [(1/T)^n, 0, 0, 1]);
	elif valuation(A[1][2]) ge n then
		return Matrix(K, 2, 2, [(1/T)^n, 0, 0, 1]);
	else
		return Matrix(K, 2, 2, [(1/T)^n, series(R, A[1][2], n - 1), 0, 1]);
	end if;
end function;

/*
 * A normal form of a vertex can now be assigned to a standard lattice in the
 * half-line GL_2(R) \ BTT. It is assumed that the matrix A is already in its
 * normal form. (see proof of Theorem 1.21)
 */

function half_line(R, A)
	K := FieldOfFractions(R);
	T := Name(K, 1);
	
	B := Matrix(R, 2, 2, [1, 0, 0, 1]);	// This will be its representative in GL_2(R).
	
	n := valuation(A[1][1]);
	
	// Consider the polynomial and non-polynomial part of y.
	
	if A[1][2] eq 0 then
		y_pol := 0;
	elif valuation(A[1][2]) le 0 then
		y_pol := series(R, A[1][2], 0);
	else
		y_pol := 0;
	end if;
	
	y_non := A[1][2] - y_pol;
	
	// Continue with the non-polynomial part of y.
	
	A := Matrix(K, 2, 2, [A[1][1], y_non, 0, 1]);
	B := Matrix(R, 2, 2, [1, -y_pol, 0, 1]) * B;
	
	// Assign A to a vertex in the half-line GL_2(R)\T.
	
	if (n le 0) or (A[1][2] eq 0) then
		if n gt 0 then
			B := Matrix(R, 2, 2, [0, 1, 1, 0]) * B;
			return B^(-1), Matrix(K, 2, 2, [1, 0, 0, (1/T)^n]);
		else
			return B^(-1), Matrix(K, 2, 2, [1, 0, 0, (1/T)^(-n)]);
		end if;
	else
		while valuation(A[1][1]) gt 0 and A[1][2] ne 0 do
			A := Matrix(K, 2, 2, [(1/T)^(valuation(A[1][1]) - 2 * valuation(A[1][2])), 1/A[1][2], 0, 1]);
			B := Matrix(R, 2, 2, [0, 1, 1, 0]) * B;
			
			A := normal_form(R, A);
			
			if A[1][2] eq 0 then
				y_pol := 0;
			elif valuation(A[1][2]) le 0 then
				y_pol := series(R, A[1][2], 0);
			else
				y_pol := 0;
			end if;
			
			y_non := A[1][2] - y_pol;
			
			A := Matrix(K, 2, 2, [A[1][1], y_non, 0, 1]);
			B := Matrix(R, 2, 2, [1, -y_pol, 0, 1]) * B;
		end while;
		
		if valuation(A[1][1]) gt 0 then
			B := Matrix(R, 2, 2, [0, 1, 1, 0]) * B;
			return B^(-1), Matrix(K, 2, 2, [1, 0, 0, (1/T)^(valuation(A[1][1]))]);
		else
			return B^(-1), Matrix(K, 2, 2, [1, 0, 0, (1/T)^(-valuation(A[1][1]))]);
		end if;
	end if;
end function;

/*
 * Finally, if A is a matrix in GL_2(K) representing an edge of the
 * Bruhat-Tits tree BTT and sigma * standard its representative on the
 * quotient graph GL_2(R) \ BTT, find its representative in the quotient graph
 * Gamma \ BTT. The following function does that.
 */

function quotient_rep(R, N, type, sigma, standard)
	K := FieldOfFractions(R);
	T := Name(K, 1);

	if type eq 0 then
		system := gammasystem(R, N);
	elif type eq 1 then
		system := gamma1system(R, N);
	end if;
	
	for g in system do
		if inGamma(R, N, type, sigma * g^(-1)) then
			return sigma * g^(-1), g, standard;
		end if;
	end for;
end function;

/*
 * Up until now, the program takes an edge represented by the matrix A,
 * computes its normal form A = [pi^n, y, 0, 1], divides it into
 * A = B * [1, 0, 0, pi^n] with a matrix B in GL_2(R), and then further splits
 * it into B = gamma * repr, where gamma is in Gamma and repr in
 * Gamma \ GL_2(R). Now, we will find alpha, beta, and delta such that
 * repr = beta * alpha * delta^(-1), where alpha is an representative of the
 * edge of stage n, with which repr has been identified; beta is an
 * element of Gamma; and delta is an element of the stabilizer of the edge
 * [1, 0, 0, pi^n]. Thus, there will hold A = gamma * beta * alpha *
 * delta^(-1) * [1, 0, 0, pi^n] = (gamma * beta) * alpha * [1, 0, 0, pi^n].
 */

function final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard)
	m := #Reps;
	n := Degree(N);
	
	stageA := valuation(standard[2][2]);
	stage := [];
	reprs := [];
	
	E := EdgeSet(Graph);
	
	if stageA eq 0 then
		stabA := stabilizer(R, 0) meet stabilizer(R, 1);
	else
		stabA := stabilizer(R, stageA);
	end if;
	
	for i := 1 to #E do
		ActualNumber1 := StringToInteger(Label(InitialVertex(E.i)));
		
		if ActualNumber1 le m then
			ActualNumber2 := StringToInteger(Label(E.i));
		else
			ActualNumber2 := ActualNumber1;
		end if;
		
		if ActualNumber2 mod m eq 0 then
			reprs[i] := Reps[m];
		else
			reprs[i] := Reps[ActualNumber2 mod m];
		end if;
		
		if ActualNumber1 mod m eq 0 then
			stage[i] := ActualNumber1 div m - 1;
		else
			stage[i] := ActualNumber1 div m;
		end if;
		
		if stageA eq stage[i] and stageA eq 0 then
			for g in stabA do
				if inGamma(R, N, type, gamma_rep * g * reprs[i]^(-1)) then
					alpha := reprs[i];
					return gamma * gamma_rep * g * alpha^(-1), alpha, standard;
				end if;
			end for;
		elif stageA eq stage[i] and stageA ne 0 then
			for g in stabA do
				if inGamma(R, N, type, gamma_rep * g * reprs[i]^(-1)) then
					alpha := reprs[i];
					return gamma * gamma_rep * g * alpha^(-1), alpha, standard;
				end if;
			end for;
		elif stageA ge n then
			if stage[i] eq n - 1 then
				for g in stabA do
					if inGamma(R, N, type, gamma_rep * g * reprs[i]^(-1)) then
						alpha := reprs[i];
						return gamma * gamma_rep * g * alpha^(-1), alpha, standard;
					end if;
				end for;
			end if;
		end if;
	end for;
end function;

/*
 * The following function computes the stabilizer of an edge of stage n
 * represented by the matrix M.
 */

function stab(R, N, type, M, n)
	list := [];
	
	if type eq 0 then
		if n lt Degree(N) then
			Include(~list, Matrix(R, 2, 2, [1, 0, 0, 1]));
			return list;
		else
			polset := polynomials(R, n - Degree(N));
			
			for f in polset do
				Include(~list, M * Matrix(R, 2, 2, [1, f * N, 0, 1]) * M^(-1));
			end for;
		end if;
	elif type eq 1 then
		A := M^(-1);
		D := N/Gcd(N, A[2][1]);
		
		if n lt Degree(D) then
			Include(~list, Matrix(R, 2, 2, [1, 0, 0, 1]));
			return list;
		else
			polset := polynomials(R, n - Degree(D));
			
			for f in polset do
				Include(~list, M * Matrix(R, 2, 2, [1, f * D, 0, 1]) * M^(-1));
			end for;
		end if;
	end if;
	
	return list;
end function;

/*
 * If dual = true, we consider the vector space Sym^(-r)(K^2) x det^(twist).
 * If dual = false, we consider Sym^(r)(K^2) x det^(twist) if irr eq false or
 * its irreducible subrepresentation L(r + twist, twist) = L(r) x det^(twist)
 * if irr eq true.
 */

function vectorspace(R, dual, irr, r, twist)
	K := FieldOfFractions(R);
	Pol<X, Y> := PolynomialRing(K, 2);
	
	n := #MonomialsOfDegree(Pol, r);
	Vec := VectorSpace(K, n);
	
	if dual eq false and irr eq true then
		list := [];
		p := Characteristic(K);
		
		if r le p - 1 then
			return Vec;
		else
			for j := 0 to r do
				if not IsDivisibleBy(Binomial(r, j), p) then
					Append(~list, BasisElement(Vec, j + 1));
				end if;
			end for;
		end if;
		
		Vec := sub< Vec | list >;
	end if;
	
	return Vec;
end function;

/*
 * The operation on the vector space of the previous function
 */

function operation(R, dual, irr, r, twist, A, v)
	K := FieldOfFractions(R);
	a := K ! A[1][1];
	b := K ! A[1][2];
	c := K ! A[2][1];
	d := K ! A[2][2];
	
	Vec := vectorspace(K, dual, irr, r, twist);
	n := Degree(Vec);
	
	Pol<X, Y> := PolynomialRing(K, 2);
	Monomials := MonomialsOfDegree(Pol, r);
	list := [];
	
	if dual eq true then
		emb := hom< Pol -> Pol | a * X + b * Y, c * X + d * Y>;
		
		for j := 0 to r do
			Include(~list, emb(X^j * Y^(r-j)));
		end for;
		
		Graph := [];
		
		for i := 1 to n do
			Vector := &+[MonomialCoefficient(list[j], Monomials[(n+1)-i]) * Vec.j : j in [1..n]];
			Include(~Graph, <Vec.i, Vector>);
		end for;
		
		op := hom < Vec -> Vec | Graph >;
		
		if twist ge 0 then
			return Determinant(A)^(twist) * op(v);
		elif twist lt 0 then
			return (1/(Determinant(A)^(Abs(twist)))) * op(v);
		end if;
	elif dual eq false then
		emb := hom < Pol -> Pol | (1 / Determinant(A)) * (d * X - b * Y), (1 / Determinant(A)) * ((-1) * c * X + a * Y)>;
		
		Vec2 := VectorSpace(K, r+1);
		
		polynomial := &+[Coordinates(Vec2, Vec2!v)[i] * Monomials[(n+1)-i] : i in [1..n]];
		polynomial := emb(polynomial);
		
		vector := &+[MonomialCoefficient(polynomial, Monomials[(n+1)-i]) * Vec2.i : i in [1..n]];
		
		if twist ge 0 then
			return Determinant(A)^(twist) * vector;
		elif twist lt 0 then
			return (1/(Determinant(A)^(Abs(twist)))) * vector;
		end if;
	end if;
end function;

/*
 * The following function computes the dimension of the vector space of the
 * Gamma-equivariant harmonic cocycles, in order to know how many coefficients
 * will be needed by the function "cocycle" following it.
 */

function preparation(R, N, type, dual, irr, r, twist, Graph, Reps)
	F := BaseRing(R);
	V := VertexSet(Graph);
	E := EdgeSet(Graph);
	EdgeCount := #E;
	
	m := #Reps;
	q := #F;
	
	BasisEdges := [];
	counter := 0;
	
	for i := 1 to EdgeCount do
		ActualNumber1 := StringToInteger(Label(InitialVertex(E.i)));
		
		if ActualNumber1 le m then
			ActualNumber2 := StringToInteger(Label(E.i));
		else
			ActualNumber2 := ActualNumber1;
		end if;
		
		if ActualNumber2 mod m eq 0 then
			rep := Reps[m];
		else
			rep := Reps[ActualNumber2 mod m];
		end if;
		
		if ActualNumber1 le m then
			v := InitialVertex(E.i);
			w := TerminalVertex(E.i);
			
			if Degree(v) eq q + 1 then
				if counter lt q then
					Append(~BasisEdges, E.i);
					counter := counter + 1;
				elif counter eq q then
					counter := 0;
				end if;
			else
				if Degree(w) eq q + 1 then
					Append(~BasisEdges, E.i);
				else
					if #stab(R, N, type, rep, 0) eq 1 then
						Append(~BasisEdges, E.i);
					end if;
				end if;
			end if;
		else
			break i;
		end if;
	end for;
	
	Vec := vectorspace(R, dual, irr, r, twist);
	
	return BasisEdges, Vec, #BasisEdges * Dimension(Vec);
end function;

/*
 * For two adjacent vertices of an edge of stage 0, the following function
 * computes the common representative.
 */

function auxiliary(R, N, type, Graph, Reps, v_sigma, w_sigma, standard)
	stab0 := stabilizer(R, 0);
	stab1 := stabilizer(R, 1);
	
	for x in stab0 do
		for y in stab1 do
			if v_sigma * x eq w_sigma * y then
				sigma := v_sigma * x;
				break x;
			end if;
		end for;
	end for;
	
	gamma, gamma_rep, standard := quotient_rep(R, N, type, sigma, standard);
	gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
	
	return gamma, gamma_rep, standard;
end function;

/*
 * Finally, the following function calculates the value of a Gamma-equivariant
 * harmonic cocycle c on an edge of the Bruhat-Tits tree, which is represented
 * by the matrix A.
 */

function cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, v, w)
	F := BaseRing(R);
	K := FieldOfFractions(R);
	T := Name(K, 1);
	
	v_norm := normal_form(R, v);
	v_sigma, v_standard := half_line(R, v_norm);
	k_v := valuation(v_standard[2][2]);
	
	w_norm := normal_form(R, w);
	w_sigma, w_standard := half_line(R, w_norm);
	k_w := valuation(w_standard[2][2]);
	
	Vec := vectorspace(R, dual, irr, r, twist);
	dim := Dimension(Vec);
	n := Degree(N);
	
	value := Vec ! 0;
	
	if Minimum(k_v, k_w) gt n + dim then
		return value;
	elif k_v eq 0 and k_w eq 1 then
		gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, v_sigma, w_sigma, v_standard);
		
		if #stab(R, N, type, gamma_rep, 0) eq 1 then
			BasisEdges, Vec, dimension := preparation(R, N, type, dual, irr, r, twist, Graph, Reps);
			BasEdCount := #BasisEdges;
			
			index := 0;
			counter := 0;
			
			for i := 1 to BasEdCount do
				if gamma_rep eq Reps[StringToInteger(Label(BasisEdges[i]))] then
					index := i;
					counter := 1;
					break i;
				end if;
			end for;
			
			if counter eq 1 then
				counter := 0;
				
				for i := 1 to dim do
					value := value + coeff[(index - 1) * dim + i] * BasisElement(Vec, i);
				end for;
				
				value := operation(R, dual, irr, r, twist, gamma, value);
				return value;
			else
				temp := series(R, v_norm[1][2], valuation(v_norm[1][1]) - 1);
				B_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) - 1), temp, 0, 1]);
				B_sigma, B_standard := half_line(R, B_norm);
				k_B := valuation(B_standard[2][2]);
				
				B_gamma, B_gamma_rep, B_standard := auxiliary(R, N, type, Graph, Reps, v_sigma, B_sigma, v_standard);
				
				if gamma_rep ne B_gamma_rep then
					value := value - cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, v_norm, B_norm);
				end if;
				
				for z in F do
					A_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) + 1), v_norm[1][2] + z * (1/T)^(valuation(v_norm[1][1])), 0, 1]);
					A_sigma, A_standard := half_line(R, A_norm);
					k_A := valuation(A_standard[2][2]);
					
					A_gamma, A_gamma_rep, A_standard := auxiliary(R, N, type, Graph, Reps, v_sigma, A_sigma, v_standard);
					
					if gamma_rep ne A_gamma_rep then
						value := value - cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, v_norm, A_norm);
					end if;
				end for;
			end if;
		else
			temp := series(R, v_norm[1][2], valuation(v_norm[1][1]) - 1);
			B_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) - 1), temp, 0, 1]);
			B_sigma, B_standard := half_line(R, B_norm);
			k_B := valuation(B_standard[2][2]);
			
			B_gamma, B_gamma_rep, B_standard := auxiliary(R, N, type, Graph, Reps, v_sigma, B_sigma, v_standard);
			
			if #stab(R, N, type, B_gamma_rep, 0) eq 1 then
				temp2 := cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, v_norm, B_norm);
				
				sta := stab(R, N, type, gamma * gamma_rep, 0);
				
				for g in sta do
					value := value - operation(R, dual, irr, r, twist, g, temp2);
				end for;
			else
				for z in F do
					A_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) + 1), v_norm[1][2] + z * (1/T)^(valuation(v_norm[1][1])), 0, 1]);
					A_sigma, A_standard := half_line(R, A_norm);
					k_A := valuation(A_standard[2][2]);
					
					A_gamma, A_gamma_rep, A_standard := auxiliary(R, N, type, Graph, Reps, v_sigma, A_sigma, v_standard);
					
					if #stab(R, N, type, A_gamma_rep, 0) eq 1 then
						temp2 := cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, v_norm, A_norm);
						
						sta := stab(R, N, type, gamma * gamma_rep, 0);
						
						for g in sta do
							value := value - operation(R, dual, irr, r, twist, g, temp2);
						end for;
						
						break z;
					end if;
				end for;
			end if;
		end if;
	elif k_v eq 1 and k_w eq 0 then
		value := -cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, w, v);
	elif k_v ge 1 and k_w eq k_v + 1 then
		gamma, gamma_rep, standard := quotient_rep(R, N, type, v_sigma, v_standard);
		gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
		
		if #stab(R, N, type, gamma_rep, k_v) eq 1 then
			temp := series(R, v_norm[1][2], valuation(v_norm[1][1]) - 1);
			B_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) - 1), temp, 0, 1]);
			B_sigma, B_standard := half_line(R, B_norm);
			k_B := valuation(B_standard[2][2]);
			
			if k_B eq k_v - 1 then
				value := value + cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, B_norm, v_norm);
			end if;
			
			for z in F do
				A_norm := Matrix(K, 2, 2, [(1/T)^(valuation(v_norm[1][1]) + 1), v_norm[1][2] + z * (1/T)^(valuation(v_norm[1][1])), 0, 1]);
				A_sigma, A_standard := half_line(R, A_norm);
				k_A := valuation(A_standard[2][2]);
				
				if k_A eq k_v - 1 then
					value := value + cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, A_norm, v_norm);
				end if;
			end for;
		else
			stable := false;
			norm := v_norm;
			sigma := v_sigma;
			stage := k_v;
			standard := v_standard;
			
			while not stable do
				temp := series(R, norm[1][2], valuation(norm[1][1]) - 1);
				B_norm := Matrix(K, 2, 2, [(1/T)^(valuation(norm[1][1]) - 1), temp, 0, 1]);
				B_sigma, B_standard := half_line(R, B_norm);
				k_B := valuation(B_standard[2][2]);
				
				if stage ne 0 and k_B eq stage - 1 then
					if k_B eq 0 then
						gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, B_sigma, sigma, B_standard);
					else
						gamma, gamma_rep, standard := quotient_rep(R, N, type, B_sigma, B_standard);
						gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
					end if;
						
					if #stab(R, N, type, gamma_rep, k_B) eq 1 then
						stable := true;
						i_norm := B_norm;
						i_sigma := B_sigma;
						i_standard := B_standard;
						t_norm := norm;
						t_sigma := sigma;
						
						break;
					else
						stable := false;
						norm := B_norm;
						sigma := B_sigma;
						stage := k_B;
						standard := B_standard;
						
						continue;
					end if;
				elif stage ne 0 and k_B eq stage + 1 then
					z := Random(F);
					A_norm := Matrix(K, 2, 2, [(1/T)^(valuation(norm[1][1]) + 1), norm[1][2] + z * (1/T)^(valuation(norm[1][1])), 0, 1]);
					A_sigma, A_standard := half_line(R, A_norm);
					k_A := valuation(A_standard[2][2]);
					
					if k_A eq 0 then
						gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, A_sigma, sigma, A_standard);
					else
						gamma, gamma_rep, standard := quotient_rep(R, N, type, A_sigma, A_standard);
						gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
					end if;
						
					if #stab(R, N, type, gamma_rep, k_A) eq 1 then
						stable := true;
						i_norm := A_norm;
						i_sigma := A_sigma;
						i_standard := A_standard;
						t_norm := norm;
						t_sigma := sigma;
						
						break;
					else
						stable := false;
						norm := A_norm;
						sigma := A_sigma;
						stage := k_A;
						standard := A_standard;
						
						continue;
					end if;
				elif stage eq 0 then
					gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, sigma, B_sigma, standard);
					
					if #stab(R, N, type, gamma_rep, 0) eq 1 then
						stable := true;
						i_norm := norm;
						i_sigma := sigma;
						i_standard := standard;
						t_norm := B_norm;
						t_sigma := B_sigma;
						
						break;
					else
						z := Random(F);
						A_norm := Matrix(K, 2, 2, [(1/T)^(valuation(norm[1][1]) + 1), norm[1][2] + z * (1/T)^(valuation(norm[1][1])), 0, 1]);
						A_sigma, A_standard := half_line(R, A_norm);
						k_A := valuation(A_standard[2][2]);
						
						stable := true;
						i_norm := norm;
						i_sigma := sigma;
						i_standard := standard;
						t_norm := A_norm;
						t_sigma := A_sigma;
						
						break;
					end if;
				end if;
			end while;
			
			temp2 := cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, i_norm, t_norm);
			
			gamma, gamma_rep, standard := quotient_rep(R, N, type, v_sigma, v_standard);
			gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
			
			sta := stab(R, N, type, gamma * gamma_rep, k_v);
			
			for g in sta do
				value := value + operation(R, dual, irr, r, twist, g, temp2);
			end for;
		end if;
	elif k_w ge 1 and k_v eq k_w + 1 then
		value := -cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, w, v);
	else
		Sprintf("The vertices are not adjacent.");
	end if;
	
	return value;
end function;

/*
 * Part 3: The Vector Space of Harmonic Cocycles for Gamma_0^1, Gamma_0, or
 * SL_2(R)
 */

/*
 * The following program computes the vector space of Gamma-equivariant
 * harmonic cocycles, where Gamma is either Gamma_0^1, Gamma_0, or SL_2(R).
 *
 * Keep in mind, this program is dependent on the previous parts. For further
 * information on the exact approach, please consider Section 3.5 of my master
 * thesis, which I will be refering to quite often in the following lines.
 */

/*
 * First, we implement a system of representatives for Gamma_0^1 / Gamma_1. 
 */

function gamma01gamma1(R, N)
	F := BaseRing(R);
	list := [];
	Q := quo<R|N>;
	
	list2 := [];
	
	for x in Q do
		if IsInvertible(x) then
			Append(~list2, Matrix(Q, 2, 2, [x, 0, 0, x^(-1)]));
		end if;
	end for;
	
	for M in list2 do
		a := R ! M[1][1];
		b := R ! M[1][2];
		c := R ! M[2][1];
		d := R ! M[2][2];
		
		if a * d - b * c eq 1 then
			Append(~list, Matrix(R, 2, 2, [a, b, c, d]));
		else
			r := (a * d - b * c - 1) div N;
			s := lift(d, c);
			c := c + s * N;
			
			v, f, g := Xgcd(c, d);
			a_new := a - (r - b * s) * g * N;
			b_new := b + (r - b * s) * f * N;
			
			Append(~list, Matrix(R, 2, 2, [a_new, b_new, c, d]));
		end if;
	end for;
	
	return list;
end function;

/*
 * Next, we implement a system of representatives for Gamma_0 / Gamma_1.
 */

function gamma0gamma1(R, N)
	F := BaseRing(R);
	list := [];
	Q := quo<R|N>;
	
	list2 := [];
	
	for x in Q do
		if IsInvertible(x) then
			Append(~list2, Matrix(Q, 2, 2, [x, 0, 0, x^(-1)]));
		end if;
	end for;
	
	for M in list2 do
		a := R ! M[1][1];
		b := R ! M[1][2];
		c := R ! M[2][1];
		d := R ! M[2][2];
		
		if a * d - b * c eq 1 then
			for x in F do
				if x ne 0 then
					A := Matrix(R, 2, 2, [x, 0, 0, 1]);
					B := Matrix(R, 2, 2, [a, b, c, d]);
					Append(~list, A * B);
				end if;
			end for;
		else
			r := (a * d - b * c - 1) div N;
			s := lift(d, c);
			c := c + s * N;
			
			v, f, g := Xgcd(c, d);
			a_new := a - (r - b * s) * g * N;
			b_new := b + (r - b * s) * f * N;
			
			for x in F do
				if x ne 0 then
					A := Matrix(R, 2, 2, [x, 0, 0, 1]);
					B := Matrix(R, 2, 2, [a_new, b_new, c, d]);
					Append(~list, A * B);
				end if;
			end for;
		end if;
	end for;
	
	return list;
end function;

/*
 * A system of representatives for SL_2(R) / Gamma.
 */

function slgamma(R, N)
	list := [];
	Q := quo<R|N>;
	sl := [];
	
	for a, b, c, d in Q do
		if a * d - b * c eq 1 then
			Append(~sl, Matrix(Q, 2, 2, [a, b, c, d]));
		end if;
	end for;
	
	for M in sl do
		a := R ! M[1][1];
		b := R ! M[1][2];
		c := R ! M[2][1];
		d := R ! M[2][2];
		
		if a * d - b * c eq 1 then
			A := Matrix(R, 2, 2, [a, b, c, d]);
			Append(~list, A);
		else
			r := (a * d - b * c - 1) div N;
			
			if c ne 0 then
				t := lift(c, d);
				d := d + t * N;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r + a * t) * g * N;
				b_new := b + (r + a * t) * f * N;
			else
				s := lift(d, c);
				c := c + s * N;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r - b * s) * g * N;
				b_new := b + (r - b * s) * f * N;
			end if;
			
			A := Matrix(R, 2, 2, [a_new, b_new, c, d]);
			Append(~list, A);
		end if;
	end for;
	
	return list;
end function;

/*
 * A system of representatives for SL_2(R) / Gamma (only generators).
 */

function slgammagen(R, N)
	list := [];
	Q := quo<R|N>;
	sl := [];
	
	for x in Q do
		y := R ! x;
		
		A := Matrix(R, 2, 2, [1, y, 0, 1]);
		Append(~list, A);
		B := Matrix(R, 2, 2, [1, 0, y, 1]);
		Append(~list, B);
	end for;
	
	return list;
end function;

/*
 * Finally, the function "space" computes the vector space of Gamma_0^1-,
 * Gamma_0-, or SL_2(R)-equivariant harmonic cocycles as a subspace of the
 * vector space of Gamma_1-equivariant harmonic cocycles.
 */

function space(R, N, dual, irr, r, twist, type, Graph, Reps)
	K := FieldOfFractions(R);
	T := Name(K, 1);
	
	if type eq 2 then
		SOR := gamma01gamma1(R, N);
	elif type eq 3 then
		SOR := gamma0gamma1(R, N);
	elif type eq 4 then
		SOR := slgammagen(R, N);
	end if;
	
	if type eq 2 or type eq 3 then
		BasisEdges, Vec, dimension := preparation(R, N, 1, dual, irr, r, twist, Graph, Reps);
	elif type eq 4 then
		BasisEdges, Vec, dimension := preparation(R, N, 0, dual, irr, r, twist, Graph, Reps);
	end if;
	
	m := #BasisEdges;
	n := Dimension(Vec);
	
	H_2 := ZeroMatrix(K, #SOR * dimension, dimension);
	EHM := ZeroMatrix(K, #SOR * dimension, dimension);
	counter := 0;
	
	for g in SOR do
		H_1 := ZeroMatrix(K, dimension, dimension);
		
		for i := 1 to dimension do
			coeff := [0 : z in [1..dimension]];
			coeff[i] := 1;
			
			for j := 1 to m do
				vertex := Reps[StringToInteger(Label(BasisEdges[j]))];
				if type eq 2 or type eq 3 then
					value := cocycle(R, N, 1, Graph, Reps, dual, irr, r, twist, coeff, g * vertex, g * vertex * Matrix(K, 2, 2, [1, 0, 0, 1/T]));
				elif type eq 4 then
					value := cocycle(R, N, 0, Graph, Reps, dual, irr, r, twist, coeff, g * vertex, g * vertex * Matrix(K, 2, 2, [1, 0, 0, 1/T]));
				end if;
				
				value := operation(R, dual, irr, r, twist, g^(-1), value);
				
				list := Coordinates(Vec, value);
				
				for k := 1 to n do
					H_1[i][(j - 1) * n + k] := list[k];
				end for;
			end for;
		end for;
		
		H_1 := Transpose(H_1);
		
		for i := 1 to dimension do
			for j := 1 to dimension do
				H_2[counter * dimension + i][j] := H_1[i][j];
				
				if i eq j then
					EHM[counter * dimension + i][j] := 1;
				else
					EHM[counter * dimension + i][j] := 0;
				end if;
			end for;
		end for;
		
		counter := counter + 1;
	end for;
	
	return KernelMatrix(Transpose(H_2 - EHM));
end function;

/*
 * Part 4: The Hecke Operator
 */

/*
 * The following function returns an element z in R such that z = x mod N,
 * z = y mod P.
 */

function lift2(N, P, x, y)
	a, b, c := Xgcd(P, N);
	
	return x * b * P + y * c * N;
end function;

/*
 * First, compute a system of representatives of
 * (Gamma cap Gamma_0(P)) \ Gamma where Gamma is one of the four congruence
 * subgroups Gamma(N), Gamma_1(N), Gamma_0^1(N), Gamma_0(N), or SL_2(R).
 */

function gammaNP(R, N, P)
	list := [];
	list2 := [];
	
	for x in gamma0system(R, P) do
		a := lift2(N, P, 1, x[1][1]);
		b := lift2(N, P, 0, x[1][2]);
		c := lift2(N, P, 0, x[2][1]);
		d := lift2(N, P, 1, x[2][2]);
		
		a := a mod (N * P);
		b := b mod (N * P);
		c := c mod (N * P);
		d := d mod (N * P);
		
		Append(~list, Matrix(R, 2, 2, [a, b, c, d]));
	end for;
	
	for x in list do
		a := x[1][1];
		b := x[1][2];
		c := x[2][1];
		d := x[2][2];
		
		if a * d - b * c eq 1 then
			Append(~list2, x);
		else
			r := (a * d - b * c - 1) div (N * P);
			
			if c ne 0 then
				t := lift(c, d);
				d := d + t * N * P;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r + a * t) * g * N * P;
				b_new := b + (r + a * t) * f * N * P;
			else
				s := lift(d, c);
				c := c + s * N * P;
				
				v, f, g := Xgcd(c, d);
				a_new := a - (r - b * s) * g * N * P;
				b_new := b + (r - b * s) * f * N * P;
			end if;
			
			Append(~list2, Matrix(R, 2, 2, [a_new, b_new, c, d]));
		end if;
	end for;
	
	return list2;
end function;

/*
 * Finally, the next function computes a Hecke operator T_P (and a list of
 * graphs as second output).
 *
 * The argument "type" sets the congruence subgroup, 0 meaning Gamma(N), 1
 * for Gamma_1(N), 2 for Gamma_0^1(N), 3 for Gamma_0(N), and 4 for SL_2(R).
 *
 * If dual = true, we consider the vector space Sym^(-r)(K^2) x det^(twist).
 * If dual = false, we consider Sym^(r)(K^2) x det^(twist) if irr eq false or
 * its irreducible subrepresentation L(r + twist, twist) = L(r) x det^(twist)
 * if irr eq true.
 *
 * graph_list is initially []. If the quotient graph is not in graph_list, the
 * function will compute the quotient graph, include it in graph_list, and
 * put it out. This output can be used for the next computation. If a graph
 * is in graph_list, then the function will use it without further
 * computations.
 */

function operator(R, type, N, P, dual, irr, r, twist, graph_list)
	K := FieldOfFractions(R);
	T := Name(K, 1);
	
	if Gcd(N, P) ne 1 then
		return "N and P should be coprime.";
	elif not IsIrreducible(P) then
		return "P should be irreducible.";
	end if;
	
	inList := 0;
	
	if type eq 0 or type eq 1 then
		if IsEmpty(graph_list) then
			Graph, Reps, String := graph(R, N, type);
			Include(~graph_list, <R, N, type, Graph, Reps, String>);
		else
			for x in graph_list do
				if <x[1], x[2], x[3]> eq <R, N, type> then
					Graph := x[4];
					Reps := x[5];
					String := x[6];
					inList := 1;
					break x;
				end if;
			end for;
			
			if inList eq 0 then
				Graph, Reps, String := graph(R, N, type);
				Include(~graph_list, <R, N, type, Graph, Reps, String>);
			end if;
		end if;
		
		BasisEdges, Vec, dimension := preparation(R, N, type, dual, irr, r, twist, Graph, Reps);
		dimension2 := dimension;
	elif type eq 2 or type eq 3 then
		if IsEmpty(graph_list) then
			Graph, Reps, String := graph(R, N, 1);
			Include(~graph_list, <R, N, 1, Graph, Reps, String>);
		else
			for x in graph_list do
				if <x[1], x[2], x[3]> eq <R, N, 1> then
					Graph := x[4];
					Reps := x[5];
					String := x[6];
					inList := 1;
					break x;
				end if;
			end for;
			
			if inList eq 0 then
				Graph, Reps, String := graph(R, N, 1);
				Include(~graph_list, <R, N, 1, Graph, Reps, String>);
			end if;
		end if;
		
		BasisEdges, Vec, dimension := preparation(R, N, 1, dual, irr, r, twist, Graph, Reps);
		A := space(R, N, dual, irr, r, twist, type, Graph, Reps);
		dimension2 := NumberOfRows(A);
	elif type eq 4 then
		if IsEmpty(graph_list) then
			Graph, Reps, String := graph(R, N, 0);
			Include(~graph_list, <R, N, 0, Graph, Reps, String>);
		else
			for x in graph_list do
				if <x[1], x[2], x[3]> eq <R, N, 0> then
					Graph := x[4];
					Reps := x[5];
					String := x[6];
					inList := 1;
					break x;
				end if;
			end for;
			
			if inList eq 0 then
				Graph, Reps, String := graph(R, N, 0);
				Include(~graph_list, <R, N, 0, Graph, Reps, String>);
			end if;
		end if;
		
		BasisEdges, Vec, dimension := preparation(R, N, 0, dual, irr, r, twist, Graph, Reps);
		A := space(R, N, dual, irr, r, twist, type, Graph, Reps);
		dimension2 := NumberOfRows(A);
	end if;
	
	BasisReps := [];
	EdgeCount := #BasisEdges;
	
	for i := 1 to EdgeCount do
		BasisReps[i] := Reps[StringToInteger(Label(BasisEdges[i]))];
	end for;
	
	SOR := gammaNP(R, N, P);
	
	T_P := ZeroMatrix(K, dimension2, dimension);
	
	for i := 1 to dimension2 do
		if type eq 0 or type eq 1 then
			coeff := [K ! 0 : z in [1..dimension]];
			coeff[i] := K ! 1;
		else
			coeff := A[i];
		end if;
		
		seq := [];
		val := [];
	
		for j := 1 to EdgeCount do
			value := Vec ! 0;
			
			for g in SOR do
				v := Matrix(K, 2, 2, [P, 0, 0, 1]) * g * BasisReps[j];
				w := Matrix(K, 2, 2, [P, 0, 0, 1]) * g * BasisReps[j] * Matrix(K, 2, 2, [1, 0, 0, 1/T]);
				
				v_norm := normal_form(R, v);
				sigma_1, standard_1 := half_line(R, v_norm);
				w_norm := normal_form(R, w);
				sigma_2, standard_2 := half_line(R, w_norm);
				
				k_1 := valuation(standard_1[2][2]);
				k_2 := valuation(standard_2[2][2]);
				
				if k_2 eq k_1 + 1 and k_2 ge 2 then
					sigma := sigma_1;
					if type eq 0 or type eq 1 then
						gamma, gamma_rep, standard := quotient_rep(R, N, type, sigma, standard_1);
						gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
					elif type eq 2 or type eq 3 then
						gamma, gamma_rep, standard := quotient_rep(R, N, 1, sigma, standard_1);
						gamma, gamma_rep, standard := final_rep(R, N, 1, Graph, Reps, gamma, gamma_rep, standard);
					elif type eq 4 then
						gamma, gamma_rep, standard := quotient_rep(R, N, 0, sigma, standard_1);
						gamma, gamma_rep, standard := final_rep(R, N, 0, Graph, Reps, gamma, gamma_rep, standard);
					end if;
					sign := 1;
				elif k_1 eq k_2 + 1 and k_1 ge 2 then
					sigma := sigma_2;
					if type eq 0 or type eq 1 then
						gamma, gamma_rep, standard := quotient_rep(R, N, type, sigma, standard_2);
						gamma, gamma_rep, standard := final_rep(R, N, type, Graph, Reps, gamma, gamma_rep, standard);
					elif type eq 2 or type eq 3 then
						gamma, gamma_rep, standard := quotient_rep(R, N, 1, sigma, standard_2);
						gamma, gamma_rep, standard := final_rep(R, N, 1, Graph, Reps, gamma, gamma_rep, standard);
					elif type eq 4 then
						gamma, gamma_rep, standard := quotient_rep(R, N, 0, sigma, standard_2);
						gamma, gamma_rep, standard := final_rep(R, N, 0, Graph, Reps, gamma, gamma_rep, standard);
					end if;
					sign := -1;
				elif k_2 eq k_1 + 1 and k_2 eq 1 then
					if type eq 0 or type eq 1 then
						gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, sigma_1, sigma_2, standard_1);
					elif type eq 2 or type eq 3 then
						gamma, gamma_rep, standard := auxiliary(R, N, 1, Graph, Reps, sigma_1, sigma_2, standard_1);
					elif type eq 4 then
						gamma, gamma_rep, standard := auxiliary(R, N, 0, Graph, Reps, sigma_1, sigma_2, standard_1);
					end if;
					sign := 1;
				elif k_1 eq k_2 + 1 and k_1 eq 1 then
					if type eq 0 or type eq 1 then
						gamma, gamma_rep, standard := auxiliary(R, N, type, Graph, Reps, sigma_2, sigma_1, standard_2);
					elif type eq 2 or type eq 3 then
						gamma, gamma_rep, standard := auxiliary(R, N, 1, Graph, Reps, sigma_2, sigma_1, standard_2);
					elif type eq 4 then
						gamma, gamma_rep, standard := auxiliary(R, N, 0, Graph, Reps, sigma_2, sigma_1, standard_2);
					end if;
					sign := -1;
				end if;
				
				counter := 0;
				
				count := #seq;
				
				for k := 1 to count do
					if gamma_rep * standard eq seq[k] then
						add := sign * val[k];
						counter := 1;
						break k;
					end if;
				end for;
				
				if counter eq 0 then
					Append(~seq, gamma_rep * standard);
					vec := gamma_rep * standard;
					if type eq 0 or type eq 1 then
						add := cocycle(R, N, type, Graph, Reps, dual, irr, r, twist, coeff, vec, vec * Matrix(K, 2, 2, [1, 0, 0, 1/T]));
					elif type eq 2 or type eq 3 then
						add := cocycle(R, N, 1, Graph, Reps, dual, irr, r, twist, coeff, vec, vec * Matrix(K, 2, 2, [1, 0, 0, 1/T]));
					elif type eq 4 then
						add := cocycle(R, N, 0, Graph, Reps, dual, irr, r, twist, coeff, vec, vec * Matrix(K, 2, 2, [1, 0, 0, 1/T]));
					end if;
					Append(~val, add);
					add := sign * add;
				else
					counter := 0;
				end if;
				
				delta := g^(-1) * Matrix(K, 2, 2, [P, 0, 0, 1])^(-1) * gamma;
				add := operation(R, dual, irr, r, twist, delta, add);
				
				value := value + add;
			end for;
			
			list := Coordinates(Vec, value);
			
			for k := 1 to Dimension(Vec) do
				T_P[i][k + (j - 1) * Dimension(Vec)] := list[k];
			end for;
		end for;
	end for;
	
	if type eq 2 or type eq 3 or type eq 4 then
		T_P_pre := ZeroMatrix(K, dimension2, dimension2);
		
		for i := 1 to dimension2 do
			V := T_P[i];
			B := Solution(A, V);
			T_P_pre[i] := B;
		end for;
		
		T_P := T_P_pre;
	end if;
	
	return T_P, graph_list;
end function;

/*
Examples in Section 4.4 (before Section 4.4.1)

F := GF(3); R<T> := PolynomialRing(F); N := T; P := T + 1; dual := true; irr := true;
P := T + 1; Q := T^2 + 1; r := 3; twist := -r; graph_list := [];
- for Gamma_1: type := 1;
- for Gamma_0: type := 3;

F := GF(3); R<T> := PolynomialRing(F); N := T; P := T + 1; dual := true; irr := true;
P := T + 1; Q := T^2 + 1; r := 4; twist := -4; graph_list := [];
- for Gamma_1: type := 1;
- for Gamma_0: type := 3;

F := GF(3); R<T> := PolynomialRing(F); N := T; type := 1; P := T + 1; graph_list := [];
- for L(4, 1): dual := false; irr := true; r := 3; twist := 1;
- for Sym^3 x det: dual := false; irr := false; r := 3; twist := 1;
- for Sym^5: dual := false; irr := true; r := 5; twist := 0;

A, graph_list := operator(R, type, N, P, dual, irr, r, twist, graph_list);
*/