:- object(relations).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist',
		date is 2014/03/03,
		comment is 'Predicates working on relations.'
	]).

   :- public(relation/2).
   :- public(pol/2).
   :- public(reduce/3).
   :- public(weak_base/2).
   :- public(is_minimal/2).
   :- public(smallest_co_clone/2).

   relation(and/2, [[1,1]]).
   relation(xor/2, [[0,1],[1,0]]).
   relation(imp/2, [[0,0], [0,1], [1,1]]).
   relation(t/1, [[1]]).
   relation(f/1, [[0]]).
   relation(dup/3, [[0,0,0],[0,0,1],[0,1,1], [1,0,0],
                    [1,1,0],[1,1,1]]).
   relation(odd/N, Rs) :-
	   setof(T,
	         S^(length(T, N),
			    matrix::tuple(T),
				sum(T, S),
				\+ even(S)),
			 Rs).
   relation(even/N, Rs) :-
	   setof(T,
	         S^(length(T, N),
			    matrix::tuple(T),
				sum(T, S),
				even(S)),
			 Rs).
   relation(or/N, Rs) :-
	   setof(T,
	         S^(length(T, N),
			    matrix::tuple(T),
				sum(T, S),
				S \= 0),
			 Rs).
   relation(nand/N, Rs) :-
	   setof(T,
	         S^(length(T, N),
			    matrix::tuple(T),
				sum(T, S),
				S \= N),
			 Rs).
   
   %Finite bases for all co-clones for which it's possible. For
   %simplicity each base is named after the corresponding co-clone.
   relation(br/3, [[0,0,1],[0,1,0],[1,0,0]]).
   relation(ii/6, R) :-
       relation(even/4, Odd),
	   relation(imp/2, Imp),
       cartesian_product(Odd, Imp, R).
   relation(ii1/7, R) :-
       relation(even/4, R0),
	   relation(imp/2, R1),
	   relation(t/1, R2),
	   cartesian_product(R0, R1, R0R1),
	   cartesian_product(R0R1, R2, R).
   relation(ii0/7, R) :-
       relation(even/4, R0),
	   relation(imp/2, R1),
	   relation(f/1, R2),
	   cartesian_product(R0, R1, R0R1),
	   cartesian_product(R0R1, R2, R).
   relation(in2/3, [[0,0,1],[0,1,0],[1,0,0],
                    [1,1,0],[1,0,1],[0,1,1]]).
   relation(in/3, R) :-
       relation(dup/3, R).
   relation(ie/3, [[0,0,0],[0,0,1],[0,1,0],
                   [1,0,0],[1,0,1],[0,1,1],
				   [1,1,1]]).
   relation(ie0/4, [[0,0,0,0],[0,0,1,0],[0,1,0,0],
                    [1,0,0,0],[1,0,1,0],[0,1,1,0],
					[1,1,1,0]]).
   relation(ie1/4, [[0,0,0,1],[0,0,1,1],[0,1,0,1],
                    [1,0,0,1],[1,0,1,1],[0,1,1,1],
					[1,1,1,1]]).
   relation(ie2/5, [[0,0,0,0,1],[0,0,1,0,1],[0,1,0,0,1],
                    [1,0,0,0,1],[1,0,1,0,1],[0,1,1,0,1],
					[1,1,1,0,1]]).
   relation(iv/3, [[0,0,0],[0,1,0],[1,0,0],
                   [1,0,1],[1,1,0],[0,1,1],
				   [1,1,1]]).
   relation(iv0/4, R) :-
       relation(iv/3, R0),
	   relation(f/1, F),
	   cartesian_product(R0, F, R).
   relation(iv1/4, R) :-
       relation(iv/3, R0),
	   relation(t/1, T),
	   cartesian_product(R0, T, R).
   relation(iv2/5, R) :-
       relation(iv/3, R0),
	   relation(t/1, T),
	   relation(f/1, F),
	   cartesian_product(T, F, TF),
	   cartesian_product(R0, TF, R).
   relation(il/4, R) :-
       relation(even/4, R).
   relation(il0/5, R) :-
       relation(even/4, R0),
	   relation(f/1, F),
	   cartesian_product(R0, F, R).
   relation(il1/5, R) :-
       relation(even/4, R0),
	   relation(t/1, T),
	   cartesian_product(R0, T, R).
   relation(il2/6, R) :-
       relation(even/4, R0),
	   relation(t/1, T),
	   relation(f/1, F),
	   cartesian_product(T, F, TF),
	   cartesian_product(R0, TF, R).
   relation(il3/4, R) :-
       relation(odd/4, R).
   relation(id/2, R) :-
       relation(xor/2, R).
   relation(id1/3, R) :-
       relation(xor/2, R0),
	   relation(t/1, T),
	   cartesian_product(R0, T, R).
   relation(id2/3, [[0,1,1],[1,0,0],[1,0,1]]).  
   relation(im/2, R) :-
       relation(imp/2, R).
   relation(im0/3, R) :-
       relation(imp/2, Imp),
	   relation(f/1, F),
	   cartesian_product(Imp, F, R).
   relation(im1/3, R) :-
       relation(imp/2, Imp),
	   relation(t/1, T),
	   cartesian_product(Imp, T, R).
   relation(im2/4, R) :-
       relation(imp/2, Imp),
	   relation(f/1, F),
	   relation(t/1, T),
	   cartesian_product(Imp, F, ImpF),
	   cartesian_product(ImpF, T, R).
   relation(is00/2, R) :-
       relation_inf(is00/2, R).
   relation(is01/2, R) :-
       relation_inf(is01/2, R).
   relation(is02/2, R) :-
       relation_inf(is02/2, R).
   relation(is0/2, R) :-
       relation_inf(is0/2, R).
   relation(is1/2, R) :-
       relation_inf(is1/2, R).
   relation(is12/2, R) :-
       relation_inf(is12/2, R).
   relation(is11/2, R) :-
       relation_inf(is11/2, R).
   relation(is10/2, R) :-
       relation_inf(is10/2, R).
   relation(ir0/1, R) :-
       relation(f/1, R).
   relation(ir1/1, R) :-
       relation(t/1, R).
   relation(ir2/2, R) :-
	   relation(t/1, T),
	   relation(f/1, F),
	   cartesian_product(T, F, R).
   relation(bf/0, [[0,0],[1,1]]).

   %Relations in the infinite parts of Post's lattice.
   relation_inf(is00/N, R) :-
       N > 1,
       relation(or/N, Or),
	   relation(t/1, T),
	   relation(f/1, F),
	   relation(imp/2, Imp),
	   cartesian_product(Or, T, OrT),
	   cartesian_product(OrT, F, OrTF),
	   cartesian_product(OrTF, Imp, R).
   relation_inf(is01/N, R) :-
       N > 1,
       relation(or/N, Or),
	   relation(imp/2, Imp),
	   cartesian_product(Or, Imp, R).
   relation_inf(is02/N, R) :-
       N > 1,
       relation(or/N, Or),
	   relation(t/1, T),
	   relation(f/1, F),
	   cartesian_product(Or, T, OrT),
	   cartesian_product(OrT, F, R).
   relation_inf(is0/N, R) :-
       N > 1,
       relation(or/N, R).
   relation_inf(is1/N, R) :-
       N > 1,
       relation(nand/N, R).
   relation_inf(is12/N, R) :-
       N > 1,
       relation(nand/N, Nand),
	   relation(t/1, T),
	   relation(f/1, F),
	   cartesian_product(Nand, T, NandT),
	   cartesian_product(NandT, F, R).
   relation_inf(is11/N, R) :-
       N > 1,
       relation(nand/N, Nand),
	   relation(imp/2, Imp),
	   cartesian_product(Nand, Imp, R).
   relation_inf(is10/N, R) :-
       N > 1,
       relation(nand/N, Nand),
	   relation(t/1, T),
	   relation(f/1, F),
	   relation(imp/2, Imp),
	   cartesian_product(Nand, T, NandT),
	   cartesian_product(NandT, F, NandTF),
	   cartesian_product(NandTF, Imp, R).

   %Polymorphisms for the finite parts of Post's lattice.
   pol(br/3, [id/1]).
   pol(ii/6, [id/1, c0/1, c1/1]).
   pol(ii0/7, [id/1, c0/1]).
   pol(ii1/7, [id/1, c1/1]).
   pol(in2/3, [not/1]).
   pol(in/3, [not/1, c0/1, c1/1]).
   pol(id2/3, [id2/3]).
   pol(id1/3, [id1/3]).
   pol(id/2, [id/3]).
   pol(ie/3, [and/2, c0/1, c1/1]).
   pol(ie0/4, [and/2, c0/1]).
   pol(ie1/4, [and/2, c1/1]).
   pol(ie2/5, [and/2]).
   pol(iv/3, [or/2, c0/1,c1/1]).
   pol(iv0/4, [or/2, c0/1]).
   pol(iv1/4, [or/2, c1/1]).
   pol(iv2/5, [or/2]).
   pol(il/4, [xor/2, c1/1]).
   pol(il0/5, [xor/2]).
   pol(il1/5, [eq/2]).
   pol(il2/6, [xor/3]).
   pol(il3/4, [il3/3]).
   pol(is00/2, [s00/3, hn/3]).
   pol(is10/2, [s10/3, dhn/3]).
   pol(is10/3, [s10/3]).
   pol(is11/3, [s10/3, c0/1]).
   pol(is12/3, [s12/3]).
   pol(is00/3, [s00/3]).
   pol(is01/3, [s00/3, c1/1]).
   pol(is02/3, [s02/3]).
   pol(is1/3, [s1/2]).
   pol(is0/3, [s0/2]).
   pol(im/2, [and/2, or/2, c0/1, c1/1]).
   pol(im0/3, [and/2, or/2, c0/1]).
   pol(im1/3, [and/2, or/2, c1/1]).
   pol(im2/4, [and/2, or/2]).   
   pol(ir0/1, [and/2, xor/2]).
   pol(ir1/1, [or/2, r1/2]).
   pol(ir2/2, [or/2, r2/3]).
   pol(bf/0, [and/2, not/1]).

   %Polymorphisms for the infinite parts of Post's lattice.
   pol_inf(is00/N, [s00/3,dhn/(N1)]) :-
        N1 is N + 1.
   pol_inf(is01/N, [c1/1,dhn/(N1)]) :-
       N1 is N + 1.
   pol_inf(is02/N, [s02/3,dhn/(N1)]) :-
       N1 is N + 1.
   pol_inf(is0/N, [imp/2,dhn/(N1)]) :-
       N1 is N + 1.   
   pol_inf(is10/N, [is10/3,hn/(N1)]) :-
       N1 is N + 1.
   pol_inf(is11/N, [c0/1,hn/(N1)]) :-
       N1 is N + 1.
   pol_inf(is12/N, [s12/3, hn/(N1)]) :-
       N1 is N + 1.
   pol_inf(is1/N, [s1/2,hn/(N1)]) :-
       N1 is N + 1.   

   %Some auxiliary predicates when constructing relations.
   even(N) :-
       0 is N mod 2.

   sum([], 0).
   sum([X|Xs], D) :-
       sum(Xs, D0),
	   D is D0 + X.

   cartesian_product(R1, R2, R1R2) :-
       setof(T1T2, T1^T2^(list::member(T1, R1),
	                      list::member(T2, R2),
						  list::append(T1, T2, T1T2)),
			 R1R2).

   size(R, S) :-
       relation(R, Rs),
       list::length(Rs, S).

   arity(_R/N, N).

   %Inclusion predicates.  
   included0(is00/3, iv2/5).
   included0(is01/3, iv1/4).
   included0(is10/3, ie2/5).
   included0(is11/3, ie0/4).

   included(R1, R2) :-
       included0(R1, Ri),
	   included(Ri, R2).

   included(R1, R2) :-
       pol(R1, _),
	   pol(R2, Fs2),
       relation(R1, Ts1),
	   operators::closed_relation(Ts1, Fs2).

   included2(Ts, R) :-
       pol(R, Fs),
	   operators::closed_relation(Ts, Fs).

   %Find the base R of the smallest co-clone which includes Rs, i.e. <Rs> = <R>.
   smallest_co_clone(Rs, R) :-
       included2(Rs, R),
	   %write('Testing inclusion in: '), nl,
	   forall((included(R0, R),R0 \= R),
	          (\+ included2(Rs, R0))).

   %Returns the core-size  of the co-clone C.
   core_size(C, S) :-
       ccols(C, S, R),
	   %write('Testing relation: '), nl,
	   %matrix::write_matrix(R),
	   %nl,
	   smallest_co_clone(R, C).

   %Computes the relation C(COLS^n).
   ccols(C, N, R) :-
       tuples(N, Ts),
	   matrix::transpose(Ts, Ts1),
	   pol(C, Fs),
	   operators::close_relation(Ts1, Fs, R).

   %Tests whether a relation is minimal. In case the relation has been
   %obtained with weak_base/2 then this relation already has no
   %redundant columns or fictitious variables - hence all that needs
   %to be tested is subset minimality.
   is_minimal(C, R) :-
       \+ (subset(R0, R),
	       R0 \= R,
	       write('Testing subset: '), nl, write(R0), nl,
	       matrix::write_matrix(R0), nl, nl,
	       smallest_co_clone(R0, C)).

   %Computes the weak base of the co-clone C.
   weak_base(C, R) :-
       between(1, 100, S),
	   core_size(C, S),
	   write('Found core size '),
	   write(S),
	   nl,
	   ccols(C, S, R0),
	   write(C), write('-cols is: '), nl,
	   matrix::write_matrix(R0),
	   reduce(R0, C, R),
	   write('Reduced relation is: '), nl,
	   matrix::write_matrix(R).

   included_inf(is01/N, is00/N).
   included_inf(is02/N, is00/N).
   included_inf(is0/N, is00/N).
   included_inf(is0/N, is01/N).
   included_inf(is0/N, is02/N).

   included_inf(is00/N, is00/M) :-
       succ(N, M).
   included_inf(is01/N, is01/M) :-
       succ(N, M).
   included_inf(is02/N, is02/M) :-
       succ(N, M).
   included_inf(is0/N, is0/M) :-
       succ(N, M).

   included_inf(is11/N, is10/N).
   included_inf(is12/N, is10/N).
   included_inf(is1/N, is10/N).
   included_inf(is0/N, is11/N).
   included_inf(is0/N, is12/N).

   included_inf(is10/N, is10/M) :-
       succ(N, M).
   included_inf(is11/N, is11/M) :-
       succ(N, M).
   included_inf(is12/N, is12/M) :-
       succ(N, M).
   included_inf(is1/N, is1/M) :-
       succ(N, M).

   included2_inf(Ts, R) :-
       pol_inf(R, Fs),
	   operators::closed_relation(Ts, Fs).

   %R is a reduction of R0.
   reduce(R0, C, R) :-
       reduce(R0, Xi,Xj,R1),
       R1 \= R0,
	   %write('Reduced relation is: '), matrix::write_matrix(R1),nl,
	   smallest_co_clone(R1, C),
	   write('Collapsed rows: '), nl,
	   meta::map([X,Y]>>(write(X), write(' '), write(Y), nl), Xi,Xj), nl,
	   write('New reduced relation is: '), nl,matrix::write_matrix(R1),nl,
	   reduce(R1, C, R).
    reduce(R, _, R).

   reduce_inf(R0, C, R) :-
       reduce(R0, _, _, R1),
       R1 \= R0,
	   %write('Reduced relation is: '), matrix::write_matrix(R1),nl,
	   smallest_co_clone_inf(R1, C),
	   reduce_inf(R1, C, R).
    reduce_inf(R, _, R).

   reduce(R, Xi,Xj,R_red) :-
	   matrix::remove_duplicate_columns(R, R_red0),
	   remove_redundant_variables(R_red0, R_red1),
       matrix::remove_duplicate_rows(R_red1, R_red2),
	   reduce1(R_red2, Xi,Xj, R_red).

   reduce1(R, _,_,R). 
   reduce1(R, Xi,Xj,R_red) :-
	   candidate_variables(R, Xi, Xj),
	   matrix::collapse_rows(R, Xi, Xj, R_red0),
	   reduce(R_red0, Xi,Xj, R_red).

   smallest_co_clone_inf(Rs0, R) :-
       reduce(Rs0, _, _, Rs),
	   !,
	   forall((included_inf(R0, R),R0 \= R),
	          (\+ included2_inf(Rs, R0))).

   ccols_inf(C, N, R) :-
       tuples(N, Ts),
	   matrix::transpose(Ts, Ts1),
	   pol_inf(C, Fs),
	   operators::close_relation(Ts1, Fs, R).

   core_size_inf(C, S) :-
       write('Trying core size '), write(S), write('.'),nl,
       ccols_inf(C, S, R),
	   smallest_co_clone_inf(R, C).

   weak_base_inf(C, R) :-
       between(1, 100, S),
	   core_size_inf(C, S),
	   write('Found core size '),
	   write(S),
	   nl,
	   ccols_inf(C, S, R0),
	   write(C), write('-cols is: '), nl,
	   matrix::write_matrix(R0),
	   reduce_inf(R0, C, R),
	   write('Reduced relation is: '), nl,
	   matrix::write_matrix(R).

   extensions(R, R1) :- 
       pol(R, Fs),
	   Start = [[0],[1]],
	   operators::close_relation(Start, Fs, Rs),
	   extensions(Rs, Fs, R1).

   extensions(R, Fs, R1) :-
       length(R, S),
	   tuples(S, Ts0),
	   %write('R is '), nl, matrix::write_matrix(R), nl, nl,
	   %write('tuples are '), nl,matrix::write_matrix(Ts0), nl, nl,
	   matrix::add_as_columns(R, Ts0, Ts),
	   %write('Result is: '), nl, matrix::write_matrix(Ts), nl, nl,
	   operators::close_relation(Ts, Fs, Ts1),
	   %matrix::transpose(Ts0, Ts),
	   (    matrix::remove_duplicate_rows(Ts1, Ts2),
	        matrix::remove_duplicate_columns(Ts2, R1)
	   ;    extensions(Ts1, Fs, R1) ).

    subset([], _).
    subset([X|Xs], [X|Ys]) :-
        subset(Xs, Ys).
    subset([X|Xs], [_Y|Ys]) :-
    	   subset([X|Xs], Ys).

   extend(R, N, R_ext) :-
       pol(R, Fs),
	   tuples(N, Ts0),
	   matrix::transpose(Ts0, Ts),
	   operators::close_relation(Ts, Fs, R_ext0),
       matrix::remove_duplicate_rows(R_ext0, R_ext1),
	   matrix::remove_duplicate_columns(R_ext1, R_ext).

   extension(R, R_ext) :-
       ::relation(R, Rs),
	   size(R, A),
       setof(T, (length(T, A), matrix::tuple(T)), Ts),
	   matrix::add_as_columns(Rs, Ts, Rs1),
	   matrix::matrix::remove_duplicate_rows(Rs1, Rs2),
	   ::pol(R, Fs),
	   operators::close_relation(Rs2, Fs, R_ext).

   candidate_variables(R, Xi, Xj) :-
	   differences(R, Ds),
	   keysort(Ds, Ds1),
	   list::member(_-[Xi,Xj], Ds1).

    differences(R, Ds) :-
       matrix::transpose(R, R1),
	   findall(D-[X1,X2], (list::select(X1, R1, R2),
	                     list::member(X2, R2),
						 matrix::difference(X1, X2, D0),D is D0),
			   Ds).

   remove_redundant_variables(R, R1) :-
       remove_redundant_variables(R, R, R1).

   remove_redundant_variables([[]|Rs], _, [[]|Rs]).
   remove_redundant_variables(R0, R3, R) :-
       matrix::first(R0, X, R1),
	   (    redundant(X, R3) ->
	   		R = R2
       ;    matrix::first(R, X, R2)),
	   remove_redundant_variables(R1, R3, R2).

   redundant(X, R) :-
       remove_variable(X, R, R1),
       length(R, L1),
   	   length(R1, L2),
	   L2 is floor(L1/2),
	   L2 is ceiling(L1/2).

    remove_variable(X, R0, R) :-
	    matrix::transpose(R0, R0t),
		list::select(X, R0t, Rt1),
	    matrix::transpose(Rt1, R1),
		matrix::remove_duplicate_rows(R1, R).

   tuples(N, Ts) :-
       setof(T, (length(T, N), matrix::tuple(T)), Ts).

   write_bu_sequence(R) :-
	   write_bu_sequence(2, R).

   write_bu_sequence(N, R) :-
       extend(R, N, R_ext),
       getn(N, N0),
	   atom_concat('output/seq', N0, Name),
	   write(R_ext),nl,
	   matrix::write_png(R_ext, Name),
	   N1 is N + 1,
	   write_bu_sequence(N1, R).

   write_sequence(R) :-
       extension(R, Rext), 
	   write_sequence(Rext, 1).

   getn(N, N1) :-
       N < 10,
	   atom_concat('00', N, N1).
   getn(N, N1) :-
       N > 9, N < 100,
	   atom_concat('0', N, N1).
   getn(N, N) :-
       N > 99.

   write_sequence(R, N) :-
       getn(N, N0),
	   atom_concat('output/seq', N0, Name),
	   matrix::write_png(R, Name),
	   matrix::write_matrix(R), nl,
       reduce(R, _, _, Rred),
	   Rred \= R,
	   !,
	   N1 is N + 1,
	   write_sequence(Rred, N1).

   write_row([X]) :-
       write(X).
   write_row([X,Y|Xs]) :-
       write(X),
	   write(' & '),
	   write_row([Y|Xs]).

   write_latex1([R]) :-
      write_row(R),
	  nl.
   write_latex1([R1, R2|Rs]) :-
      write_row(R1),
       write(' \\\\'),
	   nl,
	  write_latex1([R2|Rs]).

   write_latex(Rs) :-
       write('\\[R_{\\coclone{}{}{}} = \\begin{pmatrix} '),
	   nl,
	   write_latex1(Rs),
	   write('\\end{pmatrix}'),
	   nl,
	   write('\\]'),
	   nl.
:- end_object.
