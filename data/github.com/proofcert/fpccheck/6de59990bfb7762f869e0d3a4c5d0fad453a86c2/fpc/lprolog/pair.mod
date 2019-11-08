%TODO How to add debugging, here and elsewhere? Best during translation?

module pair.

ffClerk (pair# L0 R0) :-
	ffClerk L0,
	ffClerk R0.

ttClerk (pair# L0 R0) (pair# L1 R1) :-
	ttClerk L0 L1,
	ttClerk R0 R1.

andClerk (pair# L0 R0) (pair# L1 R1) :-
	andClerk L0 L1,
	andClerk R0 R1.

orClerk (pair# L0 R0) (pair# L1 R1) (pair# L2 R2) :-
	orClerk L0 L1 L2,
	orClerk R0 R1 R2.

impClerk (pair# L0 R0) (pair# L1 R1) :-
	impClerk L0 L1,
	impClerk R0 R1.

eqClerk (pair# L0 R0) (pair# L1 R1) :-
	eqClerk L0 L1,
	eqClerk R0 R1.

ttExpert (pair# L0 R0) :-
	ttExpert L0,
	ttExpert R0.

andExpert (pair# L0 R0) (pair# L1 R1) (pair# L2 R2) :-
	andExpert L0 L1 L2,
	andExpert R0 R1 R2.

% removed separate branches for each choice, what if both sides don't care?
orExpert (pair# L0 R0) (pair# L1 R1) C :-
	orExpert L0 L1 C,
	orExpert R0 R1 C.

impExpert (pair# L0 R0) (pair# L1 R1) (pair# L2 R2) :-
	impExpert L0 L1 L2,
	impExpert R0 R1 R2.

impExpert' (pair# L0 R0) (pair# L1 R1) (pair# L2 R2) :-
	impExpert' L0 L1 L2,
	impExpert' R0 R1 R2.

eqExpert (pair# L0 R0) :-
	eqExpert L0,
	eqExpert R0.

allClerk (pair# L0 R0) (x\ pair# (L1 x) (R1 x)) :-
	allClerk L0 L1,
	allClerk R0 R1.

someClerk (pair# L0 R0) (x\ pair# (L1 x) (R1 x)) :-
	someClerk L0 L1,
	someClerk R0 R1.

allExpert (pair# L0 R0) (pair# L1 R1) T :-
	allExpert L0 L1 T,
	allExpert R0 R1 T.

someExpert (pair# L0 R0) (pair# L1 R1) T :-
	someExpert L0 L1 T,
	someExpert R0 R1 T.

indClerk (pair# L0 R0) (pair# L1 R1) (x\ pair# (L2 x) (R2 x)) S :-
	indClerk L0 L1 L2 S,
	indClerk R0 R1 R2 S.

indClerk' (pair# L0 R0) (x\ pair# (L1 x) (R1 x)) :-
	indClerk' L0 L1,
	indClerk' R0 R1.

coindClerk (pair# L0 R0) (pair# L1 R1) (x\ pair# (L2 x) (R2 x)) S :-
	coindClerk L0 L1 L2 S,
	coindClerk R0 R1 R2 S.

coindClerk' (pair# L0 R0) (x\ pair# (L1 x) (R1 x)) :-
	coindClerk' L0 L1,
	coindClerk' R0 R1.

unfoldLClerk (pair# L0 R0) (pair# L1 R1) :-
	unfoldLClerk L0 L1,
	unfoldLClerk R0 R1.

unfoldRExpert (pair# L0 R0) (pair# L1 R1) :-
	unfoldRExpert L0 L1,
	unfoldRExpert R0 R1.

unfoldLExpert (pair# L0 R0) (pair# L1 R1) :-
	unfoldLExpert L0 L1,
	unfoldLExpert R0 R1.

unfoldRClerk (pair# L0 R0) (pair# L1 R1) :-
	unfoldRClerk L0 L1,
	unfoldRClerk R0 R1.

freezeLClerk (pair# L0 R0) (pair# L1 R1) (idx2 IL IR) :-
	freezeLClerk L0 L1 IL,
	freezeLClerk R0 R1 IR.

initRExpert (pair# L0 R0) (idx2 IL IR) :-
	initRExpert L0 IL,
	initRExpert R0 IR.

freezeRClerk (pair# L0 R0) (pair# L1 R1) :-
	freezeRClerk L0 L1,
	freezeRClerk R0 R1.

initLExpert (pair# L0 R0) :-
	initLExpert L0,
	initLExpert R0.

storeLClerk (pair# L0 R0) (pair# L1 R1) (idx2 IL IR) :-
	storeLClerk L0 L1 IL,
	storeLClerk R0 R1 IR.

decideLClerk (pair# L0 R0) (pair# L1 R1) (idx2 IL IR) :-
	decideLClerk L0 L1 IL,
	decideLClerk R0 R1 IR.

decideLClerk' (pair# L0 R0) (pair# L1 R1) I :-
	decideLClerk' L0 L1 I,
	decideLClerk' R0 R1 I.

storeRClerk (pair# L0 R0) (pair# L1 R1) :-
	storeRClerk L0 L1,
	storeRClerk R0 R1.

decideRClerk (pair# L0 R0) (pair# L1 R1) :-
	decideRClerk L0 L1,
	decideRClerk R0 R1.

releaseLExpert (pair# L0 R0) (pair# L1 R1) :-
	releaseLExpert L0 L1,
	releaseLExpert R0 R1.

releaseRExpert (pair# L0 R0) (pair# L1 R1) :-
	releaseRExpert L0 L1,
	releaseRExpert R0 R1.

end
