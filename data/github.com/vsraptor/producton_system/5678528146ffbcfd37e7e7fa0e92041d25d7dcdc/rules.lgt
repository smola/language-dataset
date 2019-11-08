:- op(1102,  fy, rule).
:- op(1101,  xfy, ==>).

:- op(800, xfy, and).
:- op(800, xfy, or).
:- op(790,  fy, not).

% global keyword for creating rules like :
%   rule <conditions> ==> <actions>.
% Ex.:  :- rule true and true ==> write(it_is_true).

rule(R) :- ==>(C,A) = R, rules::add(C,A).

:- object(rules, instantiates(rules)).

	:- uses(user, [say/1, say/2]).

%	:- meta_predicate check(*).
	:- public uid/1, last_ix/1, gamma/1, lrate/1.
	:- dynamic uid/1, last_ix/1, gamma/1, lrate/1.
	uid(0).
	last_ix(0). %last executed rule
	gamma(0.9).
	lrate(0.1).

	:- public [set_uid/1, get_uid/1, get_lix/1, set_lix/1]. %%fixme
	get_uid(U) :- ::uid(U).
	set_uid(U) :- ::retractall(uid(_)), ::assertz(uid(U)).
	new_uid(NU) :- ::uid(U), NU is U + 1, set_uid(NU).
	get_lix(Ix) :- ::last_ix(Ix).
	set_lix(Ix) :- ::retractall(last_ix(_)), ::assertz(last_ix(Ix)).


	:- public [add/2, run/0, run/1, run/2, updateq/2].
	:- public [max_elem/3, check_cond/2].

	% add new rule to the rule-db : cond(IF,Q,Ix) + act(Ix) :- THEN.
%	add(IF, THEN) :- new_uid(U), {assertz(cond(IF, 0, U))}, Rule =.. [ act, U, THEN ], {assertz(Rule)}.
	add(IF, THEN) :- new_uid(U), {assertz(cond(IF, 0, U))}, {assertz(act(U) :- THEN)}.

	%update Q-value of a rule
	updateq(Ix, NewQ) :- {clause(cond(IF, _Q, Ix), true)}, { retractall(cond(_,_,Ix)), assertz(cond(IF,NewQ,Ix)) }.

	max_elem([[Q,Ix]|T], MaxQ, MaxIx) :- max_elem(T, Q, Ix, MaxQ, MaxIx).
	max_elem([], Max, Ix, Max, Ix).
	max_elem([[Q,Ix]|T], QAcc, IxAcc, MaxQ, MaxIx ) :- Q > QAcc -> max_elem(T, Q, Ix, MaxQ, MaxIx) ; max_elem(T, QAcc, IxAcc, MaxQ, MaxIx).

	%check/evaluate logical expressions
	check(true) :- !.
	check(not Cond) :- !, \+ check(Cond).
	check(Cond and Conds) :- !, (check(Cond) , check(Conds)).
	check(Cond or  Conds) :- !, (check(Cond) ; check(Conds)), !.
	check(Cond) :- !, user::call(Cond).

	check_cond(Q, Ix) :- {cond(Cond, Q, Ix)}, check(Cond).
	step(Env) :-
		findall([Q,Ix], check_cond(Q,Ix), QI), % find all cond that are true and collect their Q , Ix
		%writeln(QI),
		max_elem(QI, _, MaxIx), % get rule-Ix with the max Q
		user::act(MaxIx), % find&execute the rule with Ix
		Env::reward(R),
		get_lix(LIx), (LIx =:= 0 -> Ix1 is MaxIx ; Ix1 is LIx),
		update_rule(MaxIx, Ix1, R), set_lix(MaxIx).

	run(_,0) :- !.
	run(Env, Steps) :- 
		step(Env), NS is Steps - 1, run(Env, NS).
	run(Env) :- run(Env,1).
	run :- run(env).

	%rules management ...
	:- public [do/1, check_rule/1, list/1, list/0, clean/0].
	update_rule(Ix0, Ix1, Reward) :-
		{ clause(cond(_,Q0,Ix0), true), clause(cond(_,Q1,Ix1), true) },
		::gamma(Gamma), ::lrate(Lrate), Error is Reward + Gamma * Q1 - Q0,
		NewQ is Q0 + Lrate * Error, updateq(Ix0, NewQ).
	check_rule(Ix) :- check_cond(_, Ix).

	do(Ix) :- {clause(act(Ix,Run), true)}, call(Run). % find&execute the rule with Ix
	list(Ix) :- { cond(C,Q,Ix), clause(act(Ix),Act) }, format('~d:~2f| ', [Ix,Q]), user::portray_clause((C :- Act)).
	list([]).
	list([H|T]) :- list(H), list(T).
	list :- findall(Ix, { cond(_,_,Ix) }, Ixs) , list(Ixs).
	clean :-  { retractall(cond(_,_,_)), retractall(act(_,_)) }.



:- end_object.
