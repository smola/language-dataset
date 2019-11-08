% Dependency rule operator, note that it has same precendece as :-
:- op(1200,xfx,'<-').
:- dynamic('<-'/2).

:- object(banpipe).
	:- public(load/1).
	
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/13,
		comment is 'The main object for interaction with banpipe scripts.']).

	:- dynamic('<-'/2).
	
	:- public(version/0).
	:- info(version/0,[comment is 'Writes current version of BANpipe.']).
	version :-
		version(V),
		writeln(V).

    	:- public(version/1).
    	:- info(version/1, [
		comment is 'Version is the current version of banpipe',
		argnames is ['Version']]).
	version('1.0 alpha release 1').

	:- public(listing/1).
	:- info(listing/1, 
		[ comment is 'Lists all banpipe dependency rules where the goal G occurs in the head.',
		argnames is ['G']]).
	listing(Goal) :-
		% escaped using {}/1 operator to find clause from "global" database rather than this objects database
		findall([Head,Body],({clause('<-'(Head,Body),true)},term_extras::conjunction_as_list(Head,HeadList),list::nth1(_,HeadList,Goal)),Rules),
		forall(list::member([Head,Body],Rules),(write(Head), write(' <- '), writeln(Body))).

	:- public(listing/0).
	:- info(listing/0,
		[ comment is 'Lists all banpipe dependency rules.']).
	listing :- listing(_).
	
	% This simply loads a script using Prologs normal mechanism
	:- info(load/1, [
		comment is 'Loads a banpipe script. Script is the (quoted) filename of the script (absolute or relative to current directory).',
		argnames is ['Script']]).
	load(Script) :- {[Script]}.
	
	:- public(run/1).
	:- info(run/1, [
		comment is 'RRecursively (sequentially) compute the File associated with Goal.',
		argnames is ['Goal']]).
	run(Goal) :-
		::run(Goal,_).

	:- public(run/2).
	:- info(run/2, [
		comment is 'Recursively (sequentially) compute the File associated with Goal.',
		argnames is ['Goal','File']]).

	run(Goal,Result) :-
		run_with_semantics(execution_semantics,Goal,Result).
		
	:- private(run_with_semantics/3).
	run_with_semantics(Semantics,Goal,Result) :-
		(config::get(trace,true) ->
			Sem=trace(Semantics)
			;
			Sem=Semantics),
		sequential_interpreter(Sem)::run(Goal,Result).


	:- public(prun/1).
	:- info(prun/1, [
		comment is 'Compute the File associated with Goal. Independent sub-tasks are computed in parallel.',
		argnames is ['Goal']]).
	prun(Goal) :-
		prun(Goal,_).

	:- public(prun/2).
	:- info(prun/2,[
		comment is 'Compute the File associated with Goal. Independent sub-tasks are computed in parallel.',
		argnames is ['Goal','File']]).

:- if(current_logtalk_flag(threads,supported)).
	prun(Goal,Result) :-
		callgraph(Goal,_,Callgraph),
		scheduler_tree::from_trace(Callgraph,Tree),
		scheduler_tree::reduce_tree(Tree,ReducedTree),
		scheduler::run(ReducedTree,[]),
		% Result should now be available on file, use 'sequential' run to retrieve it
		run(Goal,Result).
:- else.
	prun(_Goal,_Result) :-
		reporting::error('parallel execution is not supported.').
:- endif.

	:- public(callgraph/1).
	callgraph(Goal) :-
		::callgraph(Goal,_,Callgraph),
		scheduler_tree::from_trace(Callgraph,Tree1),
		scheduler_tree::reduce_tree(Tree1,Tree2),
		scheduler_tree::print(Tree2).
	
	:- public(callgraph/3).
	callgraph(Goal,Result,Callgraph) :-
		sequential_interpreter(callgraph_semantics)::run(Goal,Result,Callgraph), !.

	:- public(typecheck/1).
	:- info(typecheck/1,[
		comment is 'Typecheck Goal -- recursively check that the types of input files for Goal are compatible.',
		argnames is ['Goal']]).

	typecheck(Goal) :-
		typecheck(Goal,_).
		
	:- public(typecheck/2).
	:- info(typecheck/2,[
		comment is 'Typecheck Goal -- recursively check that the types of input files for Goal are compatible and unify resulting Type.',
		argnames is ['Goal','Type']]).
	typecheck(Goal,Type) :-
		run_with_semantics(typecheck_semantics,Goal,Type).
			
	:- public(trace/0).
	trace :- config::set(trace).
	
	:- public(notrace/0).
	notrace :- config::pop(trace) ; true.
	
:- end_object.
