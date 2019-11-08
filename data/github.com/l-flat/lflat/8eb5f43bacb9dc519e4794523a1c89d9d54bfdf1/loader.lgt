
:- initialization((
	% load Logtalk libraries used by L-FLAT:
	logtalk_load(types(loader)),
	logtalk_load(gensym(gensym)),
	logtalk_load(roots(loader)),
	% load L-FLAT itself:
	logtalk_load(lflat_home(lflat), [unknown_entities(silent), optimize(on)]),
	logtalk_load(lflat_home(hooks), [optimize(on)]),
	% print the L-FLAT banner:
	interaction::banner
)).


% some handy shortcuts to run the L-FLAT provided examples:
% (examples are located in the $HOME/lflat/examples directory)

run_example(Example) :-
	interaction::run_example(Example).

run_examples(List) :-
	interaction::run_examples(List).

all :-
	run_example(all).
