:- object(hook_debug,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/4/9,
		comment is 'Compiler hook support for activating debug statements.']).

	goal_expansion(debug(Goal), Goal).

:- end_object.

:- object(hook_production,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/4/9,
		comment is 'Compiler hook support for discarding debug statements.']).

	goal_expansion(debug(_), true).

:- end_object.
