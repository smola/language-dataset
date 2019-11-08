
:- object(xml).

	:- info([
		version is 1.0,
		author is 'Vincent Marchetti and Paulo Moura',
		date is 2006/06/15,
		comment is 'Predicates for XML output of data model validation results.']).

	:- public(output/1).
	:- mode(output(+compound), one).
	:- info(output/1, [
		comment is 'Outputs a data model validation results in XML format to standard output.',
		argnames is ['Results']]).

	:- public(output/2).
	:- mode(output(+stream, +compound), one).
	:- info(output/2, [
		comment is 'Outputs a data model validation results in XML format to an open stream.',
		argnames is ['Stream', 'Results']]).

	output(Results) :-
		current_output(Stream),
		xml(Results, Stream, 0).

	output(Stream, Results) :-
		xml(Results, Stream, 0).

	xml([], _, _).

	xml([Failure| Failures], Stream, Indent) :-
		xml(Failure, Stream, Indent),
		xml(Failures, Stream, Indent).

	xml(data_model(InstanceFailures, RuleFailures), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		inc_indent(Indent2, Indent3),
		xml_open_tag(failures, Stream, Indent1),
		xml_open_tag(instances, Stream, Indent2),
		xml(InstanceFailures, Stream, Indent3),
		xml_close_tag(instances, Stream, Indent2),
		xml_open_tag(global_rules, Stream, Indent2),
		xml(RuleFailures, Stream, Indent3),
		xml_close_tag(global_rules, Stream, Indent2),
		xml_close_tag(failures, Stream, Indent1).

	xml(instance(Self, AttributeFailures, DomainRuleFailures), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		inc_indent(Indent2, Indent3),
		xml_open_tag(instance, Stream, Indent1),
		xml_tag(name, Self, Stream, Indent2),
		xml_open_tag(attributes, Stream, Indent2),
		xml(AttributeFailures, Stream, Indent3),
		xml_close_tag(attributes, Stream, Indent2),
		xml_open_tag(domain_rules, Stream, Indent2),
		xml(DomainRuleFailures, Stream, Indent3),
		xml_close_tag(domain_rules, Stream, Indent2),
		xml_close_tag(instance, Stream, Indent1).

	xml(global_rule(Rule, RuleFailures), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		xml_open_tag(global_rule, Stream, Indent1),
		xml_tag(name, Rule, Stream, Indent2),
		xml_tag(parts, RuleFailures, Stream, Indent2),
		xml_close_tag(global_rule, Stream, Indent1).

	xml(invalid(Name, Type, Value), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		xml_open_tag(invalid, Stream, Indent1),
		xml_tag(name, Name, Stream, Indent2),
		xml_tag(type, Type, Stream, Indent2),
		xml_tag(value, Value, Stream, Indent2),
		xml_close_tag(invalid, Stream, Indent1).

	xml(missing(Name, Type), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		xml_open_tag(missing, Stream, Indent1),
		xml_tag(name, Name, Stream, Indent2),
		xml_tag(type, Type, Stream, Indent2),
		xml_close_tag(missing, Stream, Indent1).

	xml(domain_rule(Ancestor, Rule), Stream, Indent1) :-
		inc_indent(Indent1, Indent2),
		xml_open_tag(domain_rule, Stream, Indent1),
		xml_tag(ancestor, Ancestor, Stream, Indent2),
		xml_tag(rule, Rule, Stream, Indent2),
		xml_close_tag(domain_rule, Stream, Indent1).

	xml_open_tag(Tag, Stream, Indent) :-
		spaces(Stream, Indent),
		write(Stream, '<'), write(Stream, Tag), write(Stream, '>'), nl(Stream).

	xml_close_tag(Tag, Stream, Indent) :-
		spaces(Stream, Indent),
		write(Stream, '</'), write(Stream, Tag), write(Stream, '>'), nl(Stream).

	xml_tag(Tag, Contents, Stream, Indent) :-
		spaces(Stream, Indent),
		write(Stream, '<'), write(Stream, Tag), write(Stream, '>'),
		write(Stream, Contents),
		write(Stream, '</'), write(Stream, Tag), write(Stream, '>'), nl(Stream).

	spaces(_, 0) :-
		!.
	spaces(Stream, Indent1) :-
		write(Stream, ' '),
		Indent2 is Indent1 - 1,
		spaces(Stream, Indent2).

	inc_indent(Indent, NewIndent) :-
		NewIndent is Indent + 4.

:- end_object.