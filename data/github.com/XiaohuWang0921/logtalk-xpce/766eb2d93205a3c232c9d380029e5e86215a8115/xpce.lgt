:- object(xpce(_Reference), implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Xiaohu Wang',
		date is 2018/01/04,
		comment is 'Minimal abstraction for calling SWI-Prolog XPCE object methods without return value (free/1 and send-methods) using familiar message sending syntax.',
		parameter is [
			'Reference' - 'Either a class, a temporary object or an object reference.'
		]
	]).

	:- public([
		destroy/0,
		send/1,
		send/2,
		send/3,
		send/4,
		send/5,
		send/6,
		send/7
	]).

	:- meta_predicate(pce_principal:free(*)).
	:- meta_predicate(pce_principal:send(*, *)).
	:- meta_predicate(pce_principal:send(*, *, *)).
	:- meta_predicate(pce_principal:send(*, *, *, *)).
	:- meta_predicate(pce_principal:send(*, *, *, *, *)).
	:- meta_predicate(pce_principal:send(*, *, *, *, *, *)).
	:- meta_predicate(pce_principal:send(*, *, *, *, *, *, *)).
	:- meta_predicate(pce_principal:send(*, *, *, *, *, *, *, *)).

	:- mode(destroy, one).
	:- info(destroy/0, [
		comment is 'Destroys the receiver object and frees the memory it occupies.',
		argnames is []
	]).

	:- mode(send(+term), zero_or_one).
	:- info(send/1, [
		comment is 'Invokes a send-method.',
		argnames is ['SendMethod']
	]).

	:- mode(send(+atom, +term), zero_or_one).
	:- info(send/2, [
		comment is 'Invokes a send-method with a given selector and an argument.',
		argnames is ['Selector', 'Arg1']
	]).

	:- mode(send(+atom, +term, +term), zero_or_one).
	:- info(send/3, [
		comment is 'Invokes a send-method with a given selector and two arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2']
	]).

	:- mode(send(+atom, +term, +term, +term), zero_or_one).
	:- info(send/4, [
		comment is 'Invokes a send-method with a given selector and three arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3']
	]).

	:- mode(send(+atom, +term, +term, +term, +term), zero_or_one).
	:- info(send/5, [
		comment is 'Invokes a send-method with a given selector and four arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3', 'Arg4']
	]).
	
	:- mode(send(+atom, +term, +term, +term, +term, +term), zero_or_one).
	:- info(send/6, [
		comment is 'Invokes a send-method with a given selector and five arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5']
	]).

	:- mode(send(+atom, +term, +term, +term, +term, +term, +term), zero_or_one).
	:- info(send/7, [
		comment is 'Invokes a send-method with a given selector and six arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5', 'Arg6']
	]).

	destroy :-
		parameter(1, Reference),
		pce_principal:free(Reference).
	
	send(SendMethod) :-
		parameter(1, Reference),
		pce_principal:send(Reference, SendMethod).
	
	send(Selector, Arg1) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1).
	
	send(Selector, Arg1, Arg2) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1, Arg2).
	
	send(Selector, Arg1, Arg2, Arg3) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1, Arg2, Arg3).
	
	send(Selector, Arg1, Arg2, Arg3, Arg4) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1, Arg2, Arg3, Arg4).
	
	send(Selector, Arg1, Arg2, Arg3, Arg4, Arg5) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1, Arg2, Arg3, Arg4, Arg5).
	
	send(Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) :-
		parameter(1, Reference),
		pce_principal:send(Reference, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).
	
	forward(Message) :-
		send(Message).

:- end_object.

:- object(xpce(_Reference, _ReturnValue), implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Xiaohu Wang',
		date is 2018/01/04,
		comment is 'Minimal abstraction for calling SWI-Prolog XPCE object methods with a return value (new/2 and get-methods) using familiar message sending syntax.'
	]).

	:- public([
		create/0,
		get/1,
		get/2,
		get/3,
		get/4,
		get/5,
		get/6
	]).

	:- meta_predicate(pce_principal:new(*, *)).
	:- meta_predicate(pce_principal:get(*, *, *)).
	:- meta_predicate(pce_principal:get(*, *, *, *)).
	:- meta_predicate(pce_principal:get(*, *, *, *, *)).
	:- meta_predicate(pce_principal:get(*, *, *, *, *, *)).
	:- meta_predicate(pce_principal:get(*, *, *, *, *, *, *)).
	:- meta_predicate(pce_principal:get(*, *, *, *, *, *, *, *)).

	:- mode(create, zero_or_one).
	:- info(create/0, [
		comment is 'Allocates the needed memory, creates an object and returns its reference',
		argnames is []
	]).

	:- mode(get(+term), zero_or_one).
	:- info(get/1, [
		comment is 'Invokes a get-method.',
		argnames is ['GetMethod']
	]).

	:- mode(get(+atom, +term), zero_or_one).
	:- info(get/2, [
		comment is 'Invokes a get-method with a given selector and an argument.',
		argnames is ['Selector', 'Arg1']
	]).

	:- mode(get(+atom, +term, +term), zero_or_one).
	:- info(get/3, [
		comment is 'Invokes a get-method with a given selector and two arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2']
	]).

	:- mode(get(+atom, +term, +term, +term), zero_or_one).
	:- info(get/4, [
		comment is 'Invokes a get-method with a given selector and three arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3']
	]).

	:- mode(get(+atom, +term, +term, +term, +term), zero_or_one).
	:- info(get/5, [
		comment is 'Invokes a get-method with a given selector and four arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3', 'Arg4']
	]).

	:- mode(get(+atom, +term, +term, +term, +term, +term), zero_or_one).
	:- info(get/6, [
		comment is 'Invokes a get-method with a given selector and five arguments.',
		argnames is ['Selector', 'Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5']
	]).

	create :-
		parameter(1, Constructor),
		parameter(2, Reference),
		pce_principal:new(Reference, Constructor).
	
	get(GetMethod) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, GetMethod, ReturnValue).
	
	get(Selector, Arg1) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, Selector, Arg1, ReturnValue).
	
	get(Selector, Arg1, Arg2) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, Selector, Arg1, Arg2, ReturnValue).
		
	get(Selector, Arg1, Arg2, Arg3) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, Selector, Arg1, Arg2, Arg3, ReturnValue).
		
	get(Selector, Arg1, Arg2, Arg3, Arg4) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, Selector, Arg1, Arg2, Arg3, Arg4, ReturnValue).
		
	get(Selector, Arg1, Arg2, Arg3, Arg4, Arg5) :-
		parameter(1, Reference),
		parameter(2, ReturnValue),
		pce_principal:get(Reference, Selector, Arg1, Arg2, Arg3, Arg4, Arg5, ReturnValue).
	
	forward(Message) :-
		get(Message).

:- end_object.