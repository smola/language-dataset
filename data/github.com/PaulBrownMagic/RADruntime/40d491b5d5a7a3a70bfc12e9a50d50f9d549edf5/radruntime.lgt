
:- object(radruntime).

    :- public(sit/1).
    :- dynamic(sit/1).
    :- public(init_sit/1).
    init_sit(S) :-
        asserta(sit(S)).
    :- private(clobber_sit/1).
    clobber_sit(S) :-
        retractall(sit(_)),
        asserta(sit(S)).

    :- public(update/1).
    update(A) :-
        ::sit(S),
        A::do(S, S1),
        clobber_sit(S1).

:- end_object.


:- object(view,
    implements(monitoring)).

    :- uses(logtalk, [
        print_message/3
    ]).

    after(radruntime, update(Action), _Sender) :-
        print_message(information, rad, 'Did'-Action),
        radruntime::sit(S),
        ::render(S).

    :- public(render/1).
    render(State) :-
        findall(Fluent, situation::holds(Fluent, State), Fluents),
        print_message(information, rad, 'Fluents'::Fluents),
        findall(Action, situation::poss(Action, State), Actions),
        print_message(information, rad, 'Actions'::Actions).

:- end_object.
