:- category(sit_man).

    :- info([ version is 1.1
            , author is 'Paul Brown'
            , date is 2019/11/1
            , comment is 'A Situation Manager.'
            ]).

    :- private(sit_/1).
    :- dynamic(sit_/1).
    % Set in importer

    :- public(sit/1).
    :- mode(sit(?term), zero_or_one).
    :- info(sit/1,
        [ comment is 'The current situation.'
        , argnames is ['Situation']
        ]).
    sit(S) :-
        ::sit_(S).

   :- private(clobber_sit/1).
   :- mode(clobber_sit(+term), one).
   :- info(clobber_sit/1,
       [ comment is 'Assert the new Situation, retracting all prior ones'
       , argnames is ['Situation']
       ]).
   clobber_sit(S) :-
       ::retractall(sit_(_)),
       ::assertz(sit_(S)).

:- end_category.
