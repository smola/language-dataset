:- module people.

:- interface.
:- import_module list.

% Persons participating in the scriptural record.  A constructor consists of a
% name and a number.  This matches the convention used in the Book of Mormon
% Reference Companion and various Bible dictionaries.
% If only one person ever has that name, the number is omitted.
:- type person --->
    ishmael_1;
    ishmaels_children;
    jacob_2;
    joseph_2;
    laman_1;
    lehi_1;
    lehi_1_daughters;  % Lehi's daughters
    lemuel_1;
    nephi_1;
    sam_1;
    sariah_1;
    zoram.

:- pred person_name(person, string).
:- mode person_name(    in,    out) is det.
:- mode person_name(   out,     in) is semidet.

:- pred etal(list(person)::in, string::out) is semidet.

:- implementation.
:- import_module string.

person_name(ishmael_1,          "Ishmael").
person_name(ishmaels_children,  "Ishmael's Children").
person_name(jacob_2,            "Jacob").
person_name(joseph_2,           "Joseph").
person_name(laman_1,            "Laman").
person_name(lehi_1,             "Lehi").
person_name(lehi_1_daughters,   "Lehi's Daughters").
person_name(lemuel_1,           "Lemuel").
person_name(nephi_1,            "Nephi").
person_name(sam_1,              "Sam").
person_name(sariah_1,           "Sariah").
person_name(zoram,              "Zoram").

etal([Person], Label) :-
    person_name(Person, Label).
etal([One, Two], Label) :-
    person_name(One, OneName),
    person_name(Two, TwoName),
    format("%s and %s", [s(OneName),s(TwoName)], Label).
etal([One, Two, _|_], Label) :-
    person_name(One, OneName),
    person_name(Two, TwoName),
    format("%s, %s, et al", [s(OneName),s(TwoName)], Label).
