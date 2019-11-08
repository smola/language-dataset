% Load modules

% :- logtalk_load(['../queries.lgt', '../xmi.lgt', '../model.lgt'],[debug(on)]).

:- object(direct(_Package,_LocalProfile,_CodeProfile)).
:- public([tr/4,tr/3]).
:- protected([package/1, profiles/2, profile/1]).

package(Package):-
    parameter(1, Package).

profile(Profile):-
    parameter(2, Profile).

profile(Profile):-
    parameter(3, Profile).

profiles(L):-
    findall(Profile, ::profile(Profile), L).

tr(class, Class, ClassID):-
    ::package(Package),
    query(Package)::class(Name, ClassID),
    create_object(Class, [instantiates(class)],[],[]),
    create_object(Attributes, [instantiates(params)],[],[]),
    create_object(Methods, [instantiates(methodlist)],[],[]),
    Class::name(Name),
    forall(
        ::tr(attribute, Attribute, ClassID, _AttributeID),
        Attributes::append(Attribute)
    ),
    forall(
        ::tr(method, Method, ClassID, _MethodID),
        Methods::append(Method)
    ),
    Class::attributes(Attributes),
    Class::methods(Methods).

tr(attribute, Attribute, ClassID, AttributeID):-
    ::package(Package),
    query(Package)::attribute(Name, ClassID, AttributeID),
    % query(Package)::type(Type, AttributeID),
    create_object(Attribute, [instantiates(param)],[],[]),
    Attribute::name(Name).

tr(method, Method, ClassID, MethodID):-
    ::package(Package),
    query(Package)::method(Name, ClassID, MethodID),
    create_object(Method, [instantiates(method)],[],[]),
    Method::name(Name).

:- end_object.
