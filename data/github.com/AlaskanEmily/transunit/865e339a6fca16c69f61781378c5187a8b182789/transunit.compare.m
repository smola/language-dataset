% Copyright (C) 2019 Alaskan Emily, Transnat Games.
%
% This software is provided 'as-is', without any express or implied warranty.
% In no event will the authors be held liable for any damages arising from
% the use of this software.
%
% Permission is granted to anyone to use this software for any purpose,
% including commercial applications, and to alter it and redistribute it
% freely, subject to the following restrictions:
%
%   1. The origin of this software must not be misrepresented; you must not
%      claim that you wrote the original software. If you use this software
%      in a product, an acknowledgment in the product documentation would be
%      appreciated but is not required.
%
%   2. Altered source versions must be plainly marked as such, and must not
%      be misrepresented as being the original software.
%
%   3. This notice may not be removed or altered from any source distribution.
%

:- module transunit.compare.

%==============================================================================%
% General components for the unit test framework.
% I know this isn't great. But it has no dependencies, and there are not a lot
% of prebuilt solutions for Mercury.
:- interface.
%==============================================================================%

:- use_module bool.
:- use_module rbtree.
:- use_module pair.
:- use_module array.
:- use_module array2d.
:- use_module set.

%------------------------------------------------------------------------------%

:- instance to_string(int).
:- instance to_string(string).
:- instance to_string(float).
:- instance to_string(bool.bool).
:- instance to_string(maybe.maybe(T)) <= to_string(T).
:- instance to_string(pair.pair(A, B)) <= (to_string(A), to_string(B)).

%------------------------------------------------------------------------------%

:- instance compare(list(T)) <= (compare(T), to_string(T)).
:- instance compare(set.set(T)) <= (compare(T), to_string(T)).
%:- instance compare(rbtree.rbtree(K, V)) <= (compare(V), to_string(K), to_string(V)).
%:- instance compare(tree.tree(K, V)) <= (compare(V), to_string(K), to_string(V)).
:- instance compare(int).
:- instance compare(string).
:- instance compare(float).
:- instance compare(bool.bool).
:- instance compare(maybe.maybe(T)) <= (to_string(T), compare(T)).
:- instance compare(pair.pair(A, B)) <= (to_string(A), to_string(B), compare(A), compare(B)).
:- instance compare(array.array(T)) <= (to_string(T), compare(T)).
:- instance compare(array2d.array2d(T)) <= (to_string(T), compare(T)).
:- instance compare(maybe.maybe_error(T, E))
    <= (to_string(T), compare(T), to_string(E), compare(E)).

%------------------------------------------------------------------------------%

:- func generic_compare(T, T) = maybe.maybe_error <= to_string(T).

%------------------------------------------------------------------------------%

:- func negate(float) = float.

%------------------------------------------------------------------------------%
% float_equals(A, B)
:- pred float_equals(float, float).
:- mode float_equals(in, in) is semidet.
:- mode float_equals(di, di) is semidet.

%------------------------------------------------------------------------------%
% Promise the associativity of float comparisons
:- promise all[A, B] (
    float_equals(A, B) <=> float_equals(B, A)
).

%------------------------------------------------------------------------------%

:- promise all[A, B] (
    float_equals(A, B) <=> float_equals(negate(A), negate(B))
).

%------------------------------------------------------------------------------%

:- promise all[A, B] (
    (negate(A) = B) <=> (negate(B) = A)
).

%------------------------------------------------------------------------------%

:- promise all[A, B] (
    some [C] (negate(A) = C, negate(B) = C, A = B)
).

%------------------------------------------------------------------------------%
% float_equals(A, B, Epsilon)
:- pred float_equals(float, float, float).
:- mode float_equals(in, in, in) is semidet.
:- mode float_equals(di, di, in) is semidet.

%------------------------------------------------------------------------------%

:- promise all[A, B, Epsilon] (
    float_equals(A, B, Epsilon) <=> float_equals(B, A, Epsilon)
).

%==============================================================================%
:- implementation.
%==============================================================================%

:- import_module float.
:- use_module int.
:- use_module string.
:- use_module std_util.

%------------------------------------------------------------------------------%

:- instance to_string(int) where [
    func(to_string/1) is string.from_int
].

:- instance to_string(string) where [
    func(to_string/1) is std_util.id
].

:- instance to_string(float) where [
    func(to_string/1) is string.from_float
].

:- instance to_string(bool.bool) where [
    (to_string(bool.yes) = "bool.yes"),
    (to_string(bool.no) = "bool.no")
].

:- instance to_string(maybe.maybe(T)) <= to_string(T) where [
    (to_string(maybe.yes(That)) =
        string.append("maybe.yes(", string.append(to_string(That), ")"))),
    (to_string(maybe.no) = "maybe.no")
].

:- instance to_string(pair.pair(A, B)) <= (to_string(A), to_string(B)) where [
    to_string(P) =
        string.append(string.append(
            to_string(pair.fst(P)), " - "),
            to_string(pair.snd(P)))
].

%------------------------------------------------------------------------------%

generic_compare(A, B) = Result :-
    ( if
        A = B 
    then
        Result = maybe.ok
    else
        Message = string.join_list(" != ", map(to_string, [A|[B|[]]])),
        Result = maybe.error(Message)
    ).

%------------------------------------------------------------------------------%

:- pred accumulate_mismatch(T, T, list(string), list(string), int, int)
    <= compare(T).
:- mode accumulate_mismatch(in, in, in, out, in, out) is det.

accumulate_mismatch(A, B, !List, I, int.plus(I, 1)) :-
    compare(A, B) = MaybeResult,
    (
        MaybeResult = maybe.ok
    ;
        MaybeResult = maybe.error(Error),
        string.append("Element ", string.from_int(I), Prefix),
        string.append(string.append(Prefix, "\t: "), Error, Message),
        list.cons(Message, !List)
    ).

%------------------------------------------------------------------------------%

:- instance compare(list(T)) <= (compare(T), to_string(T)) where [
    ( compare(A, B) = Result :-
        list.length(A, ALen), list.length(B, BLen),
        generic_compare(ALen, BLen) = LenCompare,
        (
            LenCompare = maybe.ok,
            list.foldl2_corresponding(accumulate_mismatch, A, B, [], Errors, 0, _),
            ( if
                list.is_empty(Errors)
            then
                Result = maybe.ok
            else
                Result = maybe.error(string.join_list("\n", Errors))
            )
        ;
            LenCompare = maybe.error(Error),
            Result = maybe.error(string.append("List length ", Error))
        )
    )
].

:- instance compare(set.set(T)) <= (compare(T), to_string(T)) where [
    ( compare(A, B) = Result :-
        set.count(A, ALen), set.count(B, BLen),
        generic_compare(ALen, BLen) = LenCompare,
        (
            LenCompare = maybe.ok,
            ( set.to_sorted_list(A, AList) & set.to_sorted_list(B, BList) ),
            compare(AList, BList) = Result
        ;
            LenCompare = maybe.error(Error),
            Result = maybe.error(string.append("List length ", Error))
        )
    )
].

%:- instance compare(rbtree.rbtree(K, V)) <= (compare(V), to_string(K), to_string(V)) where [
%].

%:- instance compare(tree.tree(K, V)) <= (compare(V), to_string(K), to_string(V)) where [
%].

:- instance compare(int) where [
    func(compare/2) is generic_compare
].

:- instance compare(string) where [
    ( compare(A, B) = Result :-
        ( A = B -> Result = maybe.ok
        ; Result = maybe.error(string.join_list(" != ", [A|[B|[]]])) )
    )
].

:- instance compare(float) where [
    ( compare(A, B) = Result :-
        ( float_equals(A, B) -> Result = maybe.ok
        ; Message = string.join_list(" != ", map(string.from_float, [A|[B|[]]])),
          Result = maybe.error(Message) )
    )
].

:- instance compare(bool.bool) where [
    ( compare(bool.yes, bool.yes) = maybe.ok ),
    ( compare(bool.no, bool.no) = maybe.ok ),
    ( compare(bool.yes, bool.no) = maybe.error("bool.yes != bool.no") ),
    ( compare(bool.no, bool.yes) = maybe.error("bool.no != bool.yes") )
].

:- instance compare(maybe.maybe(T)) <= (to_string(T), compare(T)) where [
    ( compare(maybe.no, maybe.no) = maybe.ok ),
    ( compare(maybe.no, maybe.yes(B)) = maybe.error(
        string.append("maybe.no != maybe.yes(", string.append(to_string(B), ")")) )),
    ( compare(maybe.yes(A), maybe.no) = maybe.error(
        string.append("maybe.yes(", string.append(to_string(A), ") != maybe.no")) )),
    ( compare(maybe.yes(A), maybe.yes(B)) = compare(A, B) )
].

:- instance compare(pair.pair(A, B)) <= (to_string(A), to_string(B), compare(A), compare(B)) where [
    ( compare(A, B) = Result :-
        A1 = pair.fst(A),
        B1 = pair.fst(B),
        A2 = pair.snd(A),
        B2 = pair.snd(B),
        
        compare(A1, B1) = Cmp1,
        compare(A2, B2) = Cmp2,
        (
            Cmp1 = maybe.ok,
            (
                Cmp2 = maybe.ok,
                Result = maybe.ok
            ;
                Cmp2 = maybe.error(Err),
                Result = maybe.error(string.append(string.append(string.append(
                    to_string(A), " != "),
                    " For second element:"),
                    Err))
            )
        ;
            Cmp1 = maybe.error(Err1),
            (
                Cmp2 = maybe.ok,
                Result = maybe.error(string.append(string.append(string.append(
                    to_string(A), " != "),
                    " For first element:"),
                    Err1))
            ;
                Cmp2 = maybe.error(Err2),
                string.first_char(Err2Tail, ',', Err2),
                Result = maybe.error(string.append(string.append(string.append(string.append(
                    to_string(A), " != "),
                    " For both elements element:"),
                    Err1),
                    Err2Tail))
            )
        )
    )
].

:- instance compare(array.array(T)) <= (to_string(T), compare(T)) where [
    ( compare(A, B) = Result :-
        array.size(A, ALen), array.size(B, BLen),
        generic_compare(ALen, BLen) = LenCompare,
        (
            LenCompare = maybe.ok,
            ( array.to_list(A, AList) & array.to_list(B, BList) ),
            compare(AList, BList) = Result
        ;
            LenCompare = maybe.error(Error),
            Result = maybe.error(string.append("Array length ", Error))
        )
    )
].

:- instance compare(array2d.array2d(T)) <= (to_string(T), compare(T)) where [
    ( compare(A, B) = Result :-
        array2d.bounds(A, AW, AH), array2d.bounds(B, BW, BH),
        generic_compare(AW, BW) = WCompare,
        generic_compare(AH, BH) = HCompare,
        (
            WCompare = maybe.ok,
            HCompare = maybe.ok,
            % Kind of silly. Join the lists.
            (
              ( array2d.lists(A) = ALists,
                list.foldl(list.append, ALists, []) = AList ) &
              ( array2d.lists(B) = BLists,
                list.foldl(list.append, BLists, []) = BList )
            ),
            compare(AList, BList) = Result
        ;
            WCompare = maybe.ok,
            HCompare = maybe.error(Error),
            Result = maybe.error(string.append("Array2D height ", Error))
        ;
            WCompare = maybe.error(Error),
            HCompare = maybe.ok,
            Result = maybe.error(string.append("Array2D width ", Error))
        ;
            WCompare = maybe.error(WError),
            HCompare = maybe.error(HError),
            string.append("Array2D width ", WError, W),
            string.append("Array2D height ", HError, H),
            Result = maybe.error(string.join_list("\n", [W|[H|[]]]))
        )
    )
].

:- instance transunit.compare(maybe.maybe_error(T, E))
    <= (transunit.to_string(T), transunit.compare(T),
        transunit.to_string(E), transunit.compare(E)) where [
    ( compare(maybe.ok(A), maybe.ok(B)) = transunit.compare(A, B) ),
    ( compare(maybe.error(A), maybe.error(B)) = transunit.compare(A, B) ),
    ( compare(maybe.ok(OK), maybe.error(E)) =
        maybe.error(string.append(
            string.append("OK:", transunit.to_string(OK)),
            string.append(" != Error:", transunit.to_string(E)))) ),
    ( compare(maybe.error(E), maybe.ok(OK)) =
        maybe.error(string.append(
            string.append("Error:", transunit.to_string(E)),
            string.append(" != OK:", transunit.to_string(OK)))) )
].

%------------------------------------------------------------------------------%

negate(X) = -X.

%------------------------------------------------------------------------------%

float_equals(A, B) :-
    abs(A - B) =< float.epsilon.

float_equals(A, B, Epsilon) :-
    abs(A - B) =< Epsilon.
