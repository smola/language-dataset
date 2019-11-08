% #const rate=1. % NOT IMPLEMENTED
#include "./action-MPP.lp".
#include <csp>.

ordered(order(O),product(A),N) :- init(object(order,O),  value(line,(A,N))).
shelved(shelf(S),product(A),N) :- init(object(product,A),value(on,  (S,N))), ordered(order(O),product(A)).

limit(A,O,S,(N+N'-|N-N'|)/2) :- ordered(O,A,N), shelved(S,A,N').

&dom{N..N} =   ordered(O,A,0) :- ordered(O,A,N).
&dom{N..N} =   shelved(S,A,0) :- shelved(S,A,N).



&dom{0..N} =   ordered(O,A,T) :- ordered(O,A,N), time(T).
&dom{0..N} =   shelved(S,A,T) :- shelved(S,A,N), time(T).

&dom{0..M} = process(A,O,S,T) :- limit(A,O,S,M), time(T).

process(A,O,S,T) :- limit(A,O,S,M), &sum { process(A,O,S,T) } > 0, time(T).

process(O,S,T) :- process(_,O,S,T).

process(S,T) :- process(_,S,T).

:- { process(_,_,_,T) } > 1, time(T).

:- ordered(O,A,N), &sum { ordered(O,A,T); process(A,O,S,T) : limit(A,O,S,M) } != ordered(O,A,T-1), time(T).
:- shelved(S,A,N), &sum { shelved(S,A,T); process(A,O,S,T) : limit(A,O,S,M) } != shelved(S,A,T-1), time(T).


:- process(O,S,T), target(O,P), position(P,C), not position(S,C,T-1).


:- process(S,T), not carries(_,S,T-1).
:- process(S,T), carries(R,S,T-1), not waits(R,T).
