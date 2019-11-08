%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1S8-2016 Paulo Moura <pmoura@logtalk.org>
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this is the actual field object, since it's a singleton
% though I'm cheaping out on the real singleton pattern

:- object(field,
   implements(monitoring)).
:- uses(random, [random/3]).
:- uses(integer, [ between/3]).

   :- info([
       version is 0.1,
       date is 2016/05/09,
       author is 'Anne Ogborn',
       comment is 'A field where the bunnies and foxes can live happily together'
   ]).
   :- initialization(init).

   :- public(grow_grass/0).
   :- info(grow_grass/0, [
     comment is 'Makes the grass get longer'
   ]).

   :- public(print_field/0).
   :- info(print_field/0, [
     comment is 'Displays the field using ascii graphics'
   ]).

    :- public(with_bunnies/1).
    :- info(with_bunnies/1, [
      comment is 'Send a message to all bunnies'
    ]).

    :- public(with_foxes/1).
    :- info(with_foxes/1, [
      comment is 'Send a message to all foxes'
    ]).

    :- public(sniff_fox/2).
    :- mode(sniff_fox(+object_identifier, -atom), zero_or_more).
    :- info(sniff_fox/2, [
      comment is 'true if Name is a bunny ID and Direction is a direction in which there is an adjacent fox',
      argnames is ['Name', 'Direction']
    ]).

    :- public(eat_grass/2).
    :- mode(eat_grass(+object_identifier, -number), zero_or_one).
    :- info(eat_grass/2, [
      comment is 'true if Name is a bunny ID and Food is an amount of food eaten. Side effect of decreasing available grass. Bunny takes care of impact on its own hunger level. Fails if cant eat here',
      argnames is ['Name', 'Food']
    ]).

    :- public(move_away_from/2).
    :- mode(move_away_from(+object_identifier, +atom), zero_or_one).
    :- info(move_away_from/2, [
      comment is 'true if Name is an animal ID and Direction is a direction away from which the animal can and does move. Side effect, the animal moves. fails if impossible',
      argnames is ['Direction', 'Name']
    ]).

    :- public(move/2).
    :- mode(move(+object_identifier, +atom), zero_or_one).
    :- info(move_away_from/2, [
      comment is 'true if Name is an animal ID and Direction is a direction towards which the animal can and does move. Side effect, the animal moves. fails if impssible ',
      argnames is ['Direction', 'Name']
    ]).

    :- public(reset_world/0).
    :- info(reset_world/0, [
      comment is 'reset the world to a new start state'
    ]).

   :- private([animal_/3, grass_/3]).
   :- dynamic([animal_/3,grass_/3]).

   %%%%%%%%%%%%%%%%%%%%% animal utilities %%%%%%%%%%%%%%%%%%%

   wabbit(Name, X, Y) :-
     animal_(Name, X, Y),
     Name::species(bunny).

   fox(Name, X, Y) :-
     animal_(Name, X, Y),
     Name::species(fox).

%%%%%%%%%%%%%%%%%%%%%%%%% init related %%%%%%%%%%%%%%%%%%%%%%%%%%%

   init :-
     self(Self),
     abolish_events(after, _, die, _, Self),
     reset_grass,
     forall(animal_(Name, _, _), Name::die),
     retractall(animal_(_,_,_)),
     bestrew_wabbits,
     bestrew_foxes,
     % only now do we listen for events
     define_events(after, _, die, _, Self),
     shiva_dance::start.

   reset_world :-
     init.

% one less than the size of the field
   field_size(15).

     % todo make a protocol for animals that both foxes and bunnies implement
     % and have a bestrew predicate
   bestrew_foxes :-
     random(2, 12, NumFoxes),
     forall(between(1, NumFoxes, _), add_a_fox).

   bestrew_wabbits :-
       random(2, 12, NumWabbits),
       forall(between(1, NumWabbits, _), add_a_wabbit).

   add_a_wabbit :-
       field_size(S),
       random(0, S, X),
       random(0, S, Y),
       bunny::new(Name),
       asserta(animal_(Name, X, Y)).

   add_a_fox :-
      field_size(S),
       random(0, S, X),
       random(0, S, Y),
       fox::new(Name),
       asserta(animal_(Name, X, Y)).

%%%%%%%%%%%%%%%%%%%%%%% convenience mapping operators %%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate(map_field(2)).
   map_field(Goal) :-
     field_size(S),
     between(0,S,X),
     between(0,S,Y),
     call(Goal, X, Y),
     fail.
   map_field(_).

 :- meta_predicate(with_foxes(::)).
   with_foxes(Goal) :-
     animal_(ID, _, _),
     ID::species(fox),
     once(ID::Goal),
     fail.
   with_foxes(_).

 :- meta_predicate(with_bunnies(::)).
   with_bunnies(Goal) :-
     animal_(ID, _, _),
     ID::species(bunny),
     once(ID::Goal),
     fail.
   with_bunnies(_).

   %%%%%%%%%%%%%%%%%%%%%%%%%  Respond to events %%%%%%%%%%%%%%%%%%%%%

   after(Animal, die, _) :-
     write('in die handler'),
     nl,
     animal_(Animal, _, _),
     write(Animal),
     write(' died'),
     nl,
     retractall(animal_(Animal, _, _)).
   after(Object, Message, Sender) :-
     write('hey strange message: '),
     write(Sender),
     write(' --> '),
     write(Message),
     write(' --> '),
     write(Object),
     nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Environment sensing %%%%%%%%%%%%%%%%%%

  sniff_fox(Bunny, left) :-
    wabbit(Bunny, X, Y),
    FX is X - 1,
    fox(_, FX, Y).
  sniff_fox(Bunny, right) :-
    wabbit(Bunny, X, Y),
    FX is X + 1,
    fox(_, FX, Y).
  sniff_fox(Bunny, up) :-
    wabbit(Bunny, X, Y),
    FY is Y - 1,
    fox(_, X, FY).
  sniff_fox(Bunny, down) :-
    wabbit(Bunny, X, Y),
    FY is Y + 1,
    fox(_, X, FY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Act on environment %%%%%%%%%%%%%%%%
  move_away_from(left, Name) :-
    move(right, Name).
  move_away_from(right, Name) :-
    move(left, Name).
  move_away_from(up, Name) :-
    move(down, Name).
  move_away_from(down, Name) :-
    move(up, Name).

% TODO need to refactor into a base class of animal.
  move(left, Name) :-
    animal_(Name, X, Y),
    X > 0,
    NX is X - 1,
    retractall(animal_(Name, _, _)),
    asserta(animal_(Name, NX, Y)).
  move(right, Name) :-
    animal_(Name, X, Y),
    field_size(N),
    X < N,
    NX is X + 1,
    retractall(animal_(Name, _, _)),
    asserta(animal_(Name, NX, Y)).
  move(up, Name) :-
    animal_(Name, X, Y),
    Y > 0,
    NY is Y - 1,
    retractall(animal_(Name, _, _)),
    asserta(animal_(Name, X, NY)).
  move(down, Name) :-
    animal_(Name, X, Y),
    field_size(N),
    Y < N,
    NY is Y + 1,
    retractall(animal_(Name, _, _)),
    asserta(animal_(Name, X, NY)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  display field %%%%%%%%%%%%%%%%%%%%%

  print_field :-
    field_size(S),
    between(0,S,Y),
    print_row(Y),
    nl,
    fail.
  print_field.

  print_row(Row) :-
    field_size(S),
    between(0,S,X),
    print_cell(X, Row),
    fail.
  print_row(_).

  print_cell(X, Y) :- print_cell_(X, Y), !.
  print_cell_(X, Y) :-
    fox(_, X,Y),
    wabbit(_, X, Y),
    write('@#$! ').
    print_cell_(X, Y) :-
      fox(_, X,Y),
      write(' ^.^ ').
    print_cell_(X, Y) :-
      wabbit(_, X,Y),
      write('>:c. ').
    print_cell_(X, Y) :-
      grass_(X,Y, Height),
      grass_symbol(Height, Symbol),
      write(Symbol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Grass %%%%%%%%%%%%%%%%%%%%%%%%%%%%
  reset_grass :-
      retractall(grass_(_,_,_)),
      map_field(reset_grass).

  reset_grass(X, Y) :-
      asserta(grass_(X, Y, 9)).

  % randomly grow some grass

  grow_grass :-
      field_size(S),
      random(0, S, X),
      random(0, S, Y),
      grass_(X, Y, Height),
      NewHeight is Height + 1,
      retractall(grass_(X, Y, _)),
      asserta(grass_(X, Y, NewHeight)),
      fail.
  grow_grass.

  grass_symbol(Height, Symbol) :-
         (    Height >= 9 ->
             grass_symbol_table(9, Symbol)
          ;    Height < 0 ->
              grass_symbol_table(0, Symbol)
         ;    grass_symbol_table(Height, Symbol)
         ).

  grass_symbol_table(9, 'WWWW ').
  grass_symbol_table(8, 'WwWW ').
  grass_symbol_table(7, 'WwWw ').
  grass_symbol_table(6, 'wwWw ').
  grass_symbol_table(5, 'wwww ').
  grass_symbol_table(4, 'w_ww ').
  grass_symbol_table(3, 'w_w_ ').
  grass_symbol_table(2, 'w___ ').
  grass_symbol_table(1, '__w_ ').
  grass_symbol_table(0, '____ ').

  % message that a bunny is eating grass. We modify the grass value but the
  % bunny takes care of it's self
  eat_grass(Bunny, Nutrition) :-
    wabbit(Bunny, X, Y),
    grass_(X, Y, Avail),
    Avail > 2,
    retractall(grass_(X, Y, _)),
    % rabbit eats half the grass but never more than 6
    Nutrition is max(6, Avail // 2),
    NA is max(0, Avail - Nutrition),
    asserta(grass_(X, Y, NA)).

:- end_object.
