:- module(state, [
    update_state/5,
    derive_percepts/5,
    derive_base_percept/3
]).

:- use_module(ontology).
:- use_module(library(http/http_log)).

update_state(Eternals, _, CurrentFluents, Action, NextFluents) :- 
    http_log("Calculating effects of ~w \n", [Action]),
    effects(Eternals, CurrentFluents, Action, NextFluents).

derive_percepts(Eternals, PreviousFluents, CurrentFluents, Action, Percepts) :-
    derive_base_percept(Eternals, CurrentFluents, BasePercepts),
    derive_bump_percept(Action, PreviousFluents, CurrentFluents, BasePercepts, Percepts0),
    derive_scream_percept(Action, PreviousFluents, CurrentFluents, Percepts0, Percepts).

derive_base_percept(Eternals, CurrentFluents, BasePercepts) :-
    stench_percept(Eternals, CurrentFluents, [], Percepts0),
    breeze_percept(Eternals, CurrentFluents, Percepts0, Percepts1),
    glitter_percept(Eternals, CurrentFluents, Percepts1, BasePercepts).

stench_percept(Eternals, Fluents, Percepts0, [stench|Percepts0]) :-
    fat{c:Position} :< Fluents.fat_hunter,
    adjacent(Eternals, Position, Adjacents),
    member(AdjacentPosition, Adjacents),
    member(EatWumpus, Eternals.eat_wumpus),
    EatWumpus.c = AdjacentPosition.

stench_percept(Eternals, Fluents, Percepts, Percepts) :-
    fat{c:Position} :< Fluents.fat_hunter,
    adjacent(Eternals, Position, Adjacents),
    \+ (
        member(AdjacentPosition, Adjacents),
        member(EatWumpus, Eternals.eat_wumpus),
        EatWumpus.c = AdjacentPosition
    ).

breeze_percept(Eternals, Fluents, Percepts0, [breeze|Percepts0]) :-
    fat{c:Position} :< Fluents.fat_hunter,
    adjacent(Eternals, Position, Adjacents),
    member(AdjacentPosition, Adjacents),
    member(EatPit, Eternals.eat_pit),
    EatPit.c = AdjacentPosition.
breeze_percept(Eternals, Fluents, Percepts, Percepts) :-
    fat{c:Position} :< Fluents.fat_hunter,
    adjacent(Eternals, Position, Adjacents),
    \+ (
        member(AdjacentPosition, Adjacents),
        member(EatPit, Eternals.eat_pit),
        EatPit.c = AdjacentPosition
    ).

glitter_percept(Eternals, Fluents, Percepts0, [glitter|Percepts0]) :-
    fat{c:Position} :< Fluents.fat_hunter,
    member(Gold, Fluents.fat_gold),
    Gold.c = Position.
glitter_percept(Eternals, Fluents, Percepts, Percepts) :-
    fat{c:Position} :< Fluents.fat_hunter,
    \+ (
        member(Gold, Fluents.fat_gold),
        Gold.c = Position
    ).

derive_bump_percept(move, PreviousFluents, CurrentFluents, Percepts, [bump | Percepts]) :-
    PreviousFluents.fat_hunter.c = CurrentFluents.fat_hunter.c.
derive_bump_percept(move, PreviousFluents, CurrentFluents, Percepts, Percepts) :-
    PreviousFluents.fat_hunter.c \= CurrentFluents.fat_hunter.c.
derive_bump_percept(Action, _, _, Percepts, Percepts) :- Action \= move.

derive_scream_percept(shoot, PreviousFluents, CurrentFluents, Percepts, [scream | Percepts]) :-
    member(wumpus{id:W}, PreviousFluents.alive),
    \+ member(wumpus{id:W}, CurrentFluents.alive).
derive_scream_percept(shoot, PreviousFluents, CurrentFluents, Percepts, Percepts) :-
    forall(
        member(wumpus{id:W}, PreviousFluents.alive),
        member(wumpus{id:W}, CurrentFluents.alive)
    ).
derive_scream_percept(Action, _, _, Percepts, Percepts) :- Action \= shoot.

