:- module(ontology, [doables/3, effects/4, adjacent/3, next_pos/3]).
:- use_module(library(clpfd)).
:- use_module(library(http/http_log)).

adjacent(Eternals, Position, Adjacents) :-
    findall(
        Adjacent,
        (
            member(Adjacent, Eternals.cells),
            (
                Adjacent.x #= Position.x #/\ Adjacent.y #= Position.y + 1
            ) #\/ (
                Adjacent.x #= Position.x #/\ Adjacent.y #= Position.y - 1
            ) #\/ (
                Adjacent.x #= Position.x + 1 #/\ Adjacent.y #= Position.y
            ) #\/ (
                Adjacent.x #= Position.x - 1 #/\ Adjacent.y #= Position.y
            )
        ),
        Adjacents
    ).

%% actions -> move, turn, grab, shoot, climb
%% doable(Eternals, Fluents, Action)
doable(_, _, move).
doable(_, _, left).
doable(_, _, right).
doable(_, _, grab).
doable(_, Fluents, shoot) :- 
    Arrows = Fluents.has_arrow,
    member(Arrow, Arrows),
    Arrow.h.id = hunter.
doable(Eternals, Fluents, climb) :-
    Eternals.eat_exit.c = Fluents.fat_hunter.c.

doables(Eternals, Fluents, Doables) :-
    findall(Action, doable(Eternals, Fluents, Action), Doables).

%% calculate the next position of the hunter given his direction
next_pos(c{x:X,y:Y}, north, c{x:X,y:Y1}) :- Y1 #= Y + 1. %position suivante est vers le nord => alors y +=1
next_pos(c{x:X,y:Y}, south, c{x:X,y:Y1}) :- Y1 #= Y - 1.
next_pos(c{x:X,y:Y}, east, c{x:X1,y:Y}) :- X1 #= X + 1.
next_pos(c{x:X,y:Y}, west, c{x:X1,y:Y}) :- X1 #= X - 1.

turn_left(north, west).
turn_left(west, south).
turn_left(south, east).
turn_left(east, north).
turn_right(north, east).
turn_right(east, south).
turn_right(south, west).
turn_right(west, north).


fatal(Eternals, Fluents, Position) :-
    member(PitEat, Eternals.eat_pit), %il y un pit
    PitEat.c = Position,    %on check la position
    \+ (
        member(WumpusEat, Eternals.eat_wumpus),
        WumpusEat.c = Position,
        \+ (member(WumpusEat.w, Fluents.alive)) %le wumpus peut se trouver sur une case pit, en le tuant il bouche le trou
    ).
fatal(Eternals, Fluents, Position) :-
    member(WumpusEat, Eternals.eat_wumpus),
    WumpusEat.c = Position,
    member(WumpusEat.w, Fluents.alive). %wumpus même case et vivant

%% calculate the effects of an action
effects(Eternals, Fluents, move, ResultingFluents) :-
    http_log('Trying case 1 for Move \n', []),
    Fluents.fat_hunter = fat{c:OldPos, h:hunter{id:hunter}},
    % We know for now that there is only one hunter, otherwise we need to find which one
    % is being asked to move
    Fluents.dir = [dir{d:Dir, h:hunter{id:hunter}}],
    next_pos(OldPos, Dir, NewPos),
    member(WallEat, Eternals.eat_walls),
    WallEat.c = NewPos,
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(score,NewScore).
effects(Eternals, Fluents, move, ResultingFluents) :-
    http_log('Trying case 2 for Move \n', []),
    Fluents.fat_hunter = fat{c:OldPos, h:hunter{id:hunter}},
    % We know for now that there is only one hunter, otherwise we need to find which one
    % is being asked to move
    Fluents.dir = [dir{d:Dir, h:hunter{id:hunter}}],
    next_pos(OldPos, Dir, NewPos),
    \+ (
        member(WallEat, Eternals.eat_walls),
        WallEat.c = NewPos
    ),
    % We know the next position is not a wall, so we can move
    % Check if the position is a pit or a wumpus
    fatal(Eternals, Fluents, NewPos),
    selectchk(hunter{id:hunter}, Fluents.alive, NewAlive),
    NewScore #= Fluents.score - 1000,
    ResultingFluents = Fluents.put(fat_hunter/c,NewPos).put(alive,NewAlive).put(score,NewScore).put(game_state,finished).
effects(Eternals, Fluents, move, ResultingFluents) :-
    http_log('Trying case 3 for Move \n', []),
    Fluents.fat_hunter = fat{c:OldPos, h:hunter{id:hunter}},
    % We know for now that there is only one hunter, otherwise we need to find which one
    % is being asked to move
    Fluents.dir = [dir{d:Dir, h:hunter{id:hunter}}],
    next_pos(OldPos, Dir, NewPos),
    \+ (
        member(WallEat, Eternals.eat_walls),
        WallEat.c = NewPos
    ),
    \+ fatal(Eternals, Fluents, NewPos),
    % We know the next position is not a wall, so we can move
    %%Here we know we can indeed move, so we update the position of the hunter
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents
                        .put(fat_hunter/c,NewPos)
                        .put(visited, [v{to:NewPos,from:OldPos}|Fluents.visited])
                        .put(score,NewScore).

effects(_, Fluents, left, ResultingFluents) :-
    Fluents.dir = [dir{d:Dir, h:hunter{id:hunter}}],
    turn_left(Dir, NewDir),
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(dir, [dir{d:NewDir, h:hunter{id:hunter}}]).put(score,NewScore).

effects(_, Fluents, right, ResultingFluents) :-
    Fluents.dir = [dir{d:Dir, h:hunter{id:hunter}}],
    turn_right(Dir, NewDir),
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(dir, [dir{d:NewDir, h:hunter{id:hunter}}]).put(score,NewScore).

effects(_, Fluents, grab, ResultingFluents) :-
    Fluents.fat_hunter = fat{c:Pos, h:hunter{id:hunter}},
    Fluents.fat_gold = Golds,
    member(Gold, Golds),
    fat{c:Pos, g:gold{id:GID}} = Gold,
    selectchk(Gold, Golds, NewGolds),
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents
                        .put(fat_gold, NewGolds)
                        .put(has_gold, [has{h:hunter{id:hunter},g:gold{id:GID}}|Fluents.has_gold])
                        .put(score,NewScore).

effects(_, Fluents, grab, ResultingFluents) :-
    Fluents.fat_hunter = fat{c:Pos, h:hunter{id:hunter}}, %on vérifie la position du chasseur
    Fluents.fat_gold = Golds,   %idem pour l'or
    member(Gold, Golds),
    \+ (fat{c:Pos, g:gold{id:_}} = Gold),
%selectchk(Gold, Golds, NewGolds),
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(score,NewScore).

effects(_, Fluents, shoot, ResultingFluents) :-
    %%If no arrows, nothing happens
    member(Arrow, Fluents.has_arrow),
    \+ Arrow.h = hunter{id:hunter},
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(score,NewScore).

% Case where the wumpus is hit by the arrow.
effects(Eternals, Fluents, shoot, ResultingFluents) :-
    member(Arrow, Fluents.has_arrow),
    Arrow.h = hunter{id:hunter},
    Fluents.fat_hunter = fat{c:HunterPosition, h:hunter{id:hunter}},
    Fluents.dir = [dir{d:HunterDirection, h:hunter{id:hunter}}],
    in_direction(Fluents, Eternals, HunterPosition, HunterDirection, ShotWId, ShotWPos),
    selectchk(Arrow, Fluents.has_arrow, NewArrows),
    http_log("Shot wumpus ~w at position ~w~n \n", [ShotWId, ShotWPos]),
    selectchk(wumpus{id:ShotWId}, Fluents.alive, NewAlive),
    NewScore #= Fluents.score - 10,
    ResultingFluents = Fluents.put(has_arrow, NewArrows).put(alive, NewAlive).put(score,NewScore).
% Case where the wumpus is not hit by the arrow.
effects(Eternals, Fluents, shoot, ResultingFluents) :-
    member(Arrow, Fluents.has_arrow),
    Arrow.h = hunter{id:hunter},
    Fluents.fat_hunter = fat{c:HunterPosition, h:hunter{id:hunter}},
    Fluents.dir = [dir{d:HunterDirection, h:hunter{id:hunter}}],
    \+ in_direction(Fluents, Eternals, HunterPosition, HunterDirection, _, _),
    selectchk(Arrow, Fluents.has_arrow, NewArrows),
    http_log("Shot and didn't hit the wumpus \n", []),
    NewScore #= Fluents.score - 10,
    ResultingFluents = Fluents.put(has_arrow, NewArrows).put(score,NewScore).

%%% Climb not  on exit %action exit
effects(Eternals, Fluents, climb, ResultingFluents) :-
    ExitPos = Eternals.eat_exit.c,
    Fluents.fat_hunter = fat{c:HunterPos, h:hunter{id:hunter}},
    ExitPos \= HunterPos, %l'un ne s'unifie pas à l'autre
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(score,NewScore).

%%% Climb effects w/ gold
effects(Eternals, Fluents, climb, ResultingFluents) :-
    ExitPos = Eternals.eat_exit.c,
    Fluents.fat_hunter = fat{c:ExitPos, h:hunter{id:hunter}},
    length(Fluents.has_gold, NGold),
    NGold #> 0,
    NewScore #= Fluents.score + 1000 * NGold,
    ResultingFluents = Fluents.put(score,NewScore).put(game_state,finished).

%%% Climb effects w/ no gold
effects(Eternals, Fluents, climb, ResultingFluents) :-
    ExitPos = Eternals.eat_exit.c,
    Fluents.fat_hunter = fat{c:ExitPos, h:hunter{id:hunter}},
    length(Fluents.has_gold, NGold),
    NGold #= 0,
    NewScore #= Fluents.score - 1,
    ResultingFluents = Fluents.put(score,NewScore).put(game_state,finished).
    

%% Determines if there is a wumpus in the direction that the agent is facing
%% and if there is, returns both the position and the id of that wumpus
%% so that it's state can be updated after shooting (and to check if the shot)
%% has an effect at all. The first clause is commented in detail but the idea
%% is the same for all the others where what varies is the constraint on
%% whether x or y must be either the same or resp bigger or smaller.
%% TODO: We must check that the wumpuses are alive for the calculations to be correct
in_direction(Fluents, Eternals, HunterPosition, HunterDirection, ShotWId, ShotWPos) :-
    HunterDirection = north,
    %% agent position and coordinates
    % ::holds(at(agent, Position)),
    % Position::(x(XP),y(YP)),
    XP = HunterPosition.x, YP = HunterPosition.y,
    %% Find all the wumpuses that are in the same direction (same X coordinate in this case since it is looking up)
    %% and that have a larger Y position (since it is looking up)
    findall( %donc les Y plus petits sont ignorés
        eat{c:Pos,w:W}, 
        (
            member(eat{c:Pos,w:W}, Eternals.eat_wumpus),
            member(W, Fluents.alive),
            % WPosition::(x(XP),y(YW)),
            Pos.x #= XP, Pos.y #> YP
        ), 
        Ws
    ),
    %% This is the equivalent of "there exists" one wumpus (and select tries all the possibilites of those found)
    %% this is to deal with which do we hit if there are several in the same straight line
    select(eat{c:ShotWPos,w:wumpus{id:ShotWId}}, Ws, WsP),
    %% Determine the Y position of this wumpus
    % WPositionP::y(YWP),
    %% and determine the distance between the wumpus and the player.
    D #= abs(YP - ShotWPos.y),
    %% now comes the such that part,
    %% it must be the case that for all other wumpuses, they must be further away,
    %% i.e. the distance from them to the player must be larger than the distance
    %% calculated above
    forall(
        (
            % member(at(wumpus(_), WPositionPP),WsP), 
            member(eat{c:OtherWPos,w:wumpus{id:_}},WsP) 
            % WPositionPP::y(YWPP)
        ), (
            abs(YP - OtherWPos.y) #> D
        ) 
    ),
    %% Since these calculations are very expensive, we do a cut to (a) avoid leaving a choice point
    %% checking other possibilities, since the cases are always differentiated by agent's direction
    !.

in_direction(Fluents, Eternals, HunterPosition, HunterDirection, ShotWId, ShotWPos) :-
    HunterDirection = east,
    XP = HunterPosition.x, YP = HunterPosition.y,
    findall(
        eat{c:Pos,w:W}, 
        (
            member(eat{c:Pos,w:W}, Eternals.eat_wumpus),
            member(W, Fluents.alive),
            % WPosition::(x(XP),y(YW)),
            Pos.x #< XP, Pos.y #= YP
        ), 
        Ws
    ),
    select(eat{c:ShotWPos,w:wumpus{id:ShotWId}}, Ws, WsP),
    D #= abs(XP - ShotWPos.x),
    forall(
        (
            member(eat{c:OtherWPos,w:wumpus{id:_}},WsP) 
        ), (
            abs(XP - OtherWPos.x) #> D
        ) 
    ),
    !.

in_direction(Fluents, Eternals, HunterPosition, HunterDirection, ShotWId, ShotWPos) :-
    HunterDirection = south,
    XP = HunterPosition.x, YP = HunterPosition.y,
    findall(
        eat{c:Pos,w:W}, 
        (
            member(eat{c:Pos,w:W}, Eternals.eat_wumpus),
            member(W, Fluents.alive),
            % WPosition::(x(XP),y(YW)),
            Pos.x #= XP, Pos.y #< YP
        ), 
        Ws
    ),
    select(eat{c:ShotWPos,w:wumpus{id:ShotWId}}, Ws, WsP),
    D #= abs(YP - ShotWPos.y),
    forall(
        (
            member(eat{c:OtherWPos,w:wumpus{id:_}},WsP) 
        ), (
            abs(YP - OtherWPos.y) #> D
        ) 
    ),
    !.

in_direction(Fluents, Eternals, HunterPosition, HunterDirection, ShotWId, ShotWPos) :-
    HunterDirection = west,
    XP = HunterPosition.x, YP = HunterPosition.y,
    findall(
        eat{c:Pos,w:W}, 
        (
            member(eat{c:Pos,w:W}, Eternals.eat_wumpus),
            member(W, Fluents.alive),
            % WPosition::(x(XP),y(YW)),
            Pos.x #< XP, Pos.y #= YP
        ), 
        Ws
    ),
    select(eat{c:ShotWPos,w:wumpus{id:ShotWId}}, Ws, WsP),
    D #= abs(XP - ShotWPos.x),
    forall(
        (
            member(eat{c:OtherWPos,w:wumpus{id:_}},WsP) 
        ), (
            abs(XP - OtherWPos.x) #> D
        ) 
    ),
    !.


