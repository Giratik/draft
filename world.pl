:- module(world,[create_world/2]).

:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(random)).
:- use_module(library(gensym)).
:- use_module(library(pprint)).

% Size determined by N Wumpus
create_world(NWumpus, world{eternals: Eternals, fluents: Fluents}) :-
    Size #= NWumpus * 4,
    NPits #= NWumpus * 3,
    NGold #= NWumpus,
    NArrows #= NWumpus,
    %NExit #= NWumpus,
    init_eternals(Size, NPits, NWumpus, Eternals),
    init_fluents(Eternals, NGold, NArrows, Fluents).

%%% Fluents initialization %%%
init_fluents(Eternals, NGold, NArrows, Fluents) :-
    Fluents = fluents{
        fat_hunter:fat{c:c{x:1,y:1},h:hunter{id:hunter}},
        fat_gold:GoldFat,
        has_gold:[],
        has_arrow:Arrows,
        visited:[],
        alive:[hunter{id:hunter}|AliveWumpuses],
        dir:[dir{h:hunter{id:hunter},d:north}],
        game_state:running,
        score:0
    },
    % Construct the list of arrows the hunter has at the beginning
    length(Arrows, NArrows),
    maplist([has{h:hunter{id:hunter},a:arrow{id:Arrow}}]>>(
        gensym(arrow, Arrow)
    ), Arrows),
    % Get a clean list of cells without walls, exit, or pit
    exclude({Eternals}/[Cell]>>(
        (
            get_dict(eat_walls, Eternals, Walls), member(W, Walls), get_dict(c, W, Cell)
        ) ; 
        get_dict(eat_exit, Eternals, Cell) ;
        (get_dict(eat_pit, Eternals, Pits), member(P, Pits), get_dict(c, P, Cell))
    ), Eternals.cells, CleanCells),
    assign_gold(CleanCells, NGold, GoldFat),
    assign_alive_wumpuses(Eternals, AliveWumpuses).

assign_alive_wumpuses(Eternals, AliveWumpuses) :-
    maplist([eat{c:_,w:W}, W]>>(true), Eternals.eat_wumpus, AliveWumpuses).

assign_gold(Cells, NGold, GoldFat) :-
    % Get a random list of cells to assign the gold
    random_permutation(Cells, CellsRandom),
    % Get the first NGold cells
    length(GoldCells, NGold),
    append(GoldCells, _, CellsRandom),
    maplist([Cell, fat{c:Cell,g:gold{id:GoldSym}}]>>(
        gensym(gold, GoldSym)
    ), GoldCells, GoldFat).

%%% Eternals initialization %%%
init_eternals(Size, NPits, NWumpus, Eternals) :-
    WorldSize #= (Size + 2) * (Size + 2),
	LimXInf #= 0, LimYInf #= 0,
	LimXSup #= Size + 1, LimYSup #= Size + 1,
    %NPits #= Size,
    length(Cells, WorldSize),
    % Generate all the cells with the correct coordinates
    foldl(
        {LimXInf,LimXSup,LimYInf,LimYSup}/[c{x:X0,y:Y0}, [X0,Y0], [X,Y]]>>(
            calc_coords(X0,Y0,LimXSup,X,Y)
        ),
        Cells, [LimXInf,LimYInf], _
    ),
    % Create the basic dict for the eternals and set the exit by default to 1,1
    BaseEternals = eternals{
        eat_exit: eat{c:c{x:1,y:1},e:exit{id:exit}},
        eat_walls: [],
        eat_wumpus: [],
        eat_pit: [],
        cells: Cells
    },
    % Assign the walls to the cells at the borders
    assign_walls(BaseEternals, Cells, LimXSup, LimYSup, WallsEternals),
    % Get a clean list of cells without walls or exit
    exclude({WallsEternals}/[Cell]>>(
        (
            get_dict(eat_walls, WallsEternals, Walls),
            member(W, Walls), get_dict(c, W, Cell)
        ) ; (
            % TODO: Make this better
            % For now cheat and exclude 1,1
            Cell = c{x:1,y:1}
        )
    ), Cells, NonWallNonExitCells),
    % Assign the pits to the cells
    assign_pits(WallsEternals, NonWallNonExitCells, NPits, PitsEternals),
    assign_wumpus(PitsEternals, NonWallNonExitCells, NWumpus, WumpusEternals),
    Eternals = WumpusEternals.

assign_walls(BaseEternals, Cells, LimXSup, LimYSup, WallsEternals) :-
    foldl({LimXSup,LimYSup}/[c{x:X,y:Y}, Eternals, NewEternals]>>(
        (
            (X = 0 ; X = LimXSup ; Y = 0 ; Y = LimYSup) -> 
            (
                gensym(wall, WallSym),
                get_dict(eat_walls, Eternals, PrevWalls),
                put_dict(eat_walls, Eternals, [eat{c:c{x:X,y:Y},w:wall{id:WallSym}}|PrevWalls], NewEternals)
            ) ; 
            (
                NewEternals = Eternals
            )
        )
    ), Cells, BaseEternals, WallsEternals).

assign_pits(Eternals, Cells, NPits, PitsEternals) :-
    % Get a random list of cells to assign the pits
    random_permutation(Cells, CellsRandom),
    % Get the first NPits cells
    length(PitCells, NPits),
    append(PitCells, _, CellsRandom),
    foldl([Cell, Eternals, NewEternals]>>(
        gensym(pit, PitSym),
        get_dict(eat_pit, Eternals, PrevPits),
        put_dict(eat_pit, Eternals, [eat{c:Cell,p:pit{id:PitSym}}|PrevPits], NewEternals)
    ), PitCells, Eternals, PitsEternals).

assign_wumpus(Eternals, Cells, NWumpus, WumpusEternals) :-
    % Get a random list of cells to assign the wumpuses
    random_permutation(Cells, CellsRandom),
    % Get the first NPits cells
    length(WumpusCells, NWumpus),
    append(WumpusCells, _, CellsRandom),
    foldl([Cell, Eternals, NewEternals]>>(
        gensym(wumpus, WumpusSym),
        get_dict(eat_wumpus, Eternals, PrevWumpus),
        put_dict(eat_wumpus, Eternals, [eat{c:Cell,w:wumpus{id:WumpusSym}}|PrevWumpus], NewEternals)
    ), WumpusCells, Eternals, WumpusEternals).

calc_coords(X0,Y0,LimXSup,X1,Y1) :-
	(( X0 #= LimXSup) #==> (X1 #= 0 #/\ Y1 #= Y0 + 1) ) #/\
	(( X0 #\=  LimXSup) #==> (X1 #= X0 +1 #/\ Y1 #= Y0)).