:- module(wumpus, [sim_step/6, init/3, wumpus_example/1]).
%%% Wumpus world simulator
%%% IAS 4A - ESIEA
%%% By: Camilo Correa Restrepo
%%%

:- use_module(library(http/http_log)).


:- use_module(state).
:- use_module(world).

wumpus_example(
    world{ 
        eternals:eternals{ 
            cells:[
                c{x:0,y:0},c{x:1,y:0},c{x:2,y:0},c{x:3,y:0},c{x:4,y:0},c{x:5,y:0},
                c{x:0,y:1},c{x:1,y:1},c{x:2,y:1},c{x:3,y:1},c{x:4,y:1},c{x:5,y:1},
                c{x:0,y:2},c{x:1,y:2},c{x:2,y:2},c{x:3,y:2},c{x:4,y:2},c{x:5,y:2},
                c{x:0,y:3},c{x:1,y:3},c{x:2,y:3},c{x:3,y:3},c{x:4,y:3},c{x:5,y:3},
                c{x:0,y:4},c{x:1,y:4},c{x:2,y:4},c{x:3,y:4},c{x:4,y:4},c{x:5,y:4},
                c{x:0,y:5},c{x:1,y:5},c{x:2,y:5},c{x:3,y:5},c{x:4,y:5},c{x:5,y:5}
            ],
            eat_exit:eat{c:c{x:1,y:1},e:exit{id:exit}},
            eat_pit:[eat{c:c{x:4,y:4},p:pit{id:pit3}},eat{c:c{x:3,y:3},p:pit{id:pit2}},eat{c:c{x:3,y:1},p:pit{id:pit1}}],
            eat_walls:[ 
                eat{c:c{x:5,y:5},w:wall{id:wall20}},
                eat{c:c{x:4,y:5},w:wall{id:wall19}},
                eat{c:c{x:3,y:5},w:wall{id:wall18}},
                eat{c:c{x:2,y:5},w:wall{id:wall17}},
                eat{c:c{x:1,y:5},w:wall{id:wall16}},
                eat{c:c{x:0,y:5},w:wall{id:wall15}},
                eat{c:c{x:5,y:4},w:wall{id:wall14}},
                eat{c:c{x:0,y:4},w:wall{id:wall13}},
                eat{c:c{x:5,y:3},w:wall{id:wall12}},
                eat{c:c{x:0,y:3},w:wall{id:wall11}},
                eat{c:c{x:5,y:2},w:wall{id:wall10}},
                eat{c:c{x:0,y:2},w:wall{id:wall9}},
                eat{c:c{x:5,y:1},w:wall{id:wall8}},
                eat{c:c{x:0,y:1},w:wall{id:wall7}},
                eat{c:c{x:5,y:0},w:wall{id:wall6}},
                eat{c:c{x:4,y:0},w:wall{id:wall5}},
                eat{c:c{x:3,y:0},w:wall{id:wall4}},
                eat{c:c{x:2,y:0},w:wall{id:wall3}},
                eat{c:c{x:1,y:0},w:wall{id:wall2}},
                eat{c:c{x:0,y:0},w:wall{id:wall1}}
            ],
            eat_wumpus:[eat{c:c{x:1,y:3},w:wumpus{id:wumpus1}}]
        },
        fluents:fluents{ 
            alive:[hunter{id:hunter},wumpus{id:wumpus1}],
            dir:[dir{d:north,h:hunter{id:hunter}}],
            fat_gold:[fat{c:c{x:2,y:3},g:gold{id:gold}}],
            fat_hunter:fat{c:c{x:1,y:1},h:hunter{id:hunter}},
            has_arrow:[has{a:arrow{id:arrow1},h:hunter{id:hunter}}],
            has_gold:[],
            visited:[],
            game_state:running,
            score:0
        }
    }
).

%%% Entrypoint - Generate the world state
init(Size, WorldDict, InitPercepts) :-
    create_world(Size, WorldDict),
    derive_base_percept(WorldDict.eternals, WorldDict.fluents, InitPercepts).

%% handle sim request
sim_step(Eternals, Fluents, PreviousFluents, Action, NextFluents, Percepts) :-
    http_log('Updating State w/ action: ~w', [Action]),
    update_state(Eternals, PreviousFluents, Fluents, Action, NextFluents),
    http_log_stream(Stream),
    http_log('Calculated Next Fluents:', []),
    print_term(NextFluents, [output(Stream),nl(true)]),
    derive_percepts(Eternals, Fluents, NextFluents, Action, Percepts),
    http_log('Calculated Next Percepts: ~w', [Percepts]).