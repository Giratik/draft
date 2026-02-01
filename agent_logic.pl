:- module(logic, [run_hunter/0]).

% --- IMPORTS ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(clpfd)).
:- use_module(library(pita)).

:- pita.

:- set_setting(http:cors, [*]).
:- cors_enable.

% --- SERVEUR ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~n[SERVER] Hunter (DIAGNOSTIC MODE) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    format(user_error, '~n================ DECISION CYCLE ================~n', []),
    format(user_error, '[IN] Percepts: ~w~n', [Percepts]),
    
    ( catch(extract_state(BeliefsDict, CurrentState), _, fail)
    -> true 
    ;  CurrentState = [at(1,1), facing(north)], format(user_error, '[WARN] Extraction State Failed! Using fallback.~n', [])
    ),
    format(user_error, '[IN] State: ~w~n', [CurrentState]),

    ( catch(prob(choose_action_prob(CurrentState, Percepts, A, NS), Prob), Error, true)
    -> 
        ( var(Error) ->
            Action = A, NewState = NS,
            format(user_error, '[OUT] DECISION: ~w (P=~2f)~n', [Action, Prob])
        ;
            format(user_error, '[CRASH] PITA Error: ~w~n', [Error]),
            Action = move, NewState = CurrentState
        )
    ;
        format(user_error, '[FAIL] No rule applied! Forcing RIGHT.~n', []),
        Action = right, NewState = CurrentState
    ),

    update_fluents_dict(BeliefsDict, NewState, UpdatedBeliefsDict),
    Response = _{
        hunterState: _{ beliefs: UpdatedBeliefsDict, percepts: Percepts },
        action: Action
    },
    cors_enable,
    reply_json_dict(Response).


% --- MOTEUR PHYSIQUE (INSTRUMENTÉ) ---

physics_move(State, NewState) :-
    format(user_error, '   [PHYS] Trying move... ', []),
    member(at(X,Y), State),
    member(facing(Dir), State),
    
    delta(Dir, DX, DY),
    NextX #= X + DX,
    NextY #= Y + DY,
    
    % On accepte une grille large (0..10) pour être sûr que ce n'est pas un problème de bornes
    NextX in 0..10, NextY in 0..10,
    
    label([NextX, NextY]),
    format(user_error, 'Valid Target: (~w,~w) ', [NextX, NextY]),
    
    select(at(X,Y), State, at(NextX, NextY), NewState),
    format(user_error, '-> OK.~n', []).

physics_turn_right(State, NewState) :-
    member(facing(Dir), State), next_dir_right(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

physics_turn_left(State, NewState) :-
    member(facing(Dir), State), next_dir_left(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

delta(north, 0, 1).   delta(south, 0, -1).
delta(east, 1, 0).    delta(west, -1, 0).
next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).
next_dir_left(north, west).  next_dir_left(west, south).
next_dir_left(south, east).  next_dir_left(east, north).


% --- EXTRACTION (CONVERSION ROBUSTE) ---

extract_state(BeliefsDict, State) :-
    Fluents = BeliefsDict.certain_fluents,
    get_dict(fat_hunter, Fluents, H), get_dict(c, H, Pos),
    safe_number(Pos.x, X), safe_number(Pos.y, Y),
    ( get_dict(dir, Fluents, DirList), member(DObj, DirList), get_dict(d, DObj, DirAtom) -> Dir = DirAtom ; Dir = north ),
    BaseState = [at(X,Y), facing(Dir)],

    ( get_dict(safeCells, BeliefsDict, SafeJSON) -> maplist(json_to_term(safe), SafeJSON, SafeTerms) ; SafeTerms = [] ),
    ( get_dict(pitCells, BeliefsDict, PitJSON) -> maplist(json_to_term(pit), PitJSON, PitTerms) ; PitTerms = [] ),
    ( get_dict(wumpusCells, BeliefsDict, WumpJSON) -> maplist(json_to_term(wumpus), WumpJSON, WumpTerms) ; WumpTerms = [] ),
    ( get_dict(visited, Fluents, VisJSON) -> maplist(extract_visited, VisJSON, VisTerms) ; VisTerms = [] ),
    
    append(BaseState, SafeTerms, S1), append(S1, PitTerms, S2), append(S2, WumpTerms, S3), append(S3, VisTerms, State).

safe_number(Val, Num) :- number(Val), !, Num = Val.
safe_number(Atom, Num) :- atom(Atom), atom_number(Atom, Num).

json_to_term(Functor, PointDict, Term) :-
    safe_number(PointDict.x, X), safe_number(PointDict.y, Y), Term =.. [Functor, X, Y].

extract_visited(Obj, visited(X,Y)) :-
    get_dict(to, Obj, To), safe_number(To.x, X), safe_number(To.y, Y).

update_fluents_dict(BeliefsDict, NewState, NewBeliefsDict) :-
    ( member(at(NX, NY), NewState), member(facing(NDir), NewState)
    -> 
       Fluents = BeliefsDict.certain_fluents,
       NewPos = c{x:NX, y:NY},
       (get_dict(fat_hunter, Fluents, OldH) -> put_dict(c, OldH, NewPos, NewH) ; NewH = fat{c:NewPos}),
       put_dict(fat_hunter, Fluents, NewH, F1),
       NewDirObj = d{d:NDir, h:hunter{id:hunter}},
       put_dict(dir, F1, [NewDirObj], F2),
       put_dict(certain_fluents, BeliefsDict, F2, NewBeliefsDict)
    ;  NewBeliefsDict = BeliefsDict ).


% --- CERVEAU (LOGIQUE) ---

:- begin_lpad.

% 1. OR
choose_action_prob(State, Percepts, grab, State) :-
    member(glitter, Percepts).

% 2. EXPLORATION AVEUGLE (Si pas de danger immédiat)
% Si cette règle échoue, le message DEBUG ne s'affichera pas
choose_action_prob(State, Percepts, move, NewState) :-
    \+ member(breeze, Percepts),
    \+ member(stench, Percepts),
    call(format(user_error, '   [LOGIC] No Percepts. Attempting Blind Move... ', [])),
    call(physics_move(State, NewState)).

% 3. EXPLORATION SÉCURISÉE (Si danger mais destination sûre)
choose_action_prob(State, _Percepts, move, NewState) :-
    call(format(user_error, '   [LOGIC] Percepts detected. Checking Safe Move... ', [])),
    call(physics_move(State, NewState)),
    member(at(NextX, NextY), NewState),
    member(safe(NextX, NextY), State),
    call(format(user_error, '   [LOGIC] Destination (~w,~w) is SAFE.~n', [NextX, NextY])).

% 4. DÉBLOCAGE
choose_action_prob(State, _Percepts, right, NewState) :-
    call(format(user_error, '   [LOGIC] Fallback to Turn Right.~n', [])),
    call(physics_turn_right(State, NewState)).

:- end_lpad.