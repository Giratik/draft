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
    format(user_error, '~n[SERVER] Hunter Agent (FIXED) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    % Logs
    format(user_error, '~n---------------------------------------------------~n', []),
    
    % Extraction
    ( catch(extract_state(BeliefsDict, CurrentState), _, fail)
    -> true 
    ;  CurrentState = [at(1,1), facing(north)], format(user_error, '[WARN] Extraction fail~n', [])
    ),
    format(user_error, '[STATE] ~w~n', [CurrentState]),

    % Décision PITA
    ( catch(prob(choose_action_prob(CurrentState, Percepts, A, NS), Prob), Error, true)
    -> 
        ( var(Error) ->
            Action = A, NewState = NS,
            format(user_error, '[DECISION] Action: ~w (P=~2f) -> Vers: ~w~n', [Action, Prob, NewState]),
            format(atom(DebugMsg), 'Act: ~w', [Action])
        ;
            format(user_error, '[CRASH PITA] ~w~n', [Error]),
            Action = move, NewState = CurrentState, DebugMsg = 'Crash'
        )
    ;
        % Si tout échoue, on force un mouvement aléatoire pour débloquer
        format(user_error, '[FAIL] Aucune regle ! Force Right.~n', []),
        Action = right, NewState = CurrentState, DebugMsg = 'Stuck'
    ),

    update_fluents_dict(BeliefsDict, NewState, UpdatedBeliefsDict),
    Response = _{
        hunterState: _{ beliefs: UpdatedBeliefsDict, percepts: Percepts },
        action: Action,
        debug: DebugMsg
    },
    cors_enable,
    reply_json_dict(Response).


% --- MOTEUR PHYSIQUE (CLP) ---

physics_move(State, NewState) :-
    member(at(X,Y), State),
    member(facing(Dir), State),
    
    delta(Dir, DX, DY),
    NextX #= X + DX,
    NextY #= Y + DY,
    
    % FIX: Domaine large pour accepter 0..3 ou 1..4 sans planter
    NextX in 0..10, NextY in 0..10,
    label([NextX, NextY]),
    
    select(at(X,Y), State, at(NextX, NextY), NewState).

physics_turn_right(State, NewState) :-
    member(facing(Dir), State),
    next_dir_right(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

physics_turn_left(State, NewState) :-
    member(facing(Dir), State),
    next_dir_left(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

delta(north, 0, 1).   delta(south, 0, -1).
delta(east, 1, 0).    delta(west, -1, 0).

next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).

next_dir_left(north, west).  next_dir_left(west, south).
next_dir_left(south, east).  next_dir_left(east, north).


% --- EXTRACTION (CONVERSION STRICTE DES NOMBRES) ---

extract_state(BeliefsDict, State) :-
    Fluents = BeliefsDict.certain_fluents,

    % 1. Position & Orientation
    get_dict(fat_hunter, Fluents, H), get_dict(c, H, Pos),
    safe_number(Pos.x, X), safe_number(Pos.y, Y), % Utilisation de safe_number

    ( get_dict(dir, Fluents, DirList), member(DObj, DirList), get_dict(d, DObj, DirAtom)
    -> Dir = DirAtom ; Dir = north ),

    BaseState = [at(X,Y), facing(Dir)],

    % 2. Listes (Safe, Pit, etc.)
    ( get_dict(safeCells, BeliefsDict, SafeJSON) -> maplist(json_to_term(safe), SafeJSON, SafeTerms) ; SafeTerms = [] ),
    ( get_dict(pitCells, BeliefsDict, PitJSON) -> maplist(json_to_term(pit), PitJSON, PitTerms) ; PitTerms = [] ),
    ( get_dict(wumpusCells, BeliefsDict, WumpJSON) -> maplist(json_to_term(wumpus), WumpJSON, WumpTerms) ; WumpTerms = [] ),
    
    append(BaseState, SafeTerms, S1),
    append(S1, PitTerms, S2),
    append(S2, WumpTerms, State).

% FIX: Force la conversion en nombre (même si c'est un atome '1')
safe_number(Val, Num) :- number(Val), !, Num = Val.
safe_number(Atom, Num) :- atom(Atom), atom_number(Atom, Num).

% Utilitaire de conversion liste
json_to_term(Functor, PointDict, Term) :-
    safe_number(PointDict.x, X), 
    safe_number(PointDict.y, Y),
    Term =.. [Functor, X, Y].

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


% --- CERVEAU LPAD ---

:- begin_lpad.

% 1. OR
choose_action_prob(State, Percepts, grab, State) :-
    member(glitter, Percepts).

% 2. MOVE (Si safe devant)
choose_action_prob(State, _Percepts, move, NewState) :-
    call(physics_move(State, NewState)),
    member(at(NextX, NextY), NewState),
    member(safe(NextX, NextY), State).

% 3. ROTATION INTELLIGENTE (Si safe sur les côtés)
% Si safe à DROITE (relativement), tourner à droite
choose_action_prob(State, _Percepts, right, NewState) :-
    call(physics_turn_right(State, NewState)), % On simule le tour
    member(facing(NewDir), NewState),          % On regarde la nouvelle direction
    member(at(X,Y), State),                    % On regarde où on est
    % Si on avançait dans cette nouvelle direction...
    call(delta(NewDir, DX, DY)),
    TargetX #= X + DX, TargetY #= Y + DY, call(label([TargetX, TargetY])),
    % ... est-ce que ce serait safe ?
    member(safe(TargetX, TargetY), State).

% Si safe à GAUCHE, tourner à gauche
choose_action_prob(State, _Percepts, left, NewState) :-
    call(physics_turn_left(State, NewState)),
    member(facing(NewDir), NewState),
    member(at(X,Y), State),
    call(delta(NewDir, DX, DY)),
    TargetX #= X + DX, TargetY #= Y + DY, call(label([TargetX, TargetY])),
    member(safe(TargetX, TargetY), State).

% 4. FALLBACK (Si coincé, tourner à droite par défaut)
choose_action_prob(State, _Percepts, right, NewState) :-
    call(physics_turn_right(State, NewState)).

:- end_lpad.