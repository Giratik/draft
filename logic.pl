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
    format(user_error, '~n[SERVER] Hunter Agent (Smart) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    format(user_error, '~n---------------------------------------------------~n', []),
    
    ( catch(extract_state(BeliefsDict, CurrentState), _, fail)
    -> true 
    ;  CurrentState = [at(1,1), facing(north)], format(user_error, '[WARN] Extraction fail~n', [])
    ),
    format(user_error, '[STATE] ~w~n', [CurrentState]),

    ( catch(prob(choose_action_prob(CurrentState, Percepts, A, NS), Prob), Error, true)
    -> 
        ( var(Error) ->
            Action = A, NewState = NS,
            format(user_error, '[DECISION] Action: ~w (P=~2f)~n', [Action, Prob]),
            format(atom(DebugMsg), 'Act: ~w', [Action])
        ;
            format(user_error, '[CRASH PITA] ~w~n', [Error]),
            Action = move, NewState = CurrentState, DebugMsg = 'Crash'
        )
    ;
        format(user_error, '[FAIL] Force Right.~n', []),
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
    
    % On limite à 0..3 (taille standard 4x4) pour éviter de croire qu'on peut sortir
    % Si votre grille est plus grande, augmentez à 0..5 ou 0..10
    NextX in 0..3, NextY in 0..3, 
    label([NextX, NextY]),
    select(at(X,Y), State, at(NextX, NextY), NewState).

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


% --- EXTRACTION ---

extract_state(BeliefsDict, State) :-
    Fluents = BeliefsDict.certain_fluents,
    get_dict(fat_hunter, Fluents, H), get_dict(c, H, Pos),
    safe_number(Pos.x, X), safe_number(Pos.y, Y),
    ( get_dict(dir, Fluents, DirList), member(DObj, DirList), get_dict(d, DObj, DirAtom) -> Dir = DirAtom ; Dir = north ),
    BaseState = [at(X,Y), facing(Dir)],

    ( get_dict(safeCells, BeliefsDict, SafeJSON) -> maplist(json_to_term(safe), SafeJSON, SafeTerms) ; SafeTerms = [] ),
    ( get_dict(pitCells, BeliefsDict, PitJSON) -> maplist(json_to_term(pit), PitJSON, PitTerms) ; PitTerms = [] ),
    ( get_dict(wumpusCells, BeliefsDict, WumpJSON) -> maplist(json_to_term(wumpus), WumpJSON, WumpTerms) ; WumpTerms = [] ),
    % Ajout des cases visitées pour l'IA
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


% --- CERVEAU (RÈGLES DE DÉCISION) ---

:- begin_lpad.

% 1. PRIORITÉ ABSOLUE : OR
choose_action_prob(State, Percepts, grab, State) :-
    member(glitter, Percepts).

% 2. PRIORITÉ : MUR (BUMP)
% Si on a tapé un mur, on tourne (sinon on reste bloqué)
choose_action_prob(State, Percepts, right, NewState) :-
    member(bump, Percepts),
    call(physics_turn_right(State, NewState)).

% 3. EXPLORATION (MOVE)

% CAS A : Aucun danger ressenti ICI (Pas de Breeze, Pas de Stench)
% => On peut avancer vers n'importe quelle case valide de la grille !
choose_action_prob(State, Percepts, move, NewState) :-
    \+ member(breeze, Percepts),
    \+ member(stench, Percepts),
    call(physics_move(State, NewState)),
    % OPTIONNEL : On préfère ne pas revenir sur nos pas (décommenter si besoin)
    % member(at(NX, NY), NewState), \+ member(visited(NX, NY), State).
    true.

% CAS B : Danger ressenti, mais destination CONNUE SAFE
choose_action_prob(State, _Percepts, move, NewState) :-
    call(physics_move(State, NewState)),
    member(at(NextX, NextY), NewState),
    member(safe(NextX, NextY), State).

% 4. DÉBLOCAGE (ROTATION)
% Si on ne peut pas avancer (Mur ou Danger devant), on tourne pour chercher une autre voie.

% Essayer Droite
choose_action_prob(State, _Percepts, right, NewState) :-
    call(physics_turn_right(State, NewState)).

% Essayer Gauche
choose_action_prob(State, _Percepts, left, NewState) :-
    call(physics_turn_left(State, NewState)).

:- end_lpad.