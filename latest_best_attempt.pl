#!/usr/bin/env swipl

:- module(hunter_server, [run_hunter/0]).

% ==============================================================================
% 1. IMPORTATION DES MODULES
% ==============================================================================
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).
:- use_module(library(clpfd)). 
:- use_module(library(pita)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% ==============================================================================
% 2. CONFIGURATION PITA
% ==============================================================================
:- pita.
:- begin_lpad.

% --- Monde Dynamique ---
valid_grid(X, Y, Size) :- 
    Limit is Size + 1,
    between(0, Limit, X),
    between(0, Limit, Y).

start_pos(1,1).

% --- Priors Dynamiques ---
pit(X,Y, Size):Prob :- 
    valid_grid(X,Y, Size), \+ start_pos(X,Y),
    Prob is 0.75 / Size.

wumpus(X,Y, Size):Prob :- 
    valid_grid(X,Y, Size), \+ start_pos(X,Y),
    Prob is 0.25 / Size.

% --- Causalité ---
breeze(X, Y, Size) :- adjacent(X, Y, NX, NY, Size), pit(NX, NY, Size).
stench(X, Y, Size) :- adjacent(X, Y, NX, NY, Size), wumpus(NX, NY, Size).
safe(X,Y, Size)    :- \+ pit(X,Y, Size), \+ wumpus(X,Y, Size).

adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X,     NY #= Y + 1.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X,     NY #= Y - 1.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X + 1, NY #= Y.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X - 1, NY #= Y.

:- end_lpad.

% ==============================================================================
% 3. SERVEUR HTTP
% ==============================================================================
:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).

run_hunter :-
    catch(http_stop_server(8081, []), _, true),
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~N~n[SERVER] Hunter Agent (Clean JSON) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

% --- A. GESTION DU PRE-FLIGHT (OPTIONS) ---
% On laisse reply_json_dict envoyer une réponse vide mais correcte avec les headers CORS
handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    reply_json_dict(_{}). 

% --- B. GESTION DE L'ACTION (PUT) ---
handle_hunter_request(Request) :-
    % 1. Activer CORS tout de suite
    cors_enable(Request, [methods([put])]),
    
    % 2. Protéger tout le traitement par un catch
    catch(
        handle_request_logic(Request),
        Error,
        handle_error(Error)
    ).

% Gestionnaire d'erreurs qui répond en JSON propre (évite le crash I/O)
handle_error(Error) :-
    format(user_error, '~N[CRITICAL ERROR] ~w~n', [Error]),
    term_string(Error, Str),
    % On force le status 500 via l'option de la librairie, pas manuellement
    reply_json_dict(_{status: "error", message: Str}, [status(500)]).

% --- C. LOGIQUE PRINCIPALE ---
handle_request_logic(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    untag(RequestJSON, CleanJSON),
    
    Beliefs  = CleanJSON.get(beliefs, _{}),
    Percepts = CleanJSON.get(percepts, []),
    
    % Extraction
    extract_state(Beliefs, X, Y, Dir, Visited, Size),

    % Mémoire
    OldHistory = Beliefs.get(percept_history, []),
    
    ( integer(X) -> XInt=X ; XInt=1 ), 
    ( integer(Y) -> YInt=Y ; YInt=1 ),
    CurrentMemory = _{x:XInt, y:YInt, percepts:Percepts},
    
    exclude(match_pos(XInt,YInt), OldHistory, CleanHistory),
    NewHistory = [CurrentMemory | CleanHistory],
    
    format(user_error, '~N~n--- TOUR (Size ~w) --- Pos: ~w,~w ---~n', [Size, XInt, YInt]),

    % Décision
    ( decide_action(XInt, YInt, Dir, Visited, NewHistory, Percepts, Size, ComputedAction)
    -> 
        Action = ComputedAction,
        format(user_error, '  -> Action choisie: ~w~n', [Action])
    ; 
        Action = move,
        format(user_error, '  [ALERTE] decide_action echec. Repli sur move.~n', [])
    ),
    
    % Réponse
    NewBeliefs = Beliefs.put(percept_history, NewHistory),
    
    Response = _{
        hunterState: _{ beliefs: NewBeliefs, percepts: Percepts },
        action: Action
    },

    % Envoi final standard
    reply_json_dict(Response).


safe_int(Val, Int) :- integer(Val) -> Int = Val ; fd_size(Val, sup) -> Int = 1 ; Val #= Int.
match_pos(X, Y, Entry) :- Entry.x == X, Entry.y == Y.

% ==============================================================================
% 4. STRATEGIE (CORRIGÉ)
% ==============================================================================

decide_action(_, _, _, _, _, Percepts, _, grab) :-
    member(glitter, Percepts), 
    format(user_error, '  -> REFLEX: Glitter found!~n', []), !.

decide_action(_, _, _, _, _, Percepts, _, right) :-
    member(bump, Percepts), 
    format(user_error, '  -> REFLEX: Bump! Turning right.~n', []), !.

decide_action(X, Y, Dir, Visited, History, _, Size, Action) :-
    build_evidence(History, Visited, Size, EvidenceConj),
    
    get_front_cell(X, Y, Dir, FrontX, FrontY),
    evaluate_cell(FrontX, FrontY, EvidenceConj, Visited, Size, ScoreFront),
    
    next_dir_right(Dir, RightDir),
    get_front_cell(X, Y, RightDir, RightX, RightY),
    evaluate_cell(RightX, RightY, EvidenceConj, Visited, Size, ScoreRight),
    
    next_dir_left(Dir, LeftDir),
    get_front_cell(X, Y, LeftDir, LeftX, LeftY),
    evaluate_cell(LeftX, LeftY, EvidenceConj, Visited, Size, ScoreLeft),

    format(user_error, '  [SCORES] F:~w | R:~w | L:~w~n', [ScoreFront, ScoreRight, ScoreLeft]),
    choose_best(ScoreFront, ScoreRight, ScoreLeft, Action).

choose_best(F, R, L, left)  :- L > F, L > R, !.
choose_best(F, R, _, right) :- R > F, !.
choose_best(F, _, _, move)  :- F > 0, !.
choose_best(_, _, _, right).

evaluate_cell(X, Y, _, _, Size, 0) :-
    \+ is_valid_coordinate(X, Y, Size), !.

evaluate_cell(X, Y, Evidence, Visited, Size, Score) :-
    ( prob((safe(X, Y, Size), Evidence), P_Joint),
      prob(Evidence, P_Ev),
      P_Ev > 0.000001
    -> P is P_Joint / P_Ev
    ;  P = 0.5 ),
    
    format(user_error, '   > Cell (~w,~w) P(Safe)=~2f ', [X, Y, P]),
    ( P > 0.8 -> 
        ( member([X, Y], Visited) -> 
            Score = 1, format(user_error, '[OK-VU]~n', []) % FIX: format(user_error, ...)
        ; 
            Score = 2, format(user_error, '[OK-NEW]~n', []) % FIX: format(user_error, ...)
        )
    ; 
        Score = 0, format(user_error, '[DANGER]~n', []) % FIX: format(user_error, ...)
    ).

% ==============================================================================
% 5. CONSTRUCTION PREUVES
% ==============================================================================
build_evidence(History, Visited, Size, EvidenceConj) :-
    maplist(history_to_evidence(Size), History, EvidenceLists),
    flatten(EvidenceLists, HistoricalEvidence),
    findall(\+ pit(Vx, Vy, Size), member([Vx, Vy], Visited), SafePits),
    findall(\+ wumpus(Wx, Wy, Size), member([Wx, Wy], Visited), SafeWumpus),
    append(HistoricalEvidence, SafePits, T1),
    append(T1, SafeWumpus, FullList),
    list_to_conj(FullList, EvidenceConj).

history_to_evidence(Size, Entry, [StenchEv, BreezeEv]) :-
    X = Entry.x, Y = Entry.y, P = Entry.percepts,
    ( is_list(P), member(stench, P) -> StenchEv = stench(X, Y, Size) 
    ; StenchEv = \+(stench(X, Y, Size)) ),
    ( is_list(P), member(breeze, P) -> BreezeEv = breeze(X, Y, Size) 
    ; BreezeEv = \+(breeze(X, Y, Size)) ).

% ==============================================================================
% 6. UTILITAIRES
% ==============================================================================
extract_state(Beliefs, X, Y, Dir, VisitedCoordinates, Size) :-
    Fluents = Beliefs.get(certain_fluents, _{}),
    H = Fluents.get(fat_hunter, _{}), C = H.get(c, _{}),
    
    RawX = C.get(x, 1), 
    RawY = C.get(y, 1),
    RawSize = Beliefs.get(gridSize, 4),

    X is round(RawX), 
    Y is round(RawY), 
    Size is round(RawSize),

    ( get_dict(dir, Fluents, DL), member(DirObj, DL), get_dict(d, DirObj, D) -> Dir = D ; Dir = north ),

    ( get_dict(visited, Fluents, VL) -> 
        findall([VX, VY], (member(VObj, VL), get_dict(to, VObj, VTo), VX = VTo.x, VY = VTo.y), VisitedCoordinates)
    ; VisitedCoordinates = [] ).

get_front_cell(X, Y, north, FX, FY) :- FX #= X,     FY #= Y + 1.
get_front_cell(X, Y, south, FX, FY) :- FX #= X,     FY #= Y - 1.
get_front_cell(X, Y, east,  FX, FY) :- FX #= X + 1, FY #= Y.
get_front_cell(X, Y, west,  FX, FY) :- FX #= X - 1, FY #= Y.

next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).
next_dir_left(north, west). next_dir_left(west, south).
next_dir_left(south, east). next_dir_left(east, north).

is_valid_coordinate(X, Y, Size) :- Limit is Size, X >= 1, X =< Limit, Y >= 1, Y =< Limit.

list_to_conj([], true).
list_to_conj([H], H) :- !.
list_to_conj([H|T], (H, R)) :- list_to_conj(T, R).

untag(D, DOut) :- is_dict(D), !, dict_pairs(D, _, P), maplist(untag_pair, P, NP), dict_create(DOut, _, NP).
untag(L, LOut) :- is_list(L), !, maplist(untag, L, LOut).
untag(V, V).
untag_pair(K-V, K-VO) :- untag(V, VO).