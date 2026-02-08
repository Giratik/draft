#!/usr/bin/env swipl

:- module(hunter_server, [run_hunter/0]).

% ==============================================================================
% 1. LIBRARIES
% ==============================================================================
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).
:- use_module(library(clpfd)). 
:- use_module(library(pita)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(reif)). 
:- use_module(library(option)).

% ==============================================================================
% 2. BACKGROUND KNOWLEDGE (Deterministic & CLP(FD))
% ==============================================================================

% --- Bridges for reif ---
bool_to_t(1, true).
bool_to_t(0, false).

fd_gt_t(X, Y, T) :- X #> Y #<==> B, bool_to_t(B, T).
member_t(Elem, List, T) :- memberd_t(Elem, List, T).

% --- World Logic (Pure Prolog/CLP) ---
valid_grid(X, Y, Size) :- 
    X #>= 1, X #=< Size,
    Y #>= 1, Y #=< Size.

start_pos(1, 1).

adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X,     NY #= Y + 1.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X,     NY #= Y - 1.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X + 1, NY #= Y.
adjacent(X, Y, NX, NY, Size) :- valid_grid(NX, NY, Size), NX #= X - 1, NY #= Y.

% --- Reified Helper for Decision Logic ---
valid_grid_t(X, Y, Size, T) :- 
    X #>= 1 #<==> B1,
    X #=< Size #<==> B2,
    Y #>= 1 #<==> B3,
    Y #=< Size #<==> B4,
    B1 + B2 + B3 + B4 #= 4 #<==> BAll,
    bool_to_t(BAll, T).

% ==============================================================================
% 3. BRIDGE PREDICATES FOR PITA
% ==============================================================================
% PITA transforms predicates in the lpad block to have 2 extra arguments 
% (EnvironmentIn, EnvironmentOut). We must provide these signatures for 
% background predicates so PITA can call them. We simply pass P through.

valid_grid(X, Y, Size, P, P) :- valid_grid(X, Y, Size).

start_pos(X, Y, P, P) :- start_pos(X, Y).

adjacent(X, Y, NX, NY, Size, P, P) :- adjacent(X, Y, NX, NY, Size).

% ==============================================================================
% 4. PROBABILISTIC LOGIC (LPAD)
% ==============================================================================
:- pita.
:- begin_lpad.

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

:- end_lpad.

% ==============================================================================
% 5. DECISION STRATEGY
% ==============================================================================

decide_action(X, Y, Dir, Visited, History, Percepts, Size, HasGold, Action) :-
    if_(member_t(glitter, Percepts), 
        (format(user_error, '  -> REFLEX: Gold Found!~n', []), Action = grab),
        if_(member_t(bump, Percepts), 
            (format(user_error, '  -> REFLEX: Bump!~n', []), Action = right), 
            choose_directional_move(X, Y, Dir, Visited, History, Size, HasGold, Action))).

choose_directional_move(X, Y, Dir, Visited, History, Size, HasGold, Action) :-
    build_evidence(History, Visited, Size, Evidence),
    get_front_cell(X, Y, Dir, FX, FY),
    evaluate_utility(FX, FY, Evidence, Visited, Size, HasGold, UF),
    next_dir_right(Dir, RDir),
    get_front_cell(X, Y, RDir, RX, RY),
    evaluate_utility(RX, RY, Evidence, Visited, Size, HasGold, UR),
    next_dir_left(Dir, LDir),
    get_front_cell(X, Y, LDir, LX, LY),
    evaluate_utility(LX, LY, Evidence, Visited, Size, HasGold, UL),
    format(user_error, '  [SCORES] Front: ~w, Right: ~w, Left: ~w~n', [UF, UR, UL]),
    
    % Deterministic decision based on scores
    if_(fd_gt_t(UL, UF), 
        if_(fd_gt_t(UL, UR), Action = left, Action = right),
        if_(fd_gt_t(UR, UF), Action = right, Action = move)).

evaluate_utility(X, Y, Evidence, Visited, Size, HasGold, Utility) :-
    if_(valid_grid_t(X, Y, Size),
        if_(is_safe_t(X, Y, Evidence, Size),
            calculate_goal_utility(X, Y, Visited, HasGold, Utility),
            Utility #= 0),
        Utility #= 0).

is_safe_t(X, Y, Evidence, Size, T) :-
    ( prob((safe(X, Y, Size), Evidence), P_Joint),
      prob(Evidence, P_Ev),
      P_Ev > 0.00001
    -> P is P_Joint / P_Ev
    ;  P = 0.5 ),
    PInt is round(P * 100),
    format(user_error, '   > Cell (~w,~w) P(Safe)=~2f ', [X, Y, P]),
    if_(fd_gt_t(PInt, 75), 
        (format(user_error, '[SAFE]~n', []), T = true), 
        (format(user_error, '[DANGER]~n', []), T = false)).

calculate_goal_utility(X, Y, Visited, HasGold, Utility) :-
    if_(HasGold = true,
        (Dist #= abs(X - 1) + abs(Y - 1), Utility #= 100 - Dist),
        if_(member_t([X, Y], Visited), Utility #= 10, Utility #= 50)).

get_front_cell(X, Y, north, X, FY) :- FY #= Y + 1.
get_front_cell(X, Y, south, X, FY) :- FY #= Y - 1.
get_front_cell(X, Y, east,  FX, Y) :- FX #= X + 1.
get_front_cell(X, Y, west,  FX, Y) :- FX #= X - 1.

next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).
next_dir_left(north, west).  next_dir_left(west, south).
next_dir_left(south, east).  next_dir_left(east, north).

% ==============================================================================
% 6. INFRASTRUCTURE & HANDLERS
% ==============================================================================

build_evidence(History, Visited, Size, EvidenceConj) :-
    maplist(history_to_evidence(Size), History, EvidenceLists),
    flatten(EvidenceLists, HistoricalEvidence),
    findall(\+ pit(Vx, Vy, Size), member([Vx, Vy], Visited), SafePits),
    findall(\+ wumpus(Wx, Wy, Size), member([Wx, Wy], Visited), SafeWumps),
    append([HistoricalEvidence, SafePits, SafeWumps], FullList),
    list_to_conj(FullList, EvidenceConj).

history_to_evidence(Size, Entry, [S, B]) :-
    X is round(Entry.x), Y is round(Entry.y), P = Entry.percepts,
    if_(member_t(stench, P), S = stench(X, Y, Size), S = \+(stench(X, Y, Size))),
    if_(member_t(breeze, P), B = breeze(X, Y, Size), B = \+(breeze(X, Y, Size))).

list_to_conj([], true).
list_to_conj([H|T], Conj) :- list_to_conj_rec(T, H, Conj).
list_to_conj_rec([], Acc, Acc).
list_to_conj_rec([H|T], Acc, (H, Rest)) :- list_to_conj_rec(T, Acc, Rest).

:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).

handle_hunter_request(Request) :-
    option(method(Method), Request),
    handle_method(Method, Request).

handle_method(options, Request) :-
    cors_enable(Request, [methods([put, options])]),
    reply_json_dict(_{}).

handle_method(put, Request) :-
    cors_enable(Request, [methods([put, options])]),
    catch(
        handle_put_logic(Request),
        Error,
        (
            format(user_error, '[CRITICAL ERROR] ~w~n', [Error]),
            reply_json_dict(_{error: "Internal Server Error"}, [status(500)])
        )
    ).

handle_put_logic(Request) :-
    format(user_error, '~N~n--- [HTTP] New Request Received ---~n', []),
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    untag(RequestJSON, CleanJSON),
    
    Beliefs = CleanJSON.get(beliefs, _{}),
    Percepts = CleanJSON.get(percepts, []),
    
    extract_state(Beliefs, X, Y, Dir, Visited, Size, HasGold),
    format(user_error, '--- TOUR (Size ~w) --- Pos: ~w,~w ---~n', [Size, X, Y]),
    
    OldHistory = Beliefs.get(percept_history, []),
    CurrentMemory = _{x:X, y:Y, percepts:Percepts},
    NewHistory = [CurrentMemory | OldHistory],
    
    decide_action(X, Y, Dir, Visited, NewHistory, Percepts, Size, HasGold, Action),
    format(user_error, '  -> Decision: ~w~n', [Action]),
    
    Response = _{
        hunterState: _{ beliefs: Beliefs.put(percept_history, NewHistory), percepts: Percepts },
        action: Action
    },
    reply_json_dict(Response).

handle_method(Method, _) :-
    format(user_error, '[WARNING] Unknown method: ~w~n', [Method]),
    reply_json_dict(_{error: "Method not allowed"}, [status(405)]).

extract_state(Beliefs, X, Y, Dir, Visited, Size, HasGold) :-
    Fluents = Beliefs.get(certain_fluents, _{}),
    H = Fluents.get(fat_hunter, _{}), C = H.get(c, _{}),
    X is round(C.get(x, 1)), 
    Y is round(C.get(y, 1)), 
    Size is round(Beliefs.get(gridSize, 4)),
    (get_dict(has_gold, Fluents, _) -> HasGold = true ; HasGold = false),
    (get_dict(dir, Fluents, DL), member(DirObj, DL), get_dict(d, DirObj, D) -> Dir = D ; Dir = north),
    (get_dict(visited, Fluents, VL) -> 
        findall([VX, VY], (member(VObj, VL), get_dict(to, VObj, VTo), VX is round(VTo.x), VY is round(VTo.y)), Visited)
    ; Visited = []).

untag(D, DOut) :- is_dict(D), !, dict_pairs(D, _, P), maplist(untag_pair, P, NP), dict_create(DOut, _, NP).
untag(L, LOut) :- is_list(L), !, maplist(untag, L, LOut).
untag(V, V).
untag_pair(K-V, K-VO) :- untag(V, VO).

run_hunter :-
    catch(http_stop_server(8081, []), _, true),
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '--- Hunter Server Online on 8081 ---~n', []).

:- http_handler(root(action), handle_hunter_request, []).