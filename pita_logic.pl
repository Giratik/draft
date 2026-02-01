:- module(logic, [run_hunter/0]).

% --- IMPORTS ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(clpfd)).
:- use_module(library(pita)).

:- pita.

% CONFIGURATION CORS (Identique à agent_logic.pl)
:- set_setting(http:cors, [*]).
:- cors_enable.

% --- SERVEUR ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~n[SERVER] Hunter (PITA Brain) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

% ----------------------------------------------------------------------
% 1. GESTION PREFLIGHT (OPTIONS)
% Copié strictement depuis agent_logic.pl qui fonctionne
% ----------------------------------------------------------------------
handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

% ----------------------------------------------------------------------
% 2. GESTION REQUÊTE (PUT)
% ----------------------------------------------------------------------
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

    ( catch(prob(choose_action_safe(CurrentState, Percepts, A), Prob), Error, true)
    -> 
        ( var(Error) ->
            Action = A,
            format(user_error, '[OUT] DECISION: ~w (P=~2f)~n', [Action, Prob])
        ;
            format(user_error, '[CRASH] PITA Error: ~w~n', [Error]),
            Action = move
        )
    ;
        format(user_error, '[FAIL] No rule applied! Forcing RIGHT.~n', []),
        Action = right
    ),

    Response = _{
        hunterState: _{ beliefs: BeliefsDict, percepts: Percepts },
        action: Action
    },
    cors_enable,
    reply_json_dict(Response).


% --- MOTEUR PROBABILISTE (LPAD) ---

:- begin_lpad.

% A. Priors (Probabilités de world.pl)
pit(X,Y):0.2 :- valid_grid(X,Y), \+ start_pos(X,Y).
wumpus(X,Y):0.07 :- valid_grid(X,Y), \+ start_pos(X,Y).

% B. Causalité
breeze(X,Y) :- neighbor(X,Y, NX,NY), pit(NX,NY).
stench(X,Y) :- neighbor(X,Y, NX,NY), wumpus(NX,NY).

% C. Définitions
safe(X,Y) :- \+ pit(X,Y), \+ wumpus(X,Y).

start_pos(1,1).
valid_grid(X,Y) :- member(X, [0,1,2,3,4,5]), member(Y, [0,1,2,3,4,5]).

neighbor(X,Y, NX,NY) :-
    valid_grid(X,Y),
    (NX is X+1, NY is Y ; NX is X-1, NY is Y ; NX is X, NY is Y+1 ; NX is X, NY is Y-1),
    valid_grid(NX,NY).

:- end_lpad.


% --- UTILITAIRES ---

get_front_cell(X, Y, north, X, NY) :- NY is Y + 1.
get_front_cell(X, Y, south, X, NY) :- NY is Y - 1.
get_front_cell(X, Y, east, NX, Y)  :- NX is X + 1.
get_front_cell(X, Y, west, NX, Y)  :- NX is X - 1.

build_evidence(X, Y, Percepts, VisitedList, Evidence) :-
    ( member(breeze, Percepts) -> BFact = breeze(X,Y) ; BFact = \+ breeze(X,Y) ),
    ( member(stench, Percepts) -> SFact = stench(X,Y) ; SFact = \+ stench(X,Y) ),
    findall(\+ pit(VX, VY), member([VX, VY], VisitedList), PFacts),
    findall(\+ wumpus(Wx, Wy), member([Wx, Wy], VisitedList), WFacts),
    append([BFact, SFact | PFacts], WFacts, Evidence).

extract_pos_dir(BeliefsDict, X, Y, Dir, Visited) :-
    ( get_dict(certain_fluents, BeliefsDict, F) -> true ; F = _{} ),
    ( get_dict(fat_hunter, F, H), get_dict(c, H, Pos), safe_num(Pos.x, X), safe_num(Pos.y, Y) -> true ; X=1, Y=1 ),
    ( get_dict(dir, F, DL), member(D, DL), get_dict(d, D, DA) -> Dir = DA ; Dir = north ),
    ( get_dict(visited, F, VL) -> maplist(ex_vis, VL, Visited) ; Visited = [] ).

safe_num(A, N) :- atom(A), atom_number(A, N), !.
safe_num(N, N) :- number(N).
ex_vis(Obj, [X, Y]) :- get_dict(to, Obj, T), safe_num(T.x, X), safe_num(T.y, Y).