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

% ==============================================================================
% 2. CONFIGURATION PITA (Modèle Probabiliste)
% ==============================================================================
:- pita.
:- begin_lpad.

% --- Monde ---
valid_grid(X,Y) :- member(X, [0,1,2,3,4,5]), member(Y, [0,1,2,3,4,5]).
start_pos(1,1).

% --- Priors ---
pit(X,Y):0.2     :- valid_grid(X,Y), \+ start_pos(X,Y).
wumpus(X,Y):0.05 :- valid_grid(X,Y), \+ start_pos(X,Y).

% --- Causalité ---
breeze(X, Y) :- adjacent(X, Y, NX, NY), pit(NX, NY).
stench(X, Y) :- adjacent(X, Y, NX, NY), wumpus(NX, NY).

% --- Sécurité ---
safe(X,Y) :- \+ pit(X,Y), \+ wumpus(X,Y).

% --- Adjacence ---
adjacent(X, Y, NX, NY) :- valid_grid(NX, NY), NX #= X,     NY #= Y + 1.
adjacent(X, Y, NX, NY) :- valid_grid(NX, NY), NX #= X,     NY #= Y - 1.
adjacent(X, Y, NX, NY) :- valid_grid(NX, NY), NX #= X + 1, NY #= Y.
adjacent(X, Y, NX, NY) :- valid_grid(NX, NY), NX #= X - 1, NY #= Y.

:- end_lpad.

% ==============================================================================
% 3. SERVEUR HTTP
% ==============================================================================
:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).

run_hunter :-
    catch(http_stop_server(8081, []), _, true),
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~N~n[SERVER] Hunter Agent (Smart Explorer) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

% In-memory memory for percepts and contradictions
:- dynamic percepts_at/3.         % percepts_at(X,Y,PerceptsList)
:- dynamic contradiction/5.       % contradiction(X1,Y1,X2,Y2,Reason)

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    cors_enable,
    catch(
        http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
        Error,
        (format(user_error, '[ERREUR] JSON invalide: ~w~n', [Error]), fail)
    ),

    untag(RequestJSON, CleanJSON),
    Beliefs  = CleanJSON.get(beliefs, _{}),
    Percepts = CleanJSON.get(percepts, []),
    
    extract_state(Beliefs, X, Y, Dir, Visited),

    % Record current percepts in local memory and run triangulation
    update_memory(X, Y, Percepts),
    triangulate_memory,
    findall(_{x:X1, y:Y1, x2:X2, y2:Y2, reason:Reason}, contradiction(X1,Y1,X2,Y2,Reason), Contradictions),
    % Log contradictions for debugging
    ( Contradictions = [] -> true ; format(user_error, '[DEBUG] Contradictions: ~w~n', [Contradictions]) ),

    format(user_error, '~N~n================================================~n', []),
    format(user_error, '          [HUNTER ACTION REQUEST]               ~n', []),
    format(user_error, '------------------------------------------------~n', []),
    format(user_error, '  POS: ~w,~w (~w)~n', [X, Y, Dir]),
    format(user_error, '  PERCEPTS: ~w~n', [Percepts]),
    format(user_error, '================================================~n', []),

    decide_action(X, Y, Dir, Visited, Percepts, Action),
    
    Response = _{
        hunterState: _{ beliefs: Beliefs, percepts: Percepts },
        action: Action,
        contradictions: Contradictions
    },
    reply_json_dict(Response).


% ==============================================================================
% 4. CERVEAU (Stratégie d'Exploration)
% ==============================================================================

% Règle 1: OR -> GRAB
decide_action(_, _, _, _, Percepts, grab) :-
    member(glitter, Percepts),
    format(user_error, '  -> DECISION: GRAB (Or!)~n', []), !.

% Règle 2: MUR -> RIGHT
decide_action(_, _, _, _, Percepts, right) :-
    member(bump, Percepts),
    format(user_error, '  -> DECISION: RIGHT (Mur)~n', []), !.

% Règle 3: Comparaison des options (Devant vs Droite)
decide_action(X, Y, Dir, Visited, Percepts, Action) :-
    % A. Construire les preuves PITA
    build_evidence(X, Y, Percepts, Visited, EvidenceConj),
    
    % B. Evaluer le score de la case DEVANT
    get_front_cell(X, Y, Dir, FrontX, FrontY),
    evaluate_cell(FrontX, FrontY, EvidenceConj, Visited, ScoreFront),
    
    % C. Evaluer le score de la case à DROITE (Simulation de rotation)
    next_dir_right(Dir, RightDir),
    get_front_cell(X, Y, RightDir, RightX, RightY),
    evaluate_cell(RightX, RightY, EvidenceConj, Visited, ScoreRight),
    
    % Evaluate left as well
    next_dir_left(Dir, LeftDir),
    get_front_cell(X, Y, LeftDir, LeftX, LeftY),
    evaluate_cell(LeftX, LeftY, EvidenceConj, Visited, ScoreLeft),

    format(user_error, '  [SCORES] Front(~w,~w)=~w | Right(~w,~w)=~w | Left(~w,~w)=~w~n', 
           [FrontX, FrontY, ScoreFront, RightX, RightY, ScoreRight, LeftX, LeftY, ScoreLeft]),

    % D. Prise de décision among three options
    ( ScoreLeft > ScoreFront, ScoreLeft > ScoreRight ->
        Action = left,
        format(user_error, '  -> DECISION: LEFT (Meilleure opportunité à gauche)~n', [])
    ; ScoreRight > ScoreFront ->
        Action = right,
        format(user_error, '  -> DECISION: RIGHT (Meilleure opportunité à droite)~n', [])
    ; ScoreFront > 0 ->
        Action = move,
        format(user_error, '  -> DECISION: MOVE (Devant est acceptable)~n', [])
    ;
        Action = right,
        format(user_error, '  -> DECISION: RIGHT (Cul-de-sac ou Danger devant)~n', [])
    ).


% ==============================================================================
% 5. EVALUATION (PITA + Heuristique)
% ==============================================================================

% evaluate_cell(+X, +Y, +Evidence, +Visited, -Score)
% Score 2 : Safe + Non visité (Priorité max)
% Score 1 : Safe + Déjà visité (Repli)
% Score 0 : Danger ou Mur (Interdit)

evaluate_cell(X, Y, _, _, 0) :-
    \+ is_valid_coordinate(X, Y), !. % Mur

evaluate_cell(X, Y, Evidence, Visited, Score) :-
    % 1. Calcul Probabiliste P(Safe)
    ( prob((safe(X, Y), Evidence), P_Joint),
      prob(Evidence, P_Ev),
      P_Ev > 0.000001
    ->
        P is P_Joint / P_Ev
    ;
        P = 0.5 % Incertitude
    ),
    
    ( P > 0.8 -> % Seuil de confiance 80%
        % La case est sûre, vérifions si elle est visitée
        ( member([X, Y], Visited) ->
             Score = 1 % Déjà vu (Ennuyeux mais safe)
        ;
             Score = 2 % Nouveau ! (Exploration)
        )
    ;
        Score = 0 % Trop dangereux
    ).


% ==============================================================================
% 6. UTILITAIRES
% ==============================================================================

extract_state(Beliefs, X, Y, Dir, VisitedCoordinates) :-
    Fluents = Beliefs.get(certain_fluents, _{}),
    H = Fluents.get(fat_hunter, _{}), C = H.get(c, _{}),
    RawX = C.get(x, 1), RawY = C.get(y, 1),
    X #= RawX, Y #= RawY,
    ( get_dict(dir, Fluents, DL), member(dir{d:D, h:_}, DL) -> Dir = D ; Dir = north ),
    ( get_dict(visited, Fluents, VL) -> 
        findall([VX, VY], (member(VObj, VL), get_dict(to, VObj, VTo), VX = VTo.x, VY = VTo.y), VisitedCoordinates)
    ; VisitedCoordinates = [] ).

build_evidence(X, Y, Percepts, Visited, EvidenceConj) :-
    ( member(breeze, Percepts) -> B = breeze(X,Y) ; B = (\+ breeze(X,Y)) ),
    ( member(stench, Percepts) -> S = stench(X,Y) ; S = (\+ stench(X,Y)) ),
    findall(\+ pit(Vx, Vy), member([Vx, Vy], Visited), SafePits),
    findall(\+ wumpus(Wx, Wy), member([Wx, Wy], Visited), SafeWumpus),
    
    % Add constraints from contradictions: cells adjacent to BOTH contradiction sources are safer
    findall(\+ pit(CX, CY), contradiction_safe_pit(CX, CY), ContradictionSafePits),
    findall(\+ wumpus(CX, CY), contradiction_safe_wumpus(CX, CY), ContradictionSafeWumpus),
    
    append([B, S | SafePits], SafeWumpus, BaseList),
    append(BaseList, ContradictionSafePits, List2),
    append(List2, ContradictionSafeWumpus, EvidenceList),
    list_to_conj(EvidenceList, EvidenceConj).

% If cell (X1,Y1) has stench and (X2,Y2) has breeze (stench_vs_breeze),
% then cells adjacent to BOTH cannot have a pit (because the breeze comes from somewhere else)
contradiction_safe_pit(CX, CY) :-
    contradiction(X1, Y1, X2, Y2, 'stench_vs_breeze'),
    % Check all 4 neighbors of (X1,Y1) that are also neighbors of (X2,Y2)
    (   (CX is X1 + 1, CY is Y1) ; (CX is X1 - 1, CY is Y1) ; (CX is X1, CY is Y1 + 1) ; (CX is X1, CY is Y1 - 1) ),
    (   (CX is X2 + 1, CY is Y2) ; (CX is X2 - 1, CY is Y2) ; (CX is X2, CY is Y2 + 1) ; (CX is X2, CY is Y2 - 1) ),
    is_valid_coordinate(CX, CY).

% If cell (X1,Y1) has stench and (X2,Y2) has breeze (breeze_vs_stench),
% then cells adjacent to BOTH cannot have a wumpus (because the stench comes from somewhere else)
contradiction_safe_wumpus(CX, CY) :-
    contradiction(X1, Y1, X2, Y2, 'breeze_vs_stench'),
    % Check all 4 neighbors of (X1,Y1) that are also neighbors of (X2,Y2)
    (   (CX is X1 + 1, CY is Y1) ; (CX is X1 - 1, CY is Y1) ; (CX is X1, CY is Y1 + 1) ; (CX is X1, CY is Y1 - 1) ),
    (   (CX is X2 + 1, CY is Y2) ; (CX is X2 - 1, CY is Y2) ; (CX is X2, CY is Y2 + 1) ; (CX is X2, CY is Y2 - 1) ),
    is_valid_coordinate(CX, CY).

get_front_cell(X, Y, north, FX, FY) :- FX #= X,     FY #= Y + 1.
get_front_cell(X, Y, south, FX, FY) :- FX #= X,     FY #= Y - 1.
get_front_cell(X, Y, east,  FX, FY) :- FX #= X + 1, FY #= Y.
get_front_cell(X, Y, west,  FX, FY) :- FX #= X - 1, FY #= Y.

next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).

% Left direction helper
next_dir_left(north, west). next_dir_left(west, south).
next_dir_left(south, east). next_dir_left(east, north).

is_valid_coordinate(X, Y) :- member(X, [0,1,2,3,4,5]), member(Y, [0,1,2,3,4,5]).
list_to_conj([], true).
list_to_conj([H], H) :- !.
list_to_conj([H|T], (H, R)) :- list_to_conj(T, R).
untag(D, DOut) :- is_dict(D), !, dict_pairs(D, _, P), maplist(untag_pair, P, NP), dict_create(DOut, _, NP).
untag(L, LOut) :- is_list(L), !, maplist(untag, L, LOut).
untag(V, V).
untag_pair(K-V, K-VO) :- untag(V, VO).

% -----------------------------------------------------------------------------
% Memory & Triangulation helpers
% -----------------------------------------------------------------------------

% update_memory(+X,+Y,+Percepts)
% Store/update percepts observed at (X,Y)
update_memory(X, Y, Percepts) :-
    retractall(percepts_at(X, Y, _)),
    assertz(percepts_at(X, Y, Percepts)).

% triangulate_memory
% Compare stored percepts pairwise and record contradictions like
% (cell A has stench and no breeze) vs (cell B has breeze and no stench)
triangulate_memory :-
    % Clear previous contradictions and recompute
    retractall(contradiction(_,_,_,_,_)),
    findall([X1,Y1,P1,X2,Y2,P2], (percepts_at(X1,Y1,P1), percepts_at(X2,Y2,P2), (X1\=X2 ; Y1\=Y2)), Pairs),
    forall(member([X1,Y1,P1,X2,Y2,P2], Pairs), check_and_record_contradiction(X1,Y1,P1,X2,Y2,P2)).

check_and_record_contradiction(X1,Y1,P1,X2,Y2,P2) :-
    ( has_percept(P1, stench), \+ has_percept(P1, breeze),
      has_percept(P2, breeze), \+ has_percept(P2, stench) ->
        add_contradiction(X1,Y1,X2,Y2, 'stench_vs_breeze')
    ; true ),
    ( has_percept(P1, breeze), \+ has_percept(P1, stench),
      has_percept(P2, stench), \+ has_percept(P2, breeze) ->
        add_contradiction(X1,Y1,X2,Y2, 'breeze_vs_stench')
    ; true ).

has_percept(Percepts, Tag) :- is_list(Percepts), member(Tag, Percepts).

add_contradiction(X1,Y1,X2,Y2,Reason) :-
    ( contradiction(X1,Y1,X2,Y2,_) ; contradiction(X2,Y2,X1,Y1,_) ) -> true
    ; assertz(contradiction(X1,Y1,X2,Y2,Reason)).