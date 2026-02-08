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
% 2. CONFIGURATION PITA (Modèle Probabiliste Dynamique)
% ==============================================================================
:- pita.
:- begin_lpad.

% --- Monde Dynamique ---
% La grille valide dépend maintenant de Size (passé en argument)
% Les murs sont à 0 et Size+1. La zone jouable est 1..Size.
valid_grid(X,Y, Size) :- 
    Limit #= Size + 1,
    % On donne une plage large pour X/Y afin que PITA puisse explorer
    member(X, [0,1,2,3,4,5,6,7,8,9,10,11,12]), X #=< Limit, 
    member(Y, [0,1,2,3,4,5,6,7,8,9,10,11,12]), Y #=< Limit.

start_pos(1,1).

% --- Priors Dynamiques ---
% Formules basées sur world.pl :
% N_Wumpus = Size / 4
% N_Pits   = 3 * N_Wumpus
% Surface  ~ Size^2
% Prob = Nombre / Surface

pit(X,Y, Size):Prob :- 
    valid_grid(X,Y, Size), \+ start_pos(X,Y),
    % Prob ≈ (3 * Size/4) / Size^2 = 0.75 / Size
    Prob is 0.75 / Size.

wumpus(X,Y, Size):Prob :- 
    valid_grid(X,Y, Size), \+ start_pos(X,Y),
    % Prob ≈ (1 * Size/4) / Size^2 = 0.25 / Size
    Prob is 0.25 / Size.

% --- Causalité (avec propagation de Size) ---
breeze(X, Y, Size) :- adjacent(X, Y, NX, NY, Size), pit(NX, NY, Size).
stench(X, Y, Size) :- adjacent(X, Y, NX, NY, Size), wumpus(NX, NY, Size).

% --- Sécurité ---
safe(X,Y, Size) :- \+ pit(X,Y, Size), \+ wumpus(X,Y, Size).

% --- Adjacence ---
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
    format(user_error, '~N~n[SERVER] Hunter Agent (Dynamic Logic) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

% Mémoire locale pour les contradictions et percepts
:- dynamic percepts_at/3.         
:- dynamic contradiction/5.       

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
    
    % --- EXTRACTION ET DÉDUCTION DE LA TAILLE ---
    extract_state(Beliefs, X, Y, Dir, Visited, Size),

    % Mise à jour mémoire et triangulation
    update_memory(X, Y, Percepts),
    triangulate_memory,
    findall(_{x:X1, y:Y1, x2:X2, y2:Y2, reason:Reason}, contradiction(X1,Y1,X2,Y2,Reason), Contradictions),

    format(user_error, '~N~n================================================~n', []),
    format(user_error, '          [HUNTER ACTION REQUEST]               ~n', []),
    format(user_error, '------------------------------------------------~n', []),
    format(user_error, '  POS: ~w,~w (~w) | GRID SIZE (Deduced): ~w~n', [X, Y, Dir, Size]),
    format(user_error, '  PERCEPTS: ~w~n', [Percepts]),
    format(user_error, '================================================~n', []),

    decide_action(X, Y, Dir, Visited, Percepts, Size, Action),
    
    Response = _{
        hunterState: _{ beliefs: Beliefs, percepts: Percepts },
        action: Action,
        contradictions: Contradictions
    },
    reply_json_dict(Response).


% ==============================================================================
% 4. CERVEAU (Stratégie d'Exploration)
% ==============================================================================

decide_action(_, _, _, _, Percepts, _, grab) :-
    member(glitter, Percepts),
    format(user_error, '  -> DECISION: GRAB (Or!)~n', []), !.

decide_action(_, _, _, _, Percepts, _, right) :-
    member(bump, Percepts),
    format(user_error, '  -> DECISION: RIGHT (Mur)~n', []), !.

decide_action(X, Y, Dir, Visited, Percepts, Size, Action) :-
    % A. Construire les preuves (Evidence)
    build_evidence(X, Y, Percepts, Visited, Size, EvidenceConj),
    
    % B. Evaluer le score de la case DEVANT
    get_front_cell(X, Y, Dir, FrontX, FrontY),
    evaluate_cell(FrontX, FrontY, EvidenceConj, Visited, Size, ScoreFront),
    
    % C. Evaluer le score de la case à DROITE
    next_dir_right(Dir, RightDir),
    get_front_cell(X, Y, RightDir, RightX, RightY),
    evaluate_cell(RightX, RightY, EvidenceConj, Visited, Size, ScoreRight),
    
    % D. Evaluer GAUCHE
    next_dir_left(Dir, LeftDir),
    get_front_cell(X, Y, LeftDir, LeftX, LeftY),
    evaluate_cell(LeftX, LeftY, EvidenceConj, Visited, Size, ScoreLeft),

    format(user_error, '  [SCORES] Front(~w,~w)=~w | Right(~w,~w)=~w | Left(~w,~w)=~w~n', 
           [FrontX, FrontY, ScoreFront, RightX, RightY, ScoreRight, LeftX, LeftY, ScoreLeft]),

    ( ScoreLeft > ScoreFront, ScoreLeft > ScoreRight ->
        Action = left,
        format(user_error, '  -> DECISION: LEFT (Meilleur score)~n', [])
    ; ScoreRight > ScoreFront ->
        Action = right,
        format(user_error, '  -> DECISION: RIGHT (Meilleur score)~n', [])
    ; ScoreFront > 0 ->
        Action = move,
        format(user_error, '  -> DECISION: MOVE (Ok)~n', [])
    ;
        Action = right,
        format(user_error, '  -> DECISION: RIGHT (Bloqué)~n', [])
    ).


% ==============================================================================
% 5. EVALUATION (PITA + Heuristique)
% ==============================================================================

evaluate_cell(X, Y, _, _, Size, 0) :-
    \+ is_valid_coordinate(X, Y, Size), !. % Mur hors limites

evaluate_cell(X, Y, Evidence, Visited, Size, Score) :-
    % Appel Probabiliste avec SIZE
    ( prob((safe(X, Y, Size), Evidence), P_Joint),
      prob(Evidence, P_Ev),
      P_Ev > 0.000001
    ->
        P is P_Joint / P_Ev
    ;
        P = 0.5 
    ),
    
    % Ajustement de l'heuristique en fonction de la taille si besoin
    ( P > 0.8 -> 
        ( member([X, Y], Visited) ->
             Score = 1 
        ;
             Score = 2 
        )
    ;
        Score = 0 
    ).


% ==============================================================================
% 6. UTILITAIRES
% ==============================================================================

% Extraction de l'état ET calcul de la taille
extract_state(Beliefs, X, Y, Dir, VisitedCoordinates, Size) :-
    Fluents = Beliefs.get(certain_fluents, _{}),
    H = Fluents.get(fat_hunter, _{}), C = H.get(c, _{}),
    RawX = C.get(x, 1), RawY = C.get(y, 1),
    X #= RawX, Y #= RawY,
    ( get_dict(dir, Fluents, DL), member(dir{d:D, h:_}, DL) -> Dir = D ; Dir = north ),
    ( get_dict(visited, Fluents, VL) -> 
        findall([VX, VY], (member(VObj, VL), get_dict(to, VObj, VTo), VX = VTo.x, VY = VTo.y), VisitedCoordinates)
    ; VisitedCoordinates = [] ),
    
    % --- LOGIQUE DE DÉDUCTION DE LA TAILLE ---
    Eternals = Beliefs.get(certain_eternals, _{}),
    Cells = Eternals.get(cells, []),
    length(Cells, NCells),
    ( NCells > 0 -> 
        TotalDim is round(sqrt(NCells)),
        Size is TotalDim - 2  % -2 car les murs sont inclus dans cells (0 et Size+1)
    ; 
        Size = 4 % Défaut
    ).

build_evidence(X, Y, Percepts, Visited, Size, EvidenceConj) :-
    ( member(breeze, Percepts) -> B = breeze(X,Y,Size) ; B = (\+ breeze(X,Y,Size)) ),
    ( member(stench, Percepts) -> S = stench(X,Y,Size) ; S = (\+ stench(X,Y,Size)) ),
    findall(\+ pit(Vx, Vy, Size), member([Vx, Vy], Visited), SafePits),
    findall(\+ wumpus(Wx, Wy, Size), member([Wx, Wy], Visited), SafeWumpus),
    
    % Contradictions (non adaptées pour Size ici par simplicité, mais on pourrait)
    findall(\+ pit(CX, CY, Size), contradiction_safe_pit(CX, CY, Size), ContradictionSafePits),
    findall(\+ wumpus(CX, CY, Size), contradiction_safe_wumpus(CX, CY, Size), ContradictionSafeWumpus),
    
    append([B, S | SafePits], SafeWumpus, BaseList),
    append(BaseList, ContradictionSafePits, List2),
    append(List2, ContradictionSafeWumpus, EvidenceList),
    list_to_conj(EvidenceList, EvidenceConj).

contradiction_safe_pit(CX, CY, Size) :-
    contradiction(X1, Y1, X2, Y2, 'stench_vs_breeze'),
    % Voisins communs
    (   (CX is X1 + 1, CY is Y1) ; (CX is X1 - 1, CY is Y1) ; (CX is X1, CY is Y1 + 1) ; (CX is X1, CY is Y1 - 1) ),
    (   (CX is X2 + 1, CY is Y2) ; (CX is X2 - 1, CY is Y2) ; (CX is X2, CY is Y2 + 1) ; (CX is X2, CY is Y2 - 1) ),
    is_valid_coordinate(CX, CY, Size).

contradiction_safe_wumpus(CX, CY, Size) :-
    contradiction(X1, Y1, X2, Y2, 'breeze_vs_stench'),
    (   (CX is X1 + 1, CY is Y1) ; (CX is X1 - 1, CY is Y1) ; (CX is X1, CY is Y1 + 1) ; (CX is X1, CY is Y1 - 1) ),
    (   (CX is X2 + 1, CY is Y2) ; (CX is X2 - 1, CY is Y2) ; (CX is X2, CY is Y2 + 1) ; (CX is X2, CY is Y2 - 1) ),
    is_valid_coordinate(CX, CY, Size).

get_front_cell(X, Y, north, FX, FY) :- FX #= X,     FY #= Y + 1.
get_front_cell(X, Y, south, FX, FY) :- FX #= X,     FY #= Y - 1.
get_front_cell(X, Y, east,  FX, FY) :- FX #= X + 1, FY #= Y.
get_front_cell(X, Y, west,  FX, FY) :- FX #= X - 1, FY #= Y.

next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).

next_dir_left(north, west). next_dir_left(west, south).
next_dir_left(south, east). next_dir_left(east, north).

is_valid_coordinate(X, Y, Size) :- 
    Limit is Size,
    X >= 1, X =< Limit, 
    Y >= 1, Y =< Limit.

list_to_conj([], true).
list_to_conj([H], H) :- !.
list_to_conj([H|T], (H, R)) :- list_to_conj(T, R).

untag(D, DOut) :- is_dict(D), !, dict_pairs(D, _, P), maplist(untag_pair, P, NP), dict_create(DOut, _, NP).
untag(L, LOut) :- is_list(L), !, maplist(untag, L, LOut).
untag(V, V).
untag_pair(K-V, K-VO) :- untag(V, VO).

% --- Mémoire ---
update_memory(X, Y, Percepts) :-
    retractall(percepts_at(X, Y, _)),
    assertz(percepts_at(X, Y, Percepts)).

triangulate_memory :-
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