:- module(agent_logic, [run_hunter/0]).

% --- 1. IMPORTS & CONFIGURATION ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).

% Imports IA : Contraintes & Probabilités
:- use_module(library(clpfd)).
:- use_module(library(mcintyre)).

% Initialisation du moteur probabiliste
:- mc.

% Configuration HTTP
:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).
:- cors_enable.

% --- 2. SERVEUR HTTP (Prolog Standard - Hors LPAD) ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    % A. NETTOYAGE : Dict -> Termes Prolog (Interdit d'envoyer des Dicts au LPAD)
    extract_fluents(BeliefsDict.certain_fluents, CurrentState),

    % B. INTERROGATION PROBABILISTE
    % On demande au LPAD de choisir une action (échantillonnage)
    ( mc_sample_arg(
        choose_action_prob(CurrentState, Percepts, A, NS),
        1,
        (A, NS),
        Results
      ) ->
        [[(Action, NewStateList)]-_] = Results,
        format(atom(DebugMsg), 'LPAD Decision: ~w', [Action])
    ; 
        Action = move, NewStateList = CurrentState,
        DebugMsg = 'LPAD Failed - Fallback'
    ),

    % C. RECONSTRUCTION : Termes Prolog -> Dict
    update_fluents_dict(BeliefsDict.certain_fluents, NewStateList, UpdatedFluentsDict),
    put_dict(certain_fluents, BeliefsDict, UpdatedFluentsDict, NewBeliefsDict),

    Response = _{
        hunterState: _{ beliefs: NewBeliefsDict, percepts: Percepts },
        action: Action,
        debug: DebugMsg
    },
    
    cors_enable,
    reply_json_dict(Response).


% --- 3. UTILITAIRES DE CONVERSION (Dict <-> Termes) ---

extract_fluents(Dict, StateList) :-
    % Extrait la position (Exemple simplifié)
    ( get_dict(fat_hunter, Dict, H), get_dict(c, H, Pos), get_dict(x, Pos, X), get_dict(y, Pos, Y)
    -> Loc = at(X,Y) ; Loc = at(1,1) ),
    StateList = [Loc].

update_fluents_dict(Dict, NewState, NewDict) :-
    ( member(at(NX, NY), NewState)
    -> NewPos = c{x:NX, y:NY},
       put_dict(fat_hunter, Dict, fat{c:NewPos, h:hunter{id:hunter}}, NewDict)
    ;  NewDict = Dict ).


% --- 4. CERVEAU PROBABILISTE (LPAD) ---
% Zone protégée : Pas de Dicts, seulement de la logique pure.

:- begin_lpad.

% Règle 1 : Si Or, on prend (Déterministe)
choose_action_prob(State, Percepts, grab, State) :-
    member(glitter, Percepts).

% Règle 2 : Sinon, on bouge (Avec choix probabiliste si on veut)
% Ici, on appelle le moteur CLP pour valider le mouvement
choose_action_prob(State, _Percepts, move, NewState) :-
    solve_movement_clp(State, NewState).

% Moteur de mouvement CLP (Isolé pour éviter le bug "garbage_collected")
solve_movement_clp(State, NewState) :-
    member(at(X,Y), State),
    
    % Choix de direction (Pour l'instant statique, mais pourrait être probabiliste)
    Dir = east,
    delta(Dir, DX, DY),
    
    NextX #= X + DX,
    NextY #= Y + DY,
    NextX in 0..4, NextY in 0..4, % Contraintes de grille
    
    label([NextX, NextY]), % IMPORTANT : Fixer les valeurs
    
    select(at(X,Y), State, at(NextX, NextY), NewState).

delta(north, 0, 1).
delta(south, 0, -1).
delta(east, 1, 0).
delta(west, -1, 0).

:- end_lpad.