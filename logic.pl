:- module(logic, [run_hunter/0]).

% --- 1. IMPORTS ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

% MOTEURS IA
:- use_module(library(clpfd)). % Contraintes
:- use_module(library(pita)).  % Probabilités Exactes (remplace mcintyre)

% Initialisation de PITA
:- pita.

:- set_setting(http:cors, [*]).
:- cors_enable.

% --- 2. SERVEUR HTTP ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~n[SERVER] Hunter Agent (PITA + CLP) running on port 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),
    
    % LOG CONSOLE
    format(user_error, '~n---------------------------------------------------~n', []),
    format(user_error, '[PERCEPT] Recu: ~w~n', [Percepts]),

    % 1. Conversion
    ( catch(extract_fluents(BeliefsDict.certain_fluents, CurrentState), _, fail)
    -> true 
    ;  CurrentState = [at(1,1)], format(user_error, '[WARN] State vide -> Fallback at(1,1)~n', [])
    ),
    format(user_error, '[STATE] Position actuelle: ~w~n', [CurrentState]),

    % 2. Appel Probabiliste (PITA)
    % On utilise prob/2 au lieu de mc_sample_arg.
    % prob(:Query, -Probability) va essayer de prouver la requête et donner sa proba.
    ( catch(prob(choose_action_prob(CurrentState, Percepts, Action, NewStateList), Prob), Error, true)
    -> 
        ( var(Error) ->
            % Succès
            format(user_error, '[DECISION] PITA Action: ~w (Prob: ~2f) (NewState: ~w)~n', [Action, Prob, NewStateList]),
            format(atom(DebugMsg), 'PITA Action: ~w (P=~2f)', [Action, Prob])
        ;
            % Erreur interne
            format(user_error, '[CRASH] Erreur PITA: ~w~n', [Error]),
            Action = move, NewStateList = CurrentState, DebugMsg = 'PITA Crashed'
        )
    ;
        % Echec logique (Aucune règle ne s'applique)
        format(user_error, '[FAIL] PITA n\'a trouve aucune solution !~n', []),
        Action = move, NewStateList = CurrentState, DebugMsg = 'PITA Failed'
    ),

    % 3. Réponse
    update_fluents_dict(BeliefsDict.certain_fluents, NewStateList, UpdatedFluentsDict),
    put_dict(certain_fluents, BeliefsDict, UpdatedFluentsDict, NewBeliefsDict),

    Response = _{
        hunterState: _{ beliefs: NewBeliefsDict, percepts: Percepts },
        action: Action,
        debug: DebugMsg
    },
    cors_enable,
    reply_json_dict(Response).


% --- 3. MOTEUR CLP (DÉTERMINISTE & HORS LPAD) ---
% Doit rester hors du bloc lpad pour éviter les conflits d'instanciation

solve_movement_clp(State, NewState) :-
    ( member(at(X,Y), State) -> true ; X=1, Y=1 ),
    
    % Logique simple : Aller à l'EST (Pour l'instant)
    delta(east, DX, DY),
    NextX #= X + DX,
    NextY #= Y + DY,
    
    NextX in 0..5, 
    NextY in 0..5,
    
    % Labeling est CRUCIAL ici pour que PITA reçoive des entiers, pas des variables CLP
    label([NextX, NextY]),
    
    ( select(at(X,Y), State, at(NextX, NextY), TempState) 
    -> NewState = TempState
    ;  NewState = [at(NextX, NextY)] ).

delta(north, 0, 1).
delta(south, 0, -1).
delta(east, 1, 0).
delta(west, -1, 0).


% --- 4. UTILITAIRES ---

extract_fluents(Dict, StateList) :-
    % Extraction flexible (fat_hunter ou direct)
    ( get_dict(fat_hunter, Dict, H), get_dict(c, H, Pos), get_dict(x, Pos, X), get_dict(y, Pos, Y)
    ; get_dict(x, Dict, X), get_dict(y, Dict, Y)
    ),
    number(X), number(Y), !,
    StateList = [at(X,Y)].

extract_fluents(_, [at(1,1)]). % Fallback final

update_fluents_dict(Dict, NewState, NewDict) :-
    ( member(at(NX, NY), NewState)
    -> NewPos = c{x:NX, y:NY},
       % Préservation de structure
       ( get_dict(fat_hunter, Dict, _) 
       -> put_dict(fat_hunter, Dict, fat{c:NewPos, h:hunter{id:hunter}}, NewDict)
       ;  put_dict(fat_hunter, Dict, fat{c:NewPos}, NewDict) ) % Si structure simple
    ;  NewDict = Dict ).


% --- 5. CERVEAU PROBABILISTE (LPAD - PITA) ---

:- begin_lpad.

% Règle 1 : Si Or, on ramasse (Probabilité 1.0 implicite)
choose_action_prob(State, Percepts, grab, State) :-
    member(glitter, Percepts).

% Règle 2 : Sinon, on appelle le moteur CLP externe
choose_action_prob(State, _Percepts, move, NewState) :-
    % once/1 est recommandé pour éviter que PITA ne cherche inutilement d'autres solutions
    % si le mouvement est déterministe.
    once(solve_movement_clp(State, NewState)).

:- end_lpad.