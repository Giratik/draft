#!/usr/bin/env swipl

:- module(hunter_server, [run_hunter/0]).

% ==============================================================================
% 1. IMPORTATION DES MODULES
% ==============================================================================
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).
:- use_module(library(clpfd)). % ARITHMÉTIQUE DÉCLARATIVE (OBLIGATOIRE HW2)
:- use_module(library(pita)).  % RAISONNEMENT PROBABILISTE

% ==============================================================================
% 2. CONFIGURATION PITA (Section Probabiliste)
% ==============================================================================
:- pita.
:- begin_lpad.

start_pos(1,1).
valid_grid(X,Y) :- member(X, [0,1,2,3,4,5]), member(Y, [0,1,2,3,4,5]).

% A. Priors (Probabilités de world.pl)
pit(X,Y):0.2 :- valid_grid(X,Y), \+ start_pos(X,Y).
wumpus(X,Y):0.07 :- valid_grid(X,Y), \+ start_pos(X,Y).

% --- Règles de Causalité (Si Trou -> Brise) ---
breeze(X, Y) :- 
    adjacent(X, Y, NX, NY), 
    pit(NX, NY).

stench(X, Y) :- 
    adjacent(X, Y, NX, NY), 
    wumpus(NX, NY).

% --- Adjacence Déclarative avec CLP(FD) ---
adjacent(X, Y, NX, NY) :- NX #= X,     NY #= Y + 1.
adjacent(X, Y, NX, NY) :- NX #= X,     NY #= Y - 1.
adjacent(X, Y, NX, NY) :- NX #= X + 1, NY #= Y.
adjacent(X, Y, NX, NY) :- NX #= X - 1, NY #= Y.

:- end_lpad.

% ==============================================================================
% 3. CONFIGURATION DU SERVEUR
% ==============================================================================
:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).

% Point d'entrée pour lancer le serveur sur le port 8081
run_hunter :-
    catch(http_stop_server(8081, []), _, true), % Stop si déjà lancé
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~N~n[SERVER] Hunter Agent running on port 8081 (PITA + CLP(FD))...~n', []).

% Définition de la route
:- http_handler(root(action), handle_hunter_request, []).

% ==============================================================================
% 4. GESTIONNAIRE DE REQUÊTE (Cerveau de l'Agent)
% ==============================================================================

% Gestion des requêtes OPTIONS (CORS pre-flight)
handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

% Gestion des requêtes PUT (Action demandée)
handle_hunter_request(Request) :-
    % A. Lecture sécurisée du JSON
    cors_enable,
    catch(
        http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
        Error,
        (format(user_error, '[ERREUR] JSON invalide: ~w~n', [Error]), fail)
    ),

    % B. Extraction des Données (Beliefs & Percepts)
    % On utilise untag pour nettoyer les structures Prolog/JSON
    untag(RequestJSON, CleanJSON),
    Beliefs  = CleanJSON.get(beliefs, _{}),
    Percepts = CleanJSON.get(percepts, []),
    Fluents  = Beliefs.get(certain_fluents, _{}),
    
    % C. Récupération de la Position avec CLP(FD)
    % On récupère 'fat_hunter.c' (coordinates)
    HunterStruct = Fluents.get(fat_hunter, _{}),
    HunterCoord  = HunterStruct.get(c, _{}),
    
    % Utilisation de valeurs par défaut si le JSON est incomplet (ex: début de partie)
    RawX = HunterCoord.get(x, 1),
    RawY = HunterCoord.get(y, 1),
    
    % Contrainte CLP(FD) : X et Y sont des entiers
    X #= RawX,
    Y #= RawY,

    % D. LOGS ORDONNÉS (Sortie Terminal)
    format(user_error, '~N~n================================================~n', []),
    format(user_error, '          [HUNTER ACTION REQUEST]               ~n', []),
    format(user_error, '------------------------------------------------~n', []),
    format(user_error, '  POSITION (CLP)    : X=~w, Y=~w~n', [X, Y]),
    format(user_error, '  PERCEPTS          : ~w~n', [Percepts]),
    format(user_error, '================================================~n', []),

    % E. DÉCISION (Appel à la logique de l'agent)
    decide_action(Beliefs, Percepts, Action),
    
    % F. MISE À JOUR DES CROYANCES
    % Pour l'instant, on renvoie les croyances telles quelles (ou mises à jour si besoin)
    NewBeliefs = Beliefs,

    % G. ENVOI DE LA RÉPONSE
    Response = _{
        hunterState: _{
            beliefs: NewBeliefs,
            percepts: Percepts
        },
        action: Action
    },
    reply_json_dict(Response).

% ==============================================================================
% 5. LOGIQUE DE DÉCISION (Règles Métier)
% ==============================================================================

% Règle 1 : Si on voit de l'or (glitter), on le ramasse !
decide_action(_Beliefs, Percepts, grab) :-
    member(glitter, Percepts),
    format(user_error, '  -> DECISION: GRAB (J\'ai vu de l\'or!)~n', []), !.

% Règle 2 : Sinon, on avance (Action par défaut)
decide_action(_Beliefs, _Percepts, move) :-
    format(user_error, '  -> DECISION: MOVE~n', []).

% ==============================================================================
% 6. UTILITAIRES JSON
% ==============================================================================
% Nettoie les dictionnaires SWI-Prolog des tags inutiles
untag(DictIn, DictOut) :-
    is_dict(DictIn), !,
    dict_pairs(DictIn, _, Pairs),
    maplist(untag_pair, Pairs, NewPairs),
    dict_create(DictOut, _, NewPairs).
untag(ListIn, ListOut) :-
    is_list(ListIn), !,
    maplist(untag, ListIn, ListOut).
untag(Value, Value).

untag_pair(K-VIn, K-VOut) :- untag(VIn, VOut).