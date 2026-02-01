#!/usr/bin/env swipl

:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).

% AJOUTER CETTE LIGNE :
:- use_module(ontology).

% Configuration du serveur
:- set_setting(http:logfile, 'httpd_hunter.log').
:- set_setting(http:cors, [*]).

% Activation CORS (indispensable pour que le navigateur accepte la réponse)
:- cors_enable.

% Point d'entrée pour lancer le serveur sur le port 8081 (comme demandé par le store.ts)
run_hunter :-
    http_server(http_dispatch, [port(8081)]).

% Définition de la route /action
:- http_handler(root(action), handle_hunter_request, []).

% ----------------------------------------------------------------------
% Gestion des requêtes OPTIONS (pre-flight check pour CORS)
% ----------------------------------------------------------------------
handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').

% ----------------------------------------------------------------------
% Gestion de la requête PUT /action
% ----------------------------------------------------------------------
handle_hunter_request(Request) :-
    % 1. Lire le JSON en toute sécurité
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    
    % 2. Nettoyer les données (extraire beliefs et percepts)
    untag(RequestJSON.get(beliefs, _{}), Beliefs),
    untag(RequestJSON.get(percepts, []), Percepts),
    
    % 3. Extraction sécurisée des coordonnées et de la direction
    % On utilise .get/3 pour fournir des valeurs par défaut ('?') si la clé n'existe pas
    Fluents  = Beliefs.get(certain_fluents, _{}),
    Hunter   = Fluents.get(fat_hunter, _{c:_{x:'?', y:'?'}}),
    Coord    = Hunter.get(c, _{x:'?', y:'?'}),
    PosX     = Coord.get(x, '?'),
    PosY     = Coord.get(y, '?'),
    
    % Extraction de la direction dans la liste
    (   get_dict(dir, Fluents, DirList),
        member(dir{d:D, h:hunter{id:hunter}}, DirList)
    ->  Direction = D
    ;   Direction = 'inconnue'
    ),

    % 4. AFFICHAGE ORDONNÉ ET LISIBLE
    format('~N~n==============================================~n'),
    format('          REQUÊTE ACTION DU HUNTER            ~n'),
    format('----------------------------------------------~n'),
    format('  [POSITION]  X : ~w, Y : ~w~n', [PosX, PosY]),
    format('  [DIRECTION] ~w~n', [Direction]),
    format('  [PERCEPTS]  ~w~n', [Percepts]),
    format('==============================================~n~n'),

    % 5. Réponse par défaut
    Action = move,
    NewBeliefs = Beliefs,

    Response = _{
        hunterState: _{
            beliefs: NewBeliefs,
            percepts: Percepts
        },
        action: Action
    },

    cors_enable,
    reply_json_dict(Response).

% ======================================================================
% LOGIQUE DU HUNTER (Cerveau mis à jour pour ontology.pl)
% ======================================================================

% Cas 1 : Si ça brille (glitter), on RAMASSE l'or !
run_agent_turn(Beliefs, Percepts, NewBeliefs, grab) :-
    member(glitter, Percepts),
    !,
    http_log('PERCEPT: Glitter! Action: GRAB~n', []),
    % On laisse ontology.pl gérer les effets du grab dans le cas général, 
    % mais ici on renvoie juste l'action au frontend.
    NewBeliefs = Beliefs.

% Cas 2 : Si on s'est cogné (bump), on TOURNE (pour se débloquer)
run_agent_turn(Beliefs, Percepts, NewBeliefs, left) :-
    member(bump, Percepts),
    !,
    http_log('PERCEPT: Bump! Action: LEFT~n', []),
    NewBeliefs = Beliefs.

% Cas 3 : Comportement par défaut -> AVANCER (Action: move)
run_agent_turn(Beliefs, _Percepts, NewBeliefs, move) :-
    % 1. Récupérer les Fluents et Eternals (déjà nettoyés par untag/2)
    Fluents = Beliefs.certain_fluents,
    Eternals = Beliefs.certain_eternals,

    % 2. Affichage de la position et de la direction pour le debug
    (   Hunter = Fluents.fat_hunter,
        DirList = Fluents.dir,
        member(dir{d:D, h:hunter{id:hunter}}, DirList)
    ->  http_log('POSITION ACTUELLE : X=~w, Y=~w | DIRECTION: ~w~n', 
                 [Hunter.c.x, Hunter.c.y, D])
    ;   http_log('Erreur : Impossible de lire la position ou la direction dans les Fluents~n', [])
    ),

    % 3. Utiliser l'ontologie pour calculer l'effet réel du mouvement
    % Cela va utiliser vos règles effects(Eternals, Fluents, move, ResultingFluents)
    (   catch(effects(Eternals, Fluents, move, NewFluents), Error, 
              (http_log('Erreur ontology: ~w~n', [Error]), fail))
    ->  http_log('Ontology a calculé le mouvement avec succès.~n', []),
        NewBeliefs = Beliefs.put(certain_fluents, NewFluents)
    ;   http_log('ECHEC : L\'ontologie n\'a pas pu calculer le mouvement (mur ou erreur).~n', []),
        NewBeliefs = Beliefs
    ),
    http_log('Action envoyée: MOVE~n', []).

% ----------------------------------------------------------------------
% Utilitaires (copiés de server.pl pour nettoyer les dicts)
% ----------------------------------------------------------------------
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