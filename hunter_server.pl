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
    % 1. Lire le JSON envoyé par le frontend
    % L'option tag('') permet d'éviter les structures json(...) complexes
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    
    % 2. Extraire les données (Croyances et Perceptions)
    % Le frontend envoie l'objet HunterState entier : { beliefs: ..., percepts: ... }
    _{
        beliefs: RawBeliefs, 
        percepts: RawPercepts
    } :< RequestJSON,

    % 3. Nettoyer les données (enlever les tags JSON si nécessaire)
    untag(RawBeliefs, Beliefs),
    untag(RawPercepts, Percepts),
    
    % LOG pour le débogage
    http_log('Hunter received beliefs: ~w~n', [Beliefs]),
    http_log('Hunter received percepts: ~w~n', [Percepts]),

    % 4. APPEL À VOTRE INTELLIGENCE ARTIFICIELLE
    % C'est ici que vous connectez votre logique.
    % Input : Beliefs, Percepts
    % Output: NewBeliefs, Action
    run_agent_turn(Beliefs, Percepts, NewBeliefs, Action),

    % 5. Construire la réponse JSON attendue par ActionResponse dans models.ts
    Response = _{
        hunterState: _{
            beliefs: NewBeliefs,
            percepts: Percepts % On renvoie les percepts courants (ou mis à jour si besoin)
        },
        action: Action
    },

    % 6. Envoyer la réponse
    cors_enable,
    reply_json_dict(Response).


% ======================================================================
% LOGIQUE DU HUNTER (Cerveau)
% ======================================================================

% Cas 1 : Si ça brille (glitter), on RAMASSE l'or !
run_agent_turn(Beliefs, Percepts, NewBeliefs, grab) :-
    member(glitter, Percepts),
    !,
    http_log('PERCEPT: Glitter! Action: GRAB~n', []),
    % Ici, idéalement, on devrait mettre à jour Beliefs pour dire "j'ai l'or"
    NewBeliefs = Beliefs.

% Cas 2 : Si on s'est cogné (bump), on TOURNE (pour se débloquer)
run_agent_turn(Beliefs, Percepts, NewBeliefs, left) :-
    member(bump, Percepts),
    !,
    http_log('PERCEPT: Bump! Action: LEFT~n', []),
    % Ici, on devrait mettre à jour l'orientation dans Beliefs
    NewBeliefs = Beliefs.

% Cas 3 : Comportement par défaut -> AVANCER
run_agent_turn(Beliefs, _Percepts, NewBeliefs, move) :-
% 1. Récupérer l'état actuel supposé (Fluents)
    dict_pairs(Beliefs.certain_fluents, _, Pairs),
    dict_create(Fluents, fluents, Pairs),
    
    % 2. Récupérer les murs connus (Eternals)
    dict_pairs(Beliefs.certain_eternals, _, EPairs),
    dict_create(Eternals, eternals, EPairs),

    % 3. Utiliser l'ontologie pour calculer l'effet du mouvement
    % Note : Cela nécessite que 'ontology' soit importé et compatible
    (catch(effects(Eternals, Fluents, move, NewFluents), _, fail) ->
        true
    ;
        NewFluents = Fluents % Si erreur, on garde l'ancien état
    ),

    % 4. Mettre à jour les croyances
    NewBeliefs = Beliefs.put(certain_fluents, NewFluents),
    http_log('Action: MOVE (Beliefs updated)~n', []).


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