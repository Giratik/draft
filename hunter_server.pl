#!/usr/bin/env swipl

:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).

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


% ----------------------------------------------------------------------
% Stub pour l'IA (À REMPLACER PAR VOTRE PROPRE LOGIQUE)
% ----------------------------------------------------------------------
% Pour tester si le serveur marche, ce prédicat fait avancer le Hunter bêtement
run_agent_turn(Beliefs, _Percepts, Beliefs, move). 
% Note : Dans la vraie implémentation, 'move' doit être remplacé par 
% l'action décidée (move, turnLeft, shoot, etc.) et Beliefs doit être mis à jour.


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