:- use_module(library(clpfd)).
:- use_module(library(dif)).
% Si vous avez installé reif comme demandé dans le PDF :
% :- use_module(library(reif)).

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
% LOGIQUE DU HUNTER (Architecture CLP)
% ======================================================================

% Point d'entrée principal appelé par le serveur
run_agent_turn(BeliefsDict, Percepts, NewBeliefsDict, Action) :-
    % 1. CONVERSION (Dict -> Termes Prolog)
    % On extrait les faits pertinents du Dict JSON pour les rendre "propres"
    extract_fluents(BeliefsDict.certain_fluents, CurrentState),
    
    % 2. DÉCISION (Cœur Logique en CLP)
    % Ici, on ne manipule que des termes Prolog et des contraintes
    choose_action_clp(CurrentState, Percepts, Action, NewState),
    
    % 3. RECONSTRUCTION (Termes -> Dict)
    % On remet les nouvelles infos dans le format attendu par le JSON
    update_fluents_dict(BeliefsDict.certain_fluents, NewState, UpdatedFluentsDict),
    put_dict(certain_fluents, BeliefsDict, UpdatedFluentsDict, NewBeliefsDict).

% --- Helpers de conversion (A adapter selon votre ontologie exacte) ---

% Exemple : Convertit le dict {fat_hunter: {c: {x:1, y:2}, ...}} en [at(1,2), ...]
extract_fluents(FluentsDict, StateList) :-
    % Récupération de la position (exemple simplifié)
    ( get_dict(fat_hunter, FluentsDict, HunterObj),
      get_dict(c, HunterObj, PosObj),
      get_dict(x, PosObj, X),
      get_dict(y, PosObj, Y) 
    -> Location = at(X, Y)
    ; Location = at(unknown, unknown)
    ),
    % Ajoutez d'autres extractions ici (or, flèches...)
    StateList = [Location].

% Mise à jour simpliste (à étoffer)
update_fluents_dict(OldDict, [at(NewX, NewY)|_], NewDict) :-
    % Création de la structure imbriquée pour le JSON
    NewPos = c{x:NewX, y:NewY},
    % Mise à jour du dict (attention, structure profonde simplifiée ici)
    put_dict(fat_hunter, OldDict, fat{c:NewPos, h:hunter{id:hunter}}, NewDict).

% ======================================================================
% CERVEAU CLP (Sans Dicts, Sans Impératif)
% ======================================================================

% Règle 1 : Si on voit de l'or, on ramasse (Priorité absolue)
choose_action_clp(_, Percepts, grab, State) :-
    member(glitter, Percepts),
    State = State. % L'état ne change pas physiquement avant l'action

% Règle 2 : Sinon, on essaie de bouger
choose_action_clp(State, _Percepts, move, NewState) :-
    % Récupérer ma position actuelle (depuis la liste de termes)
    member(at(X, Y), State),
    
    % Calculer la prochaine case avec des CONTRAINTES (#=)
    % (Supposons qu'on veuille aller au Nord pour l'exemple)
    Direction = north, 
    delta(Direction, DX, DY),
    NextX #= X + DX,
    NextY #= Y + DY,
    
    % Contraindre les bornes (La grille est 4x4 par exemple)
    NextX in 0..5,
    NextY in 0..5,

    % Mettre à jour l'état (purement déclaratif)
    % On remplace at(X,Y) par at(NextX, NextY)
    select(at(X,Y), State, at(NextX, NextY), NewState).

% Définition déclarative des mouvements
delta(north, 0, 1).
delta(south, 0, -1).
delta(east, 1, 0).
delta(west, -1, 0).

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