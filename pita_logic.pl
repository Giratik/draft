:- module(logic, [run_hunter/0]).

% --- IMPORTS ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(clpfd)).

% Importation de PITA pour le raisonnement probabiliste
:- use_module(library(pita)).
:- pita.

:- set_setting(http:cors, [*]).
:- cors_enable.

% --- SERVEUR ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~n[SERVER] Hunter Agent (Probabilistic) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n\r\n').

handle_hunter_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    
    % 1. Récupération des données brutes
    _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON,
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    format(user_error, '~n---------------------------------------------------~n', []),
    format(user_error, '[PERCEPTS] ~w~n', [Percepts]),

    % 2. Extraction de l'état (Position, Direction, Visited)
    % On convertit les Dicts JSON en termes Prolog utilisables
    ( catch(extract_state(BeliefsDict, CurrentState, VisitedList), _, fail)
    -> true 
    ;  
       CurrentState = [at(1,1), facing(north)], 
       VisitedList = [visited(1,1)],
       format(user_error, '[WARN] Extraction fail. Using default.~n', [])
    ),
    format(user_error, '[STATE] ~w~n', [CurrentState]),

    % 3. Prise de décision (Appel au cerveau probabiliste)
    ( catch(decide_action(CurrentState, VisitedList, Percepts, Action, NewState), Error, true)
    -> 
        ( var(Error) ->
            format(user_error, '[DECISION] Action: ~w~n', [Action]),
            DebugMsg = 'OK'
        ;
            format(user_error, '[CRASH PITA] ~w~n', [Error]),
            Action = move, NewState = CurrentState, DebugMsg = 'Crash'
        )
    ;
        format(user_error, '[FAIL] Fallback decision.~n', []),
        Action = right, NewState = CurrentState, DebugMsg = 'Fallback'
    ),

    % 4. Mise à jour des croyances (SANS assert, on renvoie le nouveau dict)
    update_fluents_dict(BeliefsDict, NewState, UpdatedBeliefsDict),
    
    Response = _{
        hunterState: _{ beliefs: UpdatedBeliefsDict, percepts: Percepts },
        action: Action,
        debug: DebugMsg
    },
    cors_enable,
    reply_json_dict(Response).


% --- MODÈLE PROBABILISTE (LPAD) ---

:- begin_lpad.

% Définition de la grille (4x4)
valid_pos(X, Y) :- X in 0..3, Y in 0..3.

% 1. Modèle des Dangers
% Un Pit existe avec une probabilité de 0.2 sur une case donnée (a priori)
pit(X,Y) : 0.2 :- valid_pos(X,Y).

% Le Wumpus existe (simplification: on suppose une proba uniforme faible par case pour l'exemple)
wumpus(X,Y) : 0.1 :- valid_pos(X,Y).

% 2. Modèle des Percepts (Sensor Model)
% On sent une brise si un voisin est un trou
breeze(X,Y) :- 
    valid_pos(X,Y),
    adj(X,Y, NX, NY),
    pit(NX, NY).

% On sent une odeur si un voisin est le Wumpus
stench(X,Y) :-
    valid_pos(X,Y),
    adj(X,Y, NX, NY),
    wumpus(NX, NY).

% 3. Définition de la sécurité
% Une case est safe si elle n'a ni trou ni wumpus
safe(X,Y) :- \+ pit(X,Y), \+ wumpus(X,Y).

% Helper d'adjacence
adj(X,Y, NX,NY) :- NX #= X+1, NY #= Y.
adj(X,Y, NX,NY) :- NX #= X-1, NY #= Y.
adj(X,Y, NX,NY) :- NX #= X,   NY #= Y+1.
adj(X,Y, NX,NY) :- NX #= X,   NY #= Y-1.

:- end_lpad.


% --- CERVEAU : STRATÉGIE DE DÉCISION ---

% Cas 1 : De l'or ! On ramasse.
decide_action(State, _, Percepts, grab, State) :-
    member(glitter, Percepts).

% Cas 2 : On a cogné un mur (Bump), on tourne aléatoirement (ou droite par défaut)
decide_action(State, _, Percepts, right, NewState) :-
    member(bump, Percepts),
    physics_turn_right(State, NewState).

% Cas 3 : Mouvement intelligent basé sur les probabilités
decide_action(State, VisitedList, Percepts, Action, NewState) :-
    member(at(X,Y), State),
    member(facing(Dir), State),

    % A. Construire les preuves (Evidence) basées sur ce qu'on sait
    % Ici, on utilise le percept actuel + le fait que les cases visitées sont sûres (sinon on serait mort)
    build_evidence(X, Y, Percepts, VisitedList, Evidence),
    
    % B. Regarder la case devant nous
    delta(Dir, DX, DY),
    FrontX #= X + DX, FrontY #= Y + DY,

    ( valid_pos(FrontX, FrontY) ->
        % Calculer la probabilité que la case devant soit SÛRE
        % On interroge PITA : prob(safe(FrontX,FrontY), Evidence, ProbSafe)
        ( prob(safe(FrontX, FrontY), Evidence, P),
          format(user_error, '[PROB] Case devant (~d,~d) Safe P=~2f~n', [FrontX, FrontY, P])
        ; P = 0.0 % Si échec du calcul
        )
    ;
        P = 0.0 % Mur
    ),

    % C. Seuil de décision
    % Si P > 0.7 (assez sûr), on avance. Sinon on tourne.
    ( P > 0.7 ->
        Action = move,
        physics_move(State, NewState)
    ;
        % Sinon, on tourne (stratégie simple : tourner à droite)
        format(user_error, '[DANGER] Devant trop risqué (P=~2f). On tourne.~n', [P]),
        Action = right,
        physics_turn_right(State, NewState)
    ).


% --- OUTILS ET HELPERS ---

% Construit la liste d'Evidence pour PITA
% Evidence = [breeze(1,1), \+stench(1,1), safe(1,1), safe(1,2)...]
build_evidence(CurX, CurY, Percepts, VisitedList, Evidence) :-
    % 1. Percepts actuels
    (member(breeze, Percepts) -> B = breeze(CurX, CurY) ; B = \+ breeze(CurX, CurY)),
    (member(stench, Percepts) -> S = stench(CurX, CurY) ; S = \+ stench(CurX, CurY)),
    
    % 2. Historique : Les cases visitées sont considérées comme "safe" (observation passée)
    % Pour PITA, on peut les ajouter comme faits si le modèle est conditionnel, 
    % ou simplement assumer que le modèle 'breeze' ci-dessus suffit.
    % Pour simplifier ici, on donne juste les percepts locaux.
    Evidence = [B, S]. 
    % Note : Pour un agent parfait, il faudrait accumuler TOUS les percepts passés dans BeliefsDict.


% --- PHYSIQUE SIMULÉE (POUR PREDIRE L'ETAT SUIVANT) ---

physics_move(State, NewState) :-
    member(at(X,Y), State),
    member(facing(Dir), State),
    delta(Dir, DX, DY),
    NextX #= X + DX,
    NextY #= Y + DY,
    (valid_pos(NextX, NextY) -> 
        label([NextX, NextY]),
        select(at(X,Y), State, at(NextX, NextY), NewState)
    ;   
        NewState = State % Mur = pas de changement de pos
    ).

physics_turn_right(State, NewState) :-
    member(facing(Dir), State), next_dir_right(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

physics_turn_left(State, NewState) :-
    member(facing(Dir), State), next_dir_left(Dir, NewDir),
    select(facing(Dir), State, facing(NewDir), NewState).

delta(north, 0, 1).   delta(south, 0, -1).
delta(east, 1, 0).    delta(west, -1, 0).
next_dir_right(north, east). next_dir_right(east, south).
next_dir_right(south, west). next_dir_right(west, north).
next_dir_left(north, west).  next_dir_left(west, south).
next_dir_left(south, east).  next_dir_left(east, north).


% --- EXTRACTION & UPDATES ---

extract_state(BeliefsDict, State, VisitedList) :-
    Fluents = BeliefsDict.certain_fluents,
    get_dict(fat_hunter, Fluents, H), get_dict(c, H, Pos),
    safe_number(Pos.x, X), safe_number(Pos.y, Y),
    
    ( get_dict(dir, Fluents, DirList), member(DObj, DirList), get_dict(d, DObj, DirAtom) 
    -> Dir = DirAtom ; Dir = north ),

    BaseState = [at(X,Y), facing(Dir)],

    % Extraction des visités pour l'historique
    ( get_dict(visited, Fluents, VisJSON) 
    -> maplist(extract_visited_term, VisJSON, VisitedList) 
    ;  VisitedList = [] ),
    
    append(BaseState, VisitedList, State).

safe_number(Val, Num) :- number(Val), !, Num = Val.
safe_number(Atom, Num) :- atom(Atom), atom_number(Atom, Num).

extract_visited_term(Obj, visited(X,Y)) :-
    get_dict(to, Obj, To), safe_number(To.x, X), safe_number(To.y, Y).

% --- UPDATE FLUENTS (CORRIGÉ) ---
% Met à jour le dictionnaire JSON sans assert/retract
update_fluents_dict(BeliefsDict, NewState, NewBeliefsDict) :-
    ( member(at(NX, NY), NewState), member(facing(NDir), NewState)
    -> 
       Fluents = BeliefsDict.certain_fluents,
       
       % Récupérer l'ancienne position pour créer l'historique (from -> to)
       ( get_dict(fat_hunter, Fluents, OldH), get_dict(c, OldH, OldPos)
       -> OldX = OldPos.x, OldY = OldPos.y
       ;  OldX = 1, OldY = 1, OldPos = c{x:1, y:1} 
       ),

       NewPos = c{x:NX, y:NY},

       % Mise à jour de la liste 'visited' seulement si on a bougé
       ( (NX \= OldX ; NY \= OldY)
       -> 
           NewVisit = v{to:NewPos, from:OldPos},
           ( get_dict(visited, Fluents, OldVisited) -> true ; OldVisited = [] ),
           NewVisited = [NewVisit | OldVisited]
       ;
           ( get_dict(visited, Fluents, OldVisited) -> NewVisited = OldVisited ; NewVisited = [] )
       ),

       % Mise à jour hunter
       ( get_dict(fat_hunter, Fluents, OldFat) 
       -> put_dict(c, OldFat, NewPos, NewFat) 
       ;  NewFat = fat{c:NewPos} 
       ),

       % Mise à jour direction
       NewDirObj = d{d:NDir, h:hunter{id:hunter}},

       % Reconstruction du dict
       put_dict(fat_hunter, Fluents, NewFat, F1),
       put_dict(dir, F1, [NewDirObj], F2),
       put_dict(visited, F2, NewVisited, FFinal),

       put_dict(certain_fluents, BeliefsDict, FFinal, NewBeliefsDict)
    ;  
       NewBeliefsDict = BeliefsDict 
    ).