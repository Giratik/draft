:- module(logic, [run_hunter/0]).

% --- 1. IMPORTS & CONFIGURATION ---
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(clpfd)).
:- use_module(library(pita)).

:- pita. % Initialisation du moteur probabiliste

:- set_setting(http:cors, [*]).
:- cors_enable.

% --- 2. SERVEUR HTTP ---

run_hunter :-
    http_server(http_dispatch, [port(8081)]),
    format(user_error, '~n[SERVER] Hunter (Syntax Fixed) running on 8081...~n', []).

:- http_handler(root(action), handle_hunter_request, []).

% ----------------------------------------------------------------------
% 1. GESTION DU PREFLIGHT (OPTIONS)
% ----------------------------------------------------------------------
handle_hunter_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [
        methods([get, post, put, options]),
        headers(['Content-Type'])
    ]),
    format('Content-type: text/plain\r\n\r\n').

% ----------------------------------------------------------------------
% 2. GESTION DE LA REQUÊTE PRINCIPALE (PUT)
% ----------------------------------------------------------------------
handle_hunter_request(Request) :-
    cors_enable(Request, [methods([put])]),
    
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    
    ( _{ beliefs: BeliefsDict, percepts: RawPercepts } :< RequestJSON
    -> true 
    ;  BeliefsDict = _{}, RawPercepts = [] 
    ),
    
    (is_list(RawPercepts) -> Percepts = RawPercepts ; Percepts = []),

    % --- Suite de votre logique (Cerveau) ---
    
    % A. Extraction (CORRIGÉE : Plus de Pos.x ici)
    extract_pos_dir(BeliefsDict, X, Y, Dir, Visited),
    format(user_error, '~n[ETAT] Position: (~w,~w) Face: ~w~n', [X, Y, Dir]),

    % B. Identification Cible
    get_front_cell(X, Y, Dir, FrontX, FrontY),

    % C. Preuves
    build_evidence(X, Y, Percepts, Visited, Evidence),

    % D. Décision
    ( member(glitter, Percepts) ->
        Action = grab, DebugMsg = 'GOLD!'
    ;
        Query = safe(FrontX, FrontY),
        ( catch(prob(Query, Evidence, ProbSafe), _, fail) -> true ; ProbSafe = 0.0 ),
        
        format(user_error, '[ANALYSE] Devant (~w,~w) Surete: ~2f~n', [FrontX, FrontY, ProbSafe]),

        ( ProbSafe > 0.7 ->
            Action = move, format(atom(DebugMsg), 'Move P=~2f', [ProbSafe])
        ;
            Action = right, format(atom(DebugMsg), 'Turn P=~2f', [ProbSafe])
        )
    ),

    format(user_error, '[DECISION] ~w~n', [Action]),

    % E. Réponse
    Response = _{
        hunterState: _{ beliefs: BeliefsDict, percepts: Percepts },
        action: Action,
        debug: DebugMsg
    },
    
    reply_json_dict(Response).


% --- 3. MODÈLE PROBABILISTE (LPAD) ---

:- begin_lpad.

% A. Priors
pit(X,Y):0.2 :- valid_grid(X,Y), \+ start_pos(X,Y).
wumpus(X,Y):0.07 :- valid_grid(X,Y), \+ start_pos(X,Y).

% B. Causalité
breeze(X,Y) :- neighbor(X,Y, NX,NY), pit(NX,NY).
stench(X,Y) :- neighbor(X,Y, NX,NY), wumpus(NX,NY).

% C. Définitions
safe(X,Y) :- \+ pit(X,Y), \+ wumpus(X,Y).

start_pos(1,1).
valid_grid(X,Y) :- member(X, [0,1,2,3,4,5]), member(Y, [0,1,2,3,4,5]).

neighbor(X,Y, NX,NY) :-
    valid_grid(X,Y),
    ( NX is X+1, NY is Y
    ; NX is X-1, NY is Y
    ; NX is X,   NY is Y+1
    ; NX is X,   NY is Y-1
    ),
    valid_grid(NX,NY).

:- end_lpad.


% --- 4. UTILITAIRES ---

get_front_cell(X, Y, north, X, NY) :- NY is Y + 1.
get_front_cell(X, Y, south, X, NY) :- NY is Y - 1.
get_front_cell(X, Y, east, NX, Y)  :- NX is X + 1.
get_front_cell(X, Y, west, NX, Y)  :- NX is X - 1.

build_evidence(X, Y, Percepts, VisitedList, Evidence) :-
    ( member(breeze, Percepts) -> BFact = breeze(X,Y) ; BFact = \+ breeze(X,Y) ),
    ( member(stench, Percepts) -> SFact = stench(X,Y) ; SFact = \+ stench(X,Y) ),
    findall(\+ pit(VX, VY), member([VX, VY], VisitedList), PitFacts),
    findall(\+ wumpus(Wx, Wy), member([Wx, Wy], VisitedList), WumpusFacts),
    append([BFact, SFact | PitFacts], WumpusFacts, Evidence).


% --- 5. EXTRACTION JSON (CORRIGÉE) ---

extract_pos_dir(BeliefsDict, X, Y, Dir, Visited) :-
    F = BeliefsDict.certain_fluents,
    
    % Position (Correction: on utilise get_dict au lieu de Pos.x)
    get_dict(fat_hunter, F, H), 
    get_dict(c, H, Pos),
    get_dict(x, Pos, Px), safe_num(Px, X), 
    get_dict(y, Pos, Py), safe_num(Py, Y),
    
    % Direction
    ( get_dict(dir, F, DL), member(D, DL), get_dict(d, D, DirAtom)