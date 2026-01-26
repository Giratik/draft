#!/usr/bin/env swipl

% --- Libraries ---
:- use_module(library(main)).
:- use_module(library(settings)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).

% --- Settings ---
:- set_setting(http:logfile, 'httpd.log').
:- set_setting(http:cors, [*]).

% --- CORS Enable ---
:- cors_enable.

% --- Debugging ---
:- debug.

% --- Modules ---
:- use_module(wumpus).
:- use_module(state). % Assuming state.pl contains your custom state predicates
:- use_module(world).


%:- initialization(run, main).

% --- Run HTTP Server ---
run :-
    http_log_stream(_),
    debug(http(_)),
    ( current_prolog_flag(windows, true) ->
        http_server(http_dispatch, [port(8080)])
        ;
        http_daemon([fork(false), interactive(false), port(8080)])
    ).

% --- HTTP Handlers ---
:- http_handler(root(init), handle_init_request, []).
:- http_handler(root(default), handle_default_request, []).
:- http_handler(root(sim), handle_sim_request, []).
:- http_handler(root(action), handle_action_request, []).

% --- Handler for /default endpoint ---
handle_default_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
handle_default_request(_) :-
    wumpus_example(SampleDict),
    cors_enable,
    reply_json_dict(SampleDict).

% --- Handler for /init endpoint ---
handle_init_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
handle_init_request(Request) :-
    http_read_json_dict(Request, RequestJSON),
    _{size: Size} :< RequestJSON,
    init(Size, WorldDict, InitPercetps),
    cors_enable,
    reply_json_dict(_{state:WorldDict, percepts:InitPercetps}).

% --- Handler for /sim endpoint ---
handle_sim_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
handle_sim_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom)]),
    _{
        fluents: Fluents, eternals: Eternals,
        previous_fluents:PreviousFluents,
        plan: Action
    } :< RequestJSON,
    http_log_stream(Stream),
    print_term(_{
        fluents: Fluents, eternals: Eternals,
        previous_fluents:PreviousFluents,
        plan: Action
    }, [output(Stream),nl(true)]),
    sim_step(Eternals, Fluents, PreviousFluents, Action, NewFluents, Percepts),
    cors_enable,
    reply_json_dict(_{fluents: NewFluents, percepts: Percepts}).




get_state(RawState, State) :-
    ( get_dict(certain_fluents, RawState, CF)
    ->  ( get_dict(certain_eternals, RawState, CE)
        -> State = CF.put(eternals, CE)
        ;  State = CF
        )
    ;   State = RawState
    ).

get_beliefs(RawState, State) :-
    get_state(RawState, State).


/*
%% OUR REASONING LOGIC

  1. Bump Percept:
     - If a bump is sensed, we turn.
     - The turning move is determined by examining the previous decision:
         - If the last decision was "left-forward", then choose "right-forward".
         - If it was "right-forward", then choose "left-forward".
         - Otherwise, default to "left-forward".
  
  2. Glitter Percept:
     - If glitter is sensed, choose "grab".
  
  3. Stench Percept:
     - If a stench is sensed,check if the wumpus is still alive and we go to the last case visited if yes.
 	- If the stench and three adjacent case are visited, shoot in the unvisited case.
     - A helper predicate (wumpus_is_alive/1) looks at a flag in the state ("wumpusDead") that is updated when a scream is received.
     - If the flag is false, the wumpus is alive then choose "shoot".
  
  4. Breeze Percept:
     - If a breeze is sensed, we go to the last case visited.
  
  5. Default Rule:
     - We go forward.*/


wumpus_is_alive(Beliefs) :-
    ( get_dict(wumpusDead, Beliefs, WD) -> WD = false ; true ).

move_back([Last|_], move(Last)).

adjacent_visited(Visited, Count) :-
    findall(X, member(X, Visited), Adj),
    length(Adj, Count).

update_hunter_state(State, Percepts, Action, NewBeliefs) :-
    ( get_dict(step, State, Step) -> true ; Step = 0 ),
    NewStep is Step + 1,
    ( member(scream, Percepts)
    ->  NewWumpusDead = true
    ;   ( get_dict(wumpusDead, State, WD)
          -> NewWumpusDead = WD
          ;  NewWumpusDead = false
        )
    ),
    NewBeliefs = State.put(step, NewStep)
                     .put(lastDecision, Action)
                     .put(lastPercepts, Percepts)
                     .put(wumpusDead, NewWumpusDead).

decide_action(Percepts, Beliefs, SafeCells, BreezeSuspectCells,StenchSuspectCells,PitCells, Action, NewBeliefs) :-
    ( member(bump, Percepts) ->
        ( get_dict(lastDecision, Beliefs, LastDecision) ->
            ( LastDecision = left_forward -> Action = right_forward
            ; LastDecision = right_forward -> Action = left_forward
            ; Action = left_forward )
        ; Action = left_forward )
    ; member(glitter, Percepts) 
    ->  Action = grab
    ; member(stench, Percepts), wumpus_is_alive(Beliefs) -> move_back(Visited, Action)
    ; member(stench, Percepts), adjacent_visited(Visited, Count), Count >= 3 -> Action = shoot
    ; member(breeze, Percepts) -> move_back(Visited, Action)
    ; Action = forward ),
    update_hunter_state(State, Percepts, Action, NewBeliefs).


%% --- HTTP /action handler ---
handle_action_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put]), request_headers(['content-type'])]),
    format('Content-type: text/plain~n~n').

handle_action_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom)]),
    cors_enable,
    http_log('DEBUG: Received JSON: ~q~n', [RequestJSON]),
    ( get_dict(percepts, RequestJSON, Percepts)
      -> true
      ;  Percepts = []
    ),
    % Extract state: try hunterState first; if not, then try beliefs.
    ( get_dict(hunterState, RequestJSON, RawState)
      -> get_state(RawState, State)
      ; ( get_dict(beliefs, RequestJSON, B)
          -> ( get_dict(certain_fluents, B, CF),
               get_dict(certain_eternals, B, CE),
               State = CF.put(eternals, CE)
             )
          ; State = _{step: 0, visited: []}
        )
    ),
    
   decide_action(Percepts, Beliefs, SafeCells, BreezeSuspectCells,StenchSuspectCells,PitCells, Action, NewBeliefs), % Decide the hunters action

    reply_json_dict(_{ hunterState: NewBeliefs, action: Action }). % Reply with the updated state and the chosen action in JSON
    