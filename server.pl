#!/usr/bin/env swipl

% --- Libraries ---
:- use_module(library(main)).
:- use_module(library(settings)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_dispatch)).

% --- Settings ---
:- set_setting(http:logfile, 'httpd_sim.log').
:- set_setting(http:cors, [*]).

% --- CORS Enable ---
:- cors_enable.

% --- Debugging ---
:- debug.

% --- Modules ---
:- use_module(wumpus).
% (On n'importe pas ontology ici, c'est pour le hunter ou le simulateur interne)

% --- Run HTTP Server sur le port 8080 ---
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
% NOTE : Pas de handler /action ici ! C'est le job du Hunter.

% --- Handler /default ---
handle_default_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
handle_default_request(_) :-
    wumpus_example(SampleDict),
    cors_enable,
    reply_json_dict(SampleDict).

% --- Handler /init ---
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

% --- Handler /sim ---
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
    
    % Log pour vérifier ce qui se passe
    http_log('SIM: Received Action ~w~n', [Action]),
    
    sim_step(Eternals, Fluents, PreviousFluents, Action, NewFluents, Percepts),
    cors_enable,
    reply_json_dict(_{fluents: NewFluents, percepts: Percepts}).