#!/usr/bin/env swipl

:- use_module(library(main)).
:- use_module(library(settings)).

:- use_module(library(http/http_server)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_cors)).

:- set_setting(http:logfile, 'httpd.log').
:- set_setting(http:cors, [*]).

:- cors_enable.

:- debug.

:- use_module(wumpus).

:- initialization(run, main).

run :- 
    http_log_stream(_),
    debug(http(_)),
    ( current_prolog_flag(windows, true) ->
        http_server(http_dispatch, [port(8080)])
        ;
        http_daemon([fork(false), interactive(false), port(8080)])
    ).

:- http_handler(root(init), handle_init_request, []).
:- http_handler(root(default), handle_default_request, []).
:- http_handler(root(sim), handle_sim_request, []).

handle_default_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
handle_default_request(_) :-
    wumpus_example(SampleDict),
    cors_enable,
    reply_json_dict(SampleDict).

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

handle_sim_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request, [methods([put])]),
    format('Content-type: text/plain\r\n'),
    format('~n').
    
handle_sim_request(Request) :-
    http_read_json_dict(Request, RequestJSON, [value_string_as(atom), tag('')]),
    _{
        fluents: RawFluents, eternals: RawEternals, 
        previous_fluents:RawPreviousFluents,
        plan: RawAction
    } :< RequestJSON,
    untag(RawFluents, Fluents),
    untag(RawEternals, Eternals),
    untag(RawPreviousFluents, PreviousFluents),
    untag(RawAction, Action),
    http_log_stream(Stream),
    print_term(_{
        fluents: Fluents, eternals: Eternals,
        previous_fluents:PreviousFluents,
        plan: Action
    }, [output(Stream),nl(true)]),
    sim_step(Eternals, Fluents, PreviousFluents, Action, NewFluents, Percepts),
    cors_enable,
    reply_json_dict(_{fluents: NewFluents, percepts: Percepts}).

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