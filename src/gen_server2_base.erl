-module(gen_server2_base).
-include("gen_server2.hrl").

-behaviour(gen_server2).
-export([proc/2]).

-record(base, {
    command = cont     :: start | cont | stop,
    timeout = infinity :: infinity | non_neg_integer(),
    hibernate = false  :: boolean(),
    spawn_opt = []     :: list()
}).

proc(#base{hibernate = true} = Request, State) ->
    erlang:hibernate(?MODULE, ?FUNCTION_NAME, [Request#base{hibernate = false}, State]);
proc(#base{command = start, spawn_opt = SpawnOpt} = Request, State) ->
    spawn_opt(fun() -> proc(Request#base{command = cont}, State) end);
proc(#base{command = cont, timeout = Timeout}, State) ->
    receive
        Request ->
            loop(Request, State)
    after Timeout ->
        self() ! timeout
    end.
proc(#base{command = stop}, State) ->
    stop.

loop(Request, StateIn) ->
    {Response, StateOut} = gen_server2:proc_for(?MODULE, Request, StateIn),
    proc(#base{
        command = maps:get(command, Response, cont),
        timeout = maps:get(timeout, Response, infinity),
        hibernate = maps:get(hibernate, Response, false)
    }, StateOut}.
