-module(gen_server2_sync).
-include("gen_server2.hrl").

-export([
    send/2,
    wait/1,
    wait/2,
    call/2,
    call/3,
    reply/2
]).

-behaviour(gen_server2).
-export([proc/2]).

-type from() :: {pid(), reference()}.
-export_type([from/0]).

-record(sync, {
    request :: gen_server2:request(),
    from    :: from()
}).

-define(DEFAULT_TIMEOUT, 5000).

start(Module, Options) ->
    Server = gen_server2:start(?MODULE, Options),
    call(Server, #state{module = Module}).

send(Server, Request) ->
    Tag = make_ref(),
    Server ! #sync{request = Request, from = {self(), Tag}},
    Tag.

wait(Tag) ->
    wait(Tag, ?DEFAULT_TIMEOUT).

wait(Tag, Timeout) ->
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
        timeout
    end.

call(Server, Request) ->
    call(Server, Request, ?DEFAULT_TIMEOUT).

call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.

proc(#sync{request = Request, from = From}, StateIn) ->
    {Response, StateOut} = gen_server2:proc_for(?MODULE, Request, StateIn),
    case maps:get(sync, Response) of
        #{reply := Reply} ->
            reply(From, Reply);
        Else ->
            ok
    end,
    StateOut.
