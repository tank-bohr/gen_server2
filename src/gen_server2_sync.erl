-module(gen_server2_sync).
-include("gen_server2.hrl").

-export([start/2]).

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

-callback proc(Request, State) -> gen_server2:return() when
    Request :: gen_server2:request(),
    State   :: gen_server2:state().

-callback proc(Request, From, State) -> gen_server2:return() when
    Request :: gen_server2:request(),
    From    :: from(),
    State   :: gen_server2:state().

-optional_callbacks([proc/3]).

-record(sync, {
    request :: gen_server2:request(),
    from    :: from()
}).

-record(state, {
    module           :: module(),
    state = ?NOSTATE :: gen_server2:state()
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

proc(#sync{request = InitalState, from = From}, ?NOSTATE) ->
    reply(From, self()),
    #ok{state = InitalState};
proc(#sync{request = Request, from = From}, State) ->
    case call_proc(Request, From, State) of
        #ok{reply = ?NOREPLY, state = StatOut} = Ok ->
            Ok#ok{state = State#state{state = StatOut}};
        #ok{reply = Reply, state = StatOut} = Ok ->
            reply(From, Reply),
            Ok#ok{state = State#state{state = StatOut}};
        #stop{reply = ?NOREPLY, state = StatOut} = Stop ->
            Stop#stop{state = State#state{state = StatOut}};
        #stop{reply = Reply, state = StatOut} = Stop ->
            reply(From, Reply),
            Stop#stop{state = State#state{state = StatOut}}
    end.

call_proc(Request, From, #state{module = Module, state = StateIn}) ->
    case erlang:function_exported(Module, proc, 3) of
        true ->
            Module:proc(Request, From, StateIn);
        false ->
            Module:proc(Request, StateIn)
    end.
