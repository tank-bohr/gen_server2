-module(gen_server2).
-include("gen_server2.hrl").

-export([start/3]).

-export([
    send/2,
    wait/1,
    wait/2,
    call/2,
    call/3,
    reply/2
]).

-export([loop/2]).

-type state()   :: tuple().
-type return()  :: #ok{} | #stop{}.
-type from()    :: {pid(), reference()}.
-type request() :: tuple().

-callback proc(Request, State) -> return() when
    Request :: request(),
    State   :: state().

-callback proc(Request, From, State) -> return() when
    Request :: request(),
    From    :: from(),
    State   :: state().

-optional_callbacks([proc/3]).

-record(sync, {
    request :: request(),
    from    :: from()
}).

-record(state, {
    module :: module(),
    state  :: state()
}).

-define(DEFAULT_TIMEOUT, 5000).

start(Module, State, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    SpawnOpts = proplists:get_value(spawn_opt, Options, []),
    spawn_opt(fun() -> loop(#state{module = Module, state = State}, Timeout) end, SpawnOpts).

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
    ?FUNCTION_NAME(Server, Request, ?DEFAULT_TIMEOUT).

call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.

loop(State, Timeout, _Hibernate = true) ->
    erlang:hibernate(?MODULE, ?FUNCTION_NAME, [State, Timeout]);
loop(State, Timeout, _) ->
    loop(State, Timeout).

%% @private
loop(State, Timeout) ->
    receive
        #sync{request = Request, from = From} ->
            proc_sync(Request, From, State);
        Request ->
            proc(Request, State)
    after Timeout ->
        self() ! timeout
    end.

proc_sync(Request, From, State) ->
    case call_proc(Request, From, State) of
        #ok{reply = ?NOREPLY, state = StateOut, timeout = T, hibernate = H} ->
            loop(State#state{state = StateOut}, T, H);
        #ok{reply = Reply, state = StateOut, timeout = T, hibernate = H} ->
            reply(From, Reply),
            loop(State#state{state = StateOut}, T, H);
        #stop{reply = ?NOREPLY, state = StateOut, reason = Reason} ->
            terminate(Reason, StateOut);
        #stop{reply = Reply, state = StateOut, reason = Reason} ->
            reply(From, Reply),
            terminate(Reason, StateOut)
    end.

proc(Request, #state{module = Module, state = StateIn} = State) ->
    case Module:proc(Request, StateIn) of
        #ok{state = StateOut, timeout = T, hibernate = H} ->
            loop(State#state{state = StateOut}, T, H);
        #stop{state = StateOut, reason = Reason} ->
            terminate(Reason, StateOut)
    end.

call_proc(Request, From, #state{module = Module, state = StateIn}) ->
    case erlang:function_exported(Module, proc, 3) of
        true ->
            Module:proc(Request, From, StateIn);
        false ->
            Module:proc(Request, StateIn)
    end.

terminate(_Reason, _State) ->
    ok.
