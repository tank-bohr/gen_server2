-module(srv).
-include("srv.hrl").

-export([
    start/3,
    send/2,
    wait/1,
    wait/2,
    call/2,
    call/3,
    reply/2
]).

-export([loop/2]).

-type return()  :: #ok{} | #stop{}.
-type request() :: tuple().
-opaque from()  :: {pid(), reference()}.

-export_type([from/0]).

-callback proc(Request, State) -> return() when
    Request :: request(),
    State   :: tuple().

-callback proc(Request, From, State) -> return() when
    Request :: request(),
    From    :: from(),
    State   :: tuple().

-optional_callbacks([proc/3]).

-record(sync, {
    request :: request(),
    from    :: from()
}).

-record(state, {
    module :: module(),
    state  :: tuple()
}).

-define(DEFAULT_TIMEOUT, 5000).

-spec start(module(), tuple(), map()) -> pid().
start(Module, State, Options) ->
    {Timeout, SpawnOpts} = case Options of
        #{timeout := T, spawn_opt := SO} ->
            {T, SO};
        #{timeout := T} ->
            {T, []};
        #{spawn_opt := SO} ->
            {infinity, SO};
        #{} ->
            {infinity, []}
    end,
    spawn_opt(fun() -> loop(#state{module = Module, state = State}, Timeout) end, SpawnOpts).

-spec send(pid(), term()) -> reference().
send(Server, Request) ->
    Tag = make_ref(),
    Server ! #sync{request = Request, from = {self(), Tag}},
    Tag.

-spec wait(reference()) -> term().
wait(Tag) ->
    wait(Tag, ?DEFAULT_TIMEOUT).

-spec wait(reference(), timeout()) -> term().
wait(Tag, Timeout) ->
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
        timeout
    end.

-spec call(pid(), term()) -> term().
call(Server, Request) ->
    call(Server, Request, ?DEFAULT_TIMEOUT).

-spec call(pid(), term(), timeout()) -> term().
call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

-spec reply(from(), term()) -> {reference(), term()}.
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

terminate(Reason, _State) ->
    exit(Reason).
