-module(gen_server2).
%% @doc Drop-in replacement for the OTP gen_server.
-include("srv.hrl").
-behaviour(srv).
-export([
    proc/2,
    proc/3
]).

-export([
    start/3,
    start_link/3,
    call/2,
    call/3,
    cast/2,
    reply/2,
    stop/1,
    stop/3,
    send_request/2,
    wait_response/2
]).

-type reply()    :: term().
-type reason()   :: term().
-type gs_state() :: term().

-type init_result() :: {ok, gs_state()} |
                       {ok, gs_state(), timeout()} |
                       {ok, gs_state(), hibernate} |
                       {ok, gs_state(), {continue, term()}} |
                       {stop, reason()} |
                       ignore.

-type call_result() :: {reply, reply(), NewState :: gs_state()} |
                       {reply, reply(), NewState :: gs_state(), timeout()} |
                       {reply, reply(), NewState :: gs_state(), hibernate} |
                       {reply, reply(), NewState :: gs_state(), {continue, term()}} |
                       {noreply, NewState :: gs_state()} |
                       {noreply, NewState :: gs_state(), timeout()} |
                       {noreply, NewState :: gs_state(), hibernate} |
                       {noreply, NewState :: gs_state(), {continue, term()}} |
                       {stop, reason(), NewState :: gs_state()} |
                       {stop, reason(), reply(), NewState :: gs_state()}.

-type cast_result() :: {noreply, gs_state()} |
                       {noreply, gs_state(), timeout()} |
                       {noreply, gs_state(), hibernate} |
                       {noreply, gs_state(), {continue, term()}} |
                       {stop, reason(), gs_state()}.

-type info_result() :: {noreply, NewState :: gs_state()} |
                       {noreply, NewState :: gs_state(), timeout()} |
                       {noreply, NewState :: gs_state(), hibernate} |
                       {stop, reason(), NewState :: gs_state()}.

-callback init(Args :: term()) -> init_result().

-callback handle_call(Request, From, State) -> Result when
    Request :: term(),
    From    :: {pid(), Tag},
    State   :: gs_state(),
    Tag     :: reference(),
    Result  :: call_result().

-callback handle_cast(Request, State) -> Result when
    Request :: term(),
    State   :: gs_state(),
    Result  :: cast_result().

-callback handle_info(Info, State) -> Result when
    Info   :: timeout | term(),
    State  :: gs_state(),
    Result :: info_result().

-callback handle_continue(Continue, State) -> Result when
    Continue :: term(),
    State    :: gs_state(),
    Result   :: cast_result().

-callback terminate(Reason, State) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State  :: gs_state().

-optional_callbacks([
    handle_continue/2,
    terminate/2
]).

-record(state, {
    gs_state  :: gs_state(),
    cb_module :: module()
}).

-record(init, {
    args :: term()
}).

-record(cast, {
    request :: term()
}).

-record(call, {
    request :: term()
}).

-record(continue, {
    payload :: term()
}).

-spec start(module(), term(), list()) -> {ok, pid()} | ignore | {error, term()}.
start(Module, Args, Options) ->
    Pid = srv:start(?MODULE, #state{cb_module = Module}, proplist_to_map(Options)),
    case srv:call(Pid, #init{args = Args}) of
        ok -> {ok, Pid};
        Err -> Err
    end.

-spec start_link(module(), term(), list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Module, Args, Options) ->
    SpawnOpts = proplists:get_value(spawn_opt, Options, []),
    SpawnOpts1 = [link | SpawnOpts],
    Options1 = lists:keyreplace(spawn_opt, 1, Options, {spawn_opt, SpawnOpts1}),
    start(Module, Args, Options1).

-spec call(pid(), term()) -> term().
call(Pid, Request) ->
    call(Pid, Request, 5000).

-spec call(pid(), term(), timeout()) -> term().
call(Pid, Request, Timeout) ->
    srv:call(Pid, #call{request = Request}, Timeout).

-spec cast(pid(), term()) -> ok.
cast(Pid, Request) ->
    _Tag = srv:send(Pid, #cast{request = Request}),
    ok.

-spec reply(srv:from(), term()) -> term().
reply(Client, Reply) ->
    srv:reply(Client, Reply).

-spec stop(pid()) -> ok.
stop(Pid) ->
    srv:send(Pid, #stop{}),
    ok.

-spec stop(pid(), term(), timeout()) -> ok | no_return().
stop(Pid, Reason, Timeout) ->
    MonRef = monitor(process, Pid),
    srv:send(Pid, #stop{reason = Reason}),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            ok
    after Timeout ->
        exit(timeout)
    end.

send_request(Pid, Request) ->
    srv:send(Pid, Request).

wait_response(RequestId, Timeout) ->
    srv:wait(RequestId, Timeout).

proc(#init{args = Args}, #state{cb_module = Mod} = State) ->
    case Mod:init(Args) of
        {ok, GsState} ->
            #ok{reply = ok, state = State#state{gs_state = GsState}};
        {ok, GsState, hibernate} ->
            #ok{reply = ok, state = State#state{gs_state = GsState}, hibernate = true};
        {ok, GsState, {continue, Continue}} ->
            srv:send(self(), #continue{payload = Continue}),
            #ok{reply = ok, state = State#state{gs_state = GsState}};
        {ok, GsState, Timeout} ->
            #ok{reply = ok, state = State#state{gs_state = GsState}, timeout = Timeout};
        {stop, Reason} ->
            #stop{reply = {error, Reason}, reason = Reason, state = State};
        ignore ->
            #stop{reply = ignore, reason = normal, state = State}
    end;
proc(#cast{request = Request}, #state{cb_module = Mod, gs_state = GsState} = State) ->
    handle(Mod:handle_cast(Request, GsState), State);
proc(#continue{payload = Payload}, #state{cb_module = Mod, gs_state = GsState} = State) ->
    handle(Mod:handle_continue(Payload, GsState), State);
proc(#stop{reason = Reason}, #state{cb_module = Mod, gs_state = GsState} = State) ->
    case Mod:function_exported(terminate, 3) of
        true ->
            Mod:terminate(Reason, GsState),
            #stop{reason = Reason, state = State};
        false ->
            #stop{reason = Reason, state = State}
    end;
proc(Request, #state{cb_module = Mod, gs_state = GsState} = State) ->
    handle(Mod:handle_info(Request, GsState), State).

proc(#call{request = Request}, From, #state{cb_module = Mod, gs_state = GsState} = State) ->
    case Mod:handle_call(Request, From, GsState) of
        {reply, Reply, NewState} ->
            #ok{reply = Reply, state = State#state{gs_state = NewState}};
        {reply, Reply, NewState, hibernate} ->
            #ok{reply = Reply, state = State#state{gs_state = NewState}, hibernate = true};
        {reply, Reply, NewState, Timeout} ->
            #ok{reply = Reply, state = State#state{gs_state = NewState}, timeout = Timeout};
        {noreply, NewState} ->
            #ok{state = State#state{gs_state = NewState}};
        {noreply, NewState, hibernate} ->
            #ok{state = State#state{gs_state = NewState}, hibernate = true};
        {noreply, NewState, Timeout} ->
            #ok{state = State#state{gs_state = NewState}, timeout = Timeout};
        {stop, Reason, NewState} ->
            #stop{reason = Reason, state = State#state{gs_state = NewState}};
        {stop, Reason, Reply, NewState} ->
            #stop{reply = Reply, reason = Reason, state = State#state{gs_state = NewState}}
    end.

handle(CallbackResult, State) ->
    case CallbackResult of
        {noreply, NewState} ->
            #ok{state = State#state{gs_state = NewState}};
        {noreply, NewState, hibernate} ->
            #ok{state = State#state{gs_state = NewState}, hibernate = true};
        {noreply, NewState, {continue, Continue}} ->
            srv:send(self(), #continue{payload = Continue}),
            #ok{state = State#state{gs_state = NewState}};
        {noreply, NewState, Timeout} ->
            #ok{state = State#state{gs_state = NewState}, timeout = Timeout};
        {stop, Reason, NewState} ->
            #stop{reason = Reason, state = State#state{gs_state = NewState}}
    end.

proplist_to_map(List) ->
    proplist_to_map(List, #{}).

proplist_to_map([], Map) ->
    Map;
proplist_to_map([{K,V} | List], Map) ->
    proplist_to_map(List, Map#{K => V}).
