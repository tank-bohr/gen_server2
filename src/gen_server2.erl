-module(gen_server2).

-export([
    start/2,
    start_link/2,
    stop/1,
    stop/3,
    call/2,
    call/3,
    cast/2,
    reply/2
]).

-define(CALL, '$gen_call').
-define(CAST, '$gen_cast').
-define(STOP, '$gen_stop').

-callback init(term()) -> {ok, term()} | {stop, term()}.
-callback handle_call(atom(), term(), {pid(), reference()}) ->
    {reply, term()} |
    {noreply, term()} |
    {stop, term(), term(), term()} |
    {stop, term(), term()}.
-callback handle_cast(atom(), term()) ->
    {noreply, term()} | {stop, term(), term()}.
-callback handle_info(term(), term()) ->
    {noreply, term()} | {stop, term(), term()}.
-callback terminate(term(), term()) -> any().

start(Module, Args) ->
    spawn(fun() -> init(self(), Module, Args) end).

start_link(Module, Args) ->
    spawn_link(fun() -> init(self(), Module, Args) end).

stop(Server) ->
    stop(Server, normal, infinity).

stop(Server, Reason, Timeout) ->
    send_and_wait(Server, ?STOP, Reason, Timeout).

call(Server, Request) ->
    call(Server, Request, 5000).

call(Server, Request, Timeout) ->
    send_and_wait(Server, ?CALL, Request, Timeout).

cast(Server, Request) ->
    Server ! {?CAST, Request},
    ok.

reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.

init(Parent, Module, Args) ->
    put('$ancestors', [Parent]),
    put('$initial_call', {Module, init, 1}),
    case Module:init(Args) of
        {ok, State} ->
            loop(Module, State);
        {stop, Reason} ->
            terminate(Module, unefined, Reason)
    end.

loop(Module, State) ->
    receive
        {?CALL, From, Request} ->
            handle_call(Module, State, From, Request);
        {?CAST, Request} ->
            handle_cast(Module, State, Request);
        {?STOP, Reason} ->
            terminate(Module, State, Reason);
        Info ->
            handle_info(Module, State, Info)
    end.

handle_call(Module, State, From, Request) ->
    case Module:handle_call(Request, From, State) of
        {reply, Reply, NewState} ->
            reply(From, Reply),
            loop(Module, NewState);
        {noreply, NewState} ->
            loop(Module, NewState);
        {stop, Reason, Reply, NewState} ->
            reply(From, Reply),
            terminate(Module, NewState, Reason);
        {stop, Reason, NewState} ->
            terminate(Module, NewState, Reason)
    end.

handle_cast(Module, State, Request) ->
    case Module:handle_cast(Request, State) of
        {noreply, NewState} ->
            loop(Module, NewState);
        {stop, Reason, NewState} ->
            terminate(Module, NewState, Reason)
    end.

handle_info(Module, State, Info) ->
    case Module:handle_info(Info, State) of
        {noreply, NewState} ->
            loop(Module, NewState);
        {stop, Reason, NewState} ->
            terminate(Module, NewState, Reason)
    end.

send_and_wait(Server, Type, Message, Timeout) ->
    Tag = make_ref(),
    Server ! {Type, {self(), Tag}, Message},
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
        timeout
    end.

terminate(Module, State, Reason) ->
    Module:terminate(State, Reason).
