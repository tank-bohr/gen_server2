-module(gen_server2).

-include("gen_server2.hrl").

-export([
    start/2,
    start_link/2,
    stop/1,
    stop/3,
    call/2,
    call/3,
    send/2,
    wait/1,
    wait/2,
    reply/2
]).

-export([loop/3]).

-define(INIT, '$gen_init').
-define(SEND, '$gen_send').
-define(STOP, '$gen_stop').
-define(DEFAULT_TIMEOUT, 5000).
-define(IS_HIBERNATE(H), H =:= true orelse H =:= hibernate).

-callback proc(Request, FromOrReason, State) -> #ok{} | #reply{} | #stop{} when
    Request      :: init | terminate | term(),
    FromOrReason :: pid() | {pid(), reference()} | term(),
    State        :: term().

start(Module, Args) ->
    init(spawn(fun() -> loop(Module, Args, 0, false) end)).

start_link(Module, Args) ->
    init(spawn_link(fun() -> loop(Module, Args, 0, false) end)).

stop(Server) ->
    stop(Server, normal, infinity).

stop(Server, Reason, Timeout) ->
    wait(send(Server, Reason, ?STOP), Timeout).

call(Server, Request) ->
    call(Server, Request, ?DEFAULT_TIMEOUT).

call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

send(Server, Request) ->
    send(Server, Request, ?SEND).

send(Server, Request, Type) ->
    Tag = make_ref(),
    Server ! {Type, {self(), Tag}, Request},
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

reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.

init(Pid) ->
    case wait(send(Pid, init, ?INIT)) of
        ok ->
            {ok, Pid};
        ignore ->
            ignore;
        Else ->
            {error, Else}
    end.

loop(Module, State, Timeout, Hibernate) when ?IS_HIBERNATE(Hibernate) ->
    erlang:hibernate(?MODULE, loop, [Module, State, Timeout]);
loop(Module, State, Timeout, _) ->
    loop(Module, State, Timeout).

%% @private
loop(Module, State, Timeout) ->
    receive
        {?INIT, From, Request} ->
            proc_init(Module, State, From, Request);
        {?SEND, From, Request} ->
            proc(Module, State, From, Request);
        {?STOP, From, Reason} ->
            terminate(Module, Reason, State),
            reply(From, ok);
        Info ->
            proc(Module, State, undefined, Info)
    after Timeout ->
        self() ! timeout
    end.

proc_init(Module, State, From, Request) ->
    case Module:proc(Request, From, State) of
        #ok{state = NewState, timeout = T, hibernate = H} ->
            reply(From, ok),
            loop(Module, NewState, T, H);
        #stop{reason = Reason, state = NewState} ->
            reply(From, Reason),
            terminate(Module, Reason, NewState);
        ignore ->
            reply(From, ignore),
            terminate(Module, ignore, State)
    end.

proc(Module, State, From, Request) ->
    case Module:proc(Request, From, State) of
        #ok{state = NewState, timeout = T, hibernate = H} ->
            loop(Module, NewState, T, H);
        #reply{reply = Reply, state = NewState, timeout = T, hibernate = H} ->
            reply(From, Reply),
            loop(Module, NewState, T, H);
        #stop{reason = Reason, reply = ?NOREPLY, state = NewState} ->
            terminate(Module, Reason, NewState);
        #stop{reason = Reason, reply = Reply, state = NewState} ->
            reply(From, Reply),
            terminate(Module, Reason, NewState)
    end.

terminate(Module, Reason, State) ->
    Module:proc(terminate, Reason, State).
