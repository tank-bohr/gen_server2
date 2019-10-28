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

-define(STOP, '$gen_stop').
-define(SEND, '$gen_send').
-define(DEFAULT_TIMEOUT, 5000).
-define(IS_HIBERNATE(H), H =:= true orelse H =:= hibernate).

-type init_response() :: #ok{} | #stop{}.
-type proc_response() :: #ok{} | #reply{} | #stop{}.

-callback init(term()) -> init_response().
-callback proc(term(), {pid(), reference()}, term()) -> proc_response().
-callback terminate(term(), term()) -> any().

start(Module, Args) ->
    Parent = self(),
    spawn(fun() -> init(Parent, Module, Args) end).

start_link(Module, Args) ->
    Parent = self(),
    spawn_link(fun() -> init(Parent, Module, Args) end).

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

init(Parent, Module, Args) ->
    put('$ancestors', [Parent]),
    put('$initial_call', {Module, init, 1}),
    case Module:init(Args) of
        #ok{state = State, timeout = Timeout, hibernate = Hibernate} ->
            loop(Module, State, Timeout, Hibernate);
        #stop{reason = Reason, state = State} ->
            terminate(Module, State, Reason)
    end.

loop(Module, State, Timeout, Hibernate) when ?IS_HIBERNATE(Hibernate) ->
    erlang:hibernate(?MODULE, loop, [Module, State, Timeout]);
loop(Module, State, Timeout, _) ->
    loop(Module, State, Timeout).

%% @private
loop(Module, State, Timeout) ->
    receive
        {?SEND, From, Request} ->
            proc(Module, State, From, Request);
        {?STOP, Reason} ->
            terminate(Module, State, Reason);
        Info ->
            proc(Module, State, undefined, Info)
    after Timeout ->
        self() ! timeout
    end.

proc(Module, State, From, Request) ->
    case Module:proc(Request, From, State) of
        #ok{state = NewState, timeout = T, hibernate = H} ->
            loop(Module, NewState, T, H);
        #reply{reply = Reply, state = NewState, timeout = T, hibernate = H} ->
            reply(From, Reply),
            loop(Module, NewState, T, H);
        #stop{reason = Reason, reply = ?NOREPLY, state = NewState} ->
            terminate(Module, NewState, Reason);
        #stop{reason = Reason, reply = Reply, state = NewState} ->
            reply(From, Reply),
            terminate(Module, NewState, Reason)
    end.

terminate(Module, State, Reason) ->
    Module:terminate(State, Reason).
