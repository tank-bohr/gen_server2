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

-define(DEFAULT_TIMEOUT, 5000).
-define(IS_HIBERNATE(H), H =:= true orelse H =:= hibernate).
-define(CORRECT_TYPE(T), T =:= init orelse T =:= send orelse T =:= stop).

-record(srv, {
    type    :: init | send | stop,
    from    :: {pid(), reference()},
    request :: term()
}).

-callback proc(Request, From, State) -> #ok{} | #reply{} | #stop{} when
    Request :: tuple(),
    From    :: {pid(), reference()},
    State   :: tuple().

start(Module, Initial) ->
    init(spawn(fun() -> loop(Module, ?NOSTATE, infinity) end), Initial).

start_link(Module, Initial) ->
    init(spawn_link(fun() -> loop(Module, ?NOSTATE, infinity) end), Initial).

stop(Server) ->
    stop(Server, normal, infinity).

stop(Server, Reason, Timeout) ->
    wait(send(Server, Reason, stop), Timeout).

call(Server, Request) ->
    call(Server, Request, ?DEFAULT_TIMEOUT).

call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

send(Server, Request) ->
    send(Server, Request, send).

send(Server, Request, Type) when ?CORRECT_TYPE(Type) ->
    Tag = make_ref(),
    Server ! #srv{type = Type, from = {self(), Tag}, request = Request},
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

init(Server, Initial) ->
    wait(send(Server, Initial, init)).

loop(Module, State, Timeout, Hibernate) when ?IS_HIBERNATE(Hibernate) ->
    erlang:hibernate(?MODULE, ?FUNCTION_NAME, [Module, State, Timeout]);
loop(Module, State, Timeout, _) ->
    loop(Module, State, Timeout).

%% @private
loop(Module, State, Timeout) ->
    receive
        #srv{type = init, from = From, request = Request} ->
            proc_init(Module, State, From, Request);
        #srv{type = send, from = From, request = Request} ->
            proc(Module, State, From, Request);
        #srv{type = stop, from = From, request = Reason} ->
            terminate(Reason, State),
            reply(From, ok);
        Info ->
            proc(Module, State, undefined, Info)
    after Timeout ->
        self() ! timeout
    end.

proc_init(Module, State, {Parent, _} = From, Request) ->
    case Module:proc(Request, Parent, State) of
        #ok{state = NewState, timeout = T, hibernate = H} ->
            reply(From, {ok, self()}),
            loop(Module, NewState, T, H);
        #stop{reason = Reason, state = NewState} ->
            terminate(Reason, NewState),
            reply(From, {error, Reason})
    end.

proc(Module, State, From, Request) ->
    case Module:proc(Request, From, State) of
        #ok{state = NewState, timeout = T, hibernate = H} ->
            loop(Module, NewState, T, H);
        #reply{reply = Reply, state = NewState, timeout = T, hibernate = H} ->
            reply(From, Reply),
            loop(Module, NewState, T, H);
        #stop{reason = Reason, reply = ?NOREPLY, state = NewState} ->
            terminate(Reason, NewState);
        #stop{reason = Reason, reply = Reply, state = NewState} ->
            terminate(Reason, NewState),
            reply(From, Reply)
    end.

terminate(_Reason, _State) ->
    ok.
