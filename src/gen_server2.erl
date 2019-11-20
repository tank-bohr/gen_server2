-module(gen_server2).

-include("gen_server2.hrl").

-export([
    start/1,
    start/2,
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

-record(srv, {
    from    :: {pid(), reference()},
    request :: term()
}).

-callback proc(Request, State) -> #ok{} | #reply{} | #stop{} when
    Request :: tuple(),
    State   :: tuple().

start(Module) ->
    ?FUNCTION_NAME(Module, []).

start(Module, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    SpawnOpts = proplists:get_value(spawn_opt, Options, []),
    spawn_opt(fun() -> loop(Module, ?NOSTATE, Timeout) end, SpawnOpts).

call(Server, Request) ->
    ?FUNCTION_NAME(Server, Request, ?DEFAULT_TIMEOUT).

call(Server, Request, Timeout) ->
    wait(send(Server, Request), Timeout).

send(Server, Request) ->
    Tag = make_ref(),
    Server ! #srv{from = {self(), Tag}, request = Request},
    Tag.

wait(Tag) ->
    ?FUNCTION_NAME(Tag, ?DEFAULT_TIMEOUT).

wait(Tag, Timeout) ->
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
        timeout
    end.

reply({Pid, Tag}, Reply) ->
    Pid ! {Tag, Reply}.

loop(Module, State, Timeout, Hibernate) when ?IS_HIBERNATE(Hibernate) ->
    erlang:hibernate(?MODULE, ?FUNCTION_NAME, [Module, State, Timeout]);
loop(Module, State, Timeout, _) ->
    loop(Module, State, Timeout).

%% @private
loop(Module, State, Timeout) ->
    receive
        #srv{from = From, request = Request} ->
            proc(Module, State, From, Request);
        Info ->
            proc(Module, State, undefined, Info)
    after Timeout ->
        self() ! timeout
    end.

proc(Module, State, From, Request) ->
    case Module:proc(Request, State) of
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
