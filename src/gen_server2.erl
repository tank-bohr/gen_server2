-module(gen_server2).
-include("gen_server2.hrl").

-export([start/2]).
-export([loop/2]).

-define(IS_HIBERNATE(H), H =:= true orelse H =:= hibernate).

-type state()   :: ?NOSTATE | tuple().
-type return()  :: #ok{} | #stop{}.
-type request() :: tuple().

-export_type([
    state/0,
    return/0,
    request/0
]).

-callback proc(Request, State) -> return() when
    Request :: request(),
    State   :: state().

-record(state, {
    module           :: module(),
    state = ?NOSTATE :: state()
}).

start(Module, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    SpawnOpts = proplists:get_value(spawn_opt, Options, []),
    spawn_opt(fun() -> loop(#state{module = Module}, Timeout) end, SpawnOpts).

loop(State, Timeout, Hibernate) when ?IS_HIBERNATE(Hibernate) ->
    erlang:hibernate(?MODULE, ?FUNCTION_NAME, [State, Timeout]);
loop(State, Timeout, _) ->
    loop(State, Timeout).

%% @private
loop(State, Timeout) ->
    receive
        Request ->
            proc(Request, State)
    after Timeout ->
        self() ! timeout
    end.

proc(Request, #state{module = Module, state = StateIn} = State) ->
    case Module:proc(Request, StateIn) of
        #ok{state = StateOut, timeout = T, hibernate = H} ->
            loop(State#state{state = StateOut}, T, H);
        #stop{state = StateOut, reason = Reason} ->
            terminate(Reason, StateOut)
    end.

terminate(_Reason, _State) ->
    ok.
