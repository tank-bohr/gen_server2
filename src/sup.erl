-module(sup).
-include_lib("gen_server2/include/gen_server2.hrl").

-export([
    start_link/2
]).

-behaviour(gen_server2).
-export([proc/3]).

-type sup_flags() :: #{
    strategy  => strategy(),
    intensity => non_neg_integer(),
    period    => pos_integer()
}.

-type child_spec() :: #{
    id       := child_id(),
    start    := mfargs(),
    restart  => restart(),
    shutdown => shutdown(),
    type     => worker(),
    modules  => modules()
}.

-type child_id() :: term().
-type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type restart()  :: permanent | transient | temporary.
-type shutdown() :: brutal_kill | timeout().
-type strategy() :: one_for_all |
                    one_for_one |
                    rest_for_one |
                    simple_one_for_one.
-type worker()   :: worker | supervisor.
-type modules()  :: [module()] | dynamic.

-callback init(term()) -> {ok, {sup_flags(), [child_spec()]}} | ignore.

-record(state, {
    module        :: module(),
    children = [] :: list(),
    flags         :: sup_flags()
}).

start_link(Module, Args) ->
    gen_server2:start_link(?MODULE, {Module, Args}).

proc(init, _, {Module, Args}) ->
    process_flag(trap_exit, true),
    case Module:init(Args) of
        {ok, {Flags, Spec}} ->
            Children = start_children(Spec),
            State = #state{module = Module, children = Children, flags = Flags},
            #ok{state = State};
        ignore ->
            ignore
    end;
proc({'EXIT', Pid, _Reason}, _, #state{children = Children, flags = Flags} = State) ->
    {value, {Pid, ChildSpec}, NChildren} = lists:keytake(Pid, 1, Children),
    _Restart = maps:get_value(restart, ChildSpec, permanent),
    _Strategy = maps:get_value(strategy, Flags, one_for_one),
    {ok, NPid} = start_child(ChildSpec),
    #ok{state = State#state{children = [{NPid, ChildSpec} | NChildren]}}.

start_children(Spec) ->
    start_children(Spec, []).

start_children([], Acc) ->
    Acc;
start_children([ChildSpec | Rest], Acc) ->
    case start_child(ChildSpec) of
        {ok, Pid} ->
            start_children(Rest, [{Pid, ChildSpec} | Acc]);
        _ ->
            start_children(Rest, Acc)
    end.

start_child(#{id := _ChildId, start := [M, F, A]}) ->
    case apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        ignore ->
            {ok, undefined};
        {error, Error} ->
            {error, Error};
        Unexpected ->
            {error, Unexpected}
    end.
