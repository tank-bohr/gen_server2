-module(gen_server_compat).

-export([
    start_link/2,
    start_link/3,
    stop/1,
    stop/3
]).

-behaviour(gen_server2).
-include("gen_server2.hrl").
-export([proc/3]).

-record(foo, {
    mod  :: atom(),
    func :: atom(),
    args :: term()
}).

-record(state, {
    mod   :: atom(),
    state :: any()
}).

-callback init(term()) -> {ok, term()} | {stop, term()}.
-callback terminate(term(), term()) -> any().

start_link(Module, Args) ->
    ?FUNCTION_NAME(Module, Args, []).

start_link(Module, Args, Options) ->
    Server = gen_server2:start(?MODULE, [{spawn_opt, [link]} | Options]),
    gen_server2:call(Server, #foo{mod = Module, func = init, args = Args}).

stop(Server) ->
    ?FUNCTION_NAME(Server, normal, infinity).

stop(Server, Reason, Timeout) ->
    gen_server2:call(Server, #foo{func = terminate, args = Reason}, Timeout).

proc(#foo{func = init, mod = Module, args = Args}, _From, _) ->
    case Module:init(Args) of
        {ok, State} ->
            #reply{reply = {ok, self()},
                state = #state{state = State, mod = Module}};
        {ok, State, hibernate} ->
            #reply{reply = {ok, self()},
                state = #state{state = State, mod = Module}, hibernate = true};
        {ok, State, Timeout} ->
            #reply{reply = {ok, self()},
                state = #state{state = State, mod = Module}, timeout = Timeout};
        {stop, Reason} ->
            Module:terminate(Reason, undefined),
            #stop{reply = {error, reason}, reason = Reason}
    end;
proc(#foo{func = terminate, args = Reason}, _, #state{state = State, mod = Module}) ->
    Module:terminate(Reason, State),
    #stop{reply = ok, reason = Reason,
        state = #state{state = State, mod = Module}}.
