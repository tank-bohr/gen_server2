-module(fizz_buzz_server).
-include_lib("gen_server2/include/gen_server2.hrl").

-export([
    start_link/0,
    stop/3,
    next/1,
    print/1
]).

-behaviour(gen_server2).
-export([
    proc/2
]).

-record(state, {
    current = 0 :: non_neg_integer()
}).

-record(fb, { %% fizz_buzz
    msg = init :: init | next | print | stop
}).

start_link() ->
    Pid = gen_server2:start(?MODULE, [{spawn_opt, [link]}]),
    gen_server2:call(Pid, #fb{}).

next(Server) ->
    gen_server2:call(Server, #fb{msg = next}).

print(Server) ->
    gen_server2:call(Server, #fb{msg = print}).

stop(Server, _Reason, Timeout) ->
    gen_server2:call(Server, #fb{msg = stop}, Timeout).

proc(#fb{msg = init}, ?NOSTATE) ->
    #reply{reply = {ok, self()}, state = #state{}};
proc(#fb{msg = next}, #state{current = Cur} = State) ->
    Next = Cur + 1,
    #reply{reply = fizz_buzz(Next), state = State#state{current = Next}};
proc(#fb{msg = print}, #state{current = Cur} = State) ->
    Next = Cur + 1,
    io:format("~p~n", [fizz_buzz(Next)]),
    #ok{state = State#state{current = Next}};
proc(#fb{msg = stop}, State) ->
    #stop{state = State}.

fizz_buzz(Num) ->
    case {Num rem 3, Num rem 5} of
        {0, 0} ->
            <<"FizzBuzz">>;
        {0, _} ->
            <<"Fizz">>;
        {_, 0} ->
            <<"Buzz">>;
        _ ->
            Num
    end.
