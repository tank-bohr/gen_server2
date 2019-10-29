-module(fizz_buzz_server).
-include_lib("gen_server2/include/gen_server2.hrl").

-export([
    start_link/0,
    next/1,
    print/1
]).

-behaviour(gen_server2).
-export([
    init/1,
    proc/3,
    terminate/2
]).

-record(state, {
    current = 0 :: non_neg_integer()
}).

start_link() ->
    gen_server2:start_link(?MODULE, []).

init([]) ->
    #ok{state = #state{}}.

next(Server) ->
    gen_server2:call(Server, next).

print(Server) ->
    gen_server2:call(Server, print).

proc(next, _From, #state{current = Cur} = State) ->
    Next = Cur + 1,
    #reply{reply = fizz_buzz(Next), state = State#state{current = Next}};
proc(print, _From, #state{current = Cur} = State) ->
    Next = Cur + 1,
    io:format("~p~n", [fizz_buzz(Next)]),
    #ok{state = State#state{current = Next}};
proc(Unexpected, _From, State) ->
    io:format("Unexpected message: ~p~n", [Unexpected]),
    #ok{state = State}.

terminate(_Reason, _State) ->
    ok.

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
