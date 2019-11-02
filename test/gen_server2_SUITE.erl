-module (gen_server2_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
]).

-export([
    fizz_buzz_test/1
]).

all() ->
    [fizz_buzz_test].

fizz_buzz_test(_Config) ->
    {ok, Server} = fizz_buzz_server:start_link(),
    ?assertEqual(1, fizz_buzz_server:next(Server)),
    ?assertEqual(2, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Fizz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(4, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Buzz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Fizz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(7, fizz_buzz_server:next(Server)),
    ?assertEqual(8, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Fizz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Buzz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(11, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"Fizz">>, fizz_buzz_server:next(Server)),
    ?assertEqual(13, fizz_buzz_server:next(Server)),
    ?assertEqual(14, fizz_buzz_server:next(Server)),
    ?assertEqual(<<"FizzBuzz">>, fizz_buzz_server:next(Server)),
    ct:capture_start(),
    fizz_buzz_server:print(Server),
    ?assertEqual(["16\n"], ct:capture_get()),
    Server ! pants,
    ct:sleep(500),
    ?assertEqual(["Unexpected message: pants\n"], ct:capture_get()),
    ct:capture_stop(),
    ?assert(is_process_alive(Server)),
    gen_server2:stop(Server, kill, 500),
    ?assert(not is_process_alive(Server)).
