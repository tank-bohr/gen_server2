-module(gen_server2).
-include("gen_server2.hrl").

-type state()   :: map().
-type request() :: tuple().

-export_type([
    state/0,
    request/0
]).

-callback proc(Request, State) -> state() when
    Request :: request(),
    State   :: state().

-export([
    proc_for/3
]).
