-ifndef(GEN_SERVER2_HRL).
-define(GEN_SERVER2_HRL, true).

-define(NOREPLY, '$noreply').
-define(NOSTATE, '$nostate').

-record(ok, {
    state               :: term(),
    timeout =  infinity :: non_neg_integer() | infinity,
    hibernate = false   :: boolean()
}).

-record(reply, {
    reply              :: term(),
    state              :: term(),
    timeout = infinity :: non_neg_integer() | infinity,
    hibernate = false  :: boolean()
}).

-record(stop, {
    reason           :: term(),
    reply = ?NOREPLY :: term(),
    state = ?NOSTATE :: term()
}).

-endif.
