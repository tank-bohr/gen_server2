-ifndef(GEN_SERVER2_HRL).
-define(GEN_SERVER2_HRL, true).

-define(NOREPLY, '$noreply').
-define(NOSTATE, '$nostate').

-record(ok, {
    reply     = ?NOREPLY :: term(),
    state     = ?NOSTATE :: ?NOSTATE | tuple(),
    timeout   = infinity :: non_neg_integer() | infinity,
    hibernate = false    :: boolean()
}).

-record(stop, {
    reply = ?NOREPLY :: term(),
    state = ?NOSTATE :: ?NOSTATE | tuple(),
    reason           :: term()
}).

-endif.
