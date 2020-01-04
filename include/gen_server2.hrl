-ifndef(GEN_SERVER2_HRL).
-define(GEN_SERVER2_HRL, true).

-define(NOREPLY, '$noreply').

-record(ok, {
    reply     = ?NOREPLY :: term(),
    state                :: tuple(),
    timeout   = infinity :: non_neg_integer() | infinity,
    hibernate = false    :: boolean()
}).

-record(stop, {
    reply = ?NOREPLY :: term(),
    state            :: tuple(),
    reason           :: term()
}).

-endif.
