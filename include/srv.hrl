-ifndef(SRV_HRL).
-define(SRV_HRL, true).

-define(NOREPLY, '$noreply').

-record(ok, {
    reply     = ?NOREPLY :: term(),
    state                :: tuple(),
    timeout   = infinity :: non_neg_integer() | infinity,
    hibernate = false    :: boolean()
}).

-record(stop, {
    reply  = ?NOREPLY :: term(),
    reason = normal   :: term(),
    state  = {}       :: tuple()
}).

-endif.
