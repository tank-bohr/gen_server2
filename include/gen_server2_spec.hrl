-ifndef(GEN_SERVER2_SPEC_HRL).
-define(GEN_SERVER2_SPEC_HRL, true).

-spec proc(Request, FromOrReason, State) -> #ok{} | #reply{} | #stop{} when
    Request      :: init | terminate | term(),
    FromOrReason :: pid() | {pid(), reference()} | term(),
    State        :: term().

-endif.
