%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   12 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------

-record(store, {
    name    = undefined :: undefined | atom(),
    backend = undefined :: undefined | module(),
    options = undefined :: undefined | any(),
    state   = undefined :: undefined | any()
}).

-record(call, {
    store    = undefined :: undefined | #store{},
    op       = undefined :: undefined | atom(),
    params   = []        :: [any()],
    from     = undefined :: undefined | {pid(), any()},
    replied  = false     :: boolean()
}).
