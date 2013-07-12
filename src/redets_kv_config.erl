%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   12 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_config).

-include("redets_kv.hrl").

%% API
-export([list_to_store/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec list_to_store([{atom(), term()}]) -> #store{}.
list_to_store(S) ->
    #store{
        name    = req(name, S),
        backend = req(backend, S),
        options = req(options, S)
    }.

%% Return `Value' for `Key' in proplist `P' or crashes with an
%% informative message if no value is found.
req(Key, P) ->
    case lists:keyfind(Key, 1, P) of
        false ->
            error({missing_required_config, Key, P});
        {Key, Value} ->
            Value
    end.
