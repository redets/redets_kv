%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_util).

-include("redets_kv.hrl").

-define(EPOCH, 62167219200). %% calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})

%% API
-export([now/0, timestamp/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc UTC in *NIX seconds
-spec now() -> pos_integer().
now() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?EPOCH.

%% @doc Current timestamp
-spec timestamp() -> float().
timestamp() ->
    ?MODULE:now() + element(3, erlang:now()) / 1000000.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
