%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   12 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_store_sup).
-behaviour(supervisor).

-include("redets_kv.hrl").

%% API
-export([start_link/1, store_sup_name/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Store=#store{}) ->
    SupName = store_sup_name(Store),
    supervisor:start_link({local, SupName}, ?MODULE, Store).

store_sup_name(#store{name=StoreName}) ->
    list_to_atom("redets_kv_" ++ atom_to_list(StoreName) ++ "_store_sup").

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Store=#store{}) ->
    RedetsKVSpec = {redets_kv,
        {redets_kv, start_link, [Store]},
        transient, 5000, worker, [redets_kv]},
    %% five restarts in 60 seconds, then shutdown
    Restart = {one_for_all, 5, 60},
    {ok, {Restart, [RedetsKVSpec]}}.
