%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   6 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_storage_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Ref, Backend, BackendOpts) ->
    supervisor:start_link(?MODULE, {Ref, Backend, BackendOpts}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init({Ref, Backend, BackendOpts}) ->
    {ok, BackendState} = Backend:start(Ref, BackendOpts),
    {ok, {{one_for_one, 5, 10}, [
        {redets_storage,
            {redets_storage, start_link, [Ref, Backend, BackendState]},
            permanent, infinity, worker, [redets_storage]}
    ]}}.
