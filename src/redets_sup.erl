%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    redets_server = ets:new(redets_server, [ordered_set, public, named_table]),
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(redets_server, worker),
        {redets_fsm_sup,
            {redets_fsm_sup, start_link, []},
            permanent, infinity, supervisor, [redets_fsm_sup]}
    ]}}.
