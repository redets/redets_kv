%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_call_fsm_sup).
-behaviour(supervisor).

-include("redets_kv.hrl").

%% API
-export([start_link/0, call/4]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

call(Store, Op, Params, From) ->
    supervisor:start_child(?MODULE, [#call{
        store=Store,
        op=Op,
        params=Params,
        from=From
    }]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    FSM = {undefined,
        {redets_kv_call_fsm, start_link, []},
        temporary, 5000, worker, [redets_kv_call_fsm]},
    Specs = [FSM],
    Restart = {simple_one_for_one, 1, 1},
    {ok, {Restart, Specs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
