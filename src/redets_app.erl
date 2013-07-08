%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_Type, _Args) ->
    redets_sup:start_link().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
