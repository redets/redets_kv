%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   1 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets).

%% API
-export([start/0, start_storage/3, stop_storage/1, child_spec/3]).
-export([call/3, call/4, cast/3]). %% Storage

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
    application:start(sasl),
    application:start(redets).

start_storage(Ref, Backend, BackendOpts) ->
    supervisor:start_child(redets_sup, child_spec(Ref, Backend, BackendOpts)).

stop_storage(Ref) ->
    case supervisor:terminate_child(redets_sup, {redets_storage_sup, Ref}) of
        ok ->
            _ = supervisor:delete_child(redets_sup, {redets_storage_sup, Ref}),
            redets_server:cleanup_storage(Ref);
        {error, Reason} ->
            {error, Reason}
    end.

child_spec(Ref, Backend, BackendOpts) ->
    {{redets_storage_sup, Ref},
        {redets_storage_sup, start_link, [Ref, Backend, BackendOpts]},
        permanent, 5000, supervisor, [redets_storage_sup]}.

%% Storage

call(Ref, Method, Args) ->
    Pid = redets_server:get_storage(Ref),
    gen_server:call(Pid, {execute, Method, Args}).

call(Ref, Method, Args, Timeout) ->
    Pid = redets_server:get_storage(Ref),
    gen_server:call(Pid, {execute, Method, Args}, Timeout).

cast(Ref, Method, Args) ->
    Pid = redets_server:get_storage(Ref),
    gen_server:cast(Pid, {execute, Method, Args}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
