%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_sup).
-behaviour(supervisor).

-include("redets_kv.hrl").

%% API
-export([start_link/0, new_store/1, rm_store/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Create a new store from proplist store config `StoreConfig'.
%% The public API for this functionality is {@link redets_kv:new_store/1}.
new_store(StoreConfig) ->
    NewStore = redets_kv_config:list_to_store(StoreConfig),
    Spec = store_sup_spec(NewStore),
    supervisor:start_child(?MODULE, Spec).

%% @doc Shutdown the named store.
rm_store(Name) ->
    SupName = store_sup_name(Name),
    case supervisor:terminate_child(?MODULE, SupName) of
        {error, not_found} ->
            ok;
        ok ->
            supervisor:delete_child(?MODULE, SupName);
        Error ->
            Error
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    %% a list of store configs
    Config = case application:get_env(redets_kv, stores) of
        {ok, C} ->
            C;
        undefined ->
            []
    end,
    Stores = [ redets_kv_config:list_to_store(L) || L <- Config ],
    StoreSupSpecs = [ store_sup_spec(Store) || Store <- Stores ],
    {ok, {{one_for_one, 5, 60}, [call_fsm_sup_spec() | StoreSupSpecs]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

call_fsm_sup_spec() ->
    {redets_kv_call_fsm_sup,
        {redets_kv_call_fsm_sup, start_link, []},
        transient, 5000, supervisor, [redets_kv_call_fsm_sup]}.

store_sup_spec(Store=#store{name=Name}) ->
    SupName = store_sup_name(Name),
    {SupName,
        {redets_kv_store_sup, start_link, [Store]},
        transient, 5000, supervisor, [redets_kv_store_sup]}.

store_sup_name(Name) ->
    list_to_atom("redets_kv_" ++ atom_to_list(Name) ++ "_store_sup").
