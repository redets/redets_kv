%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   1 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv).
-behaviour(gen_server).

-include("redets_kv.hrl").

%% API
-export([manual_start/0, new_store/1, rm_store/1, start_link/1]).

%% Storage API
-export([call/3, call/4, cast/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

manual_start() ->
    application:start(sasl),
    application:start(redets_kv).

new_store(StoreConfig) ->
    redets_kv_sup:new_store(StoreConfig).

rm_store(StoreName) ->
    redets_kv_sup:rm_store(StoreName).

start_link(Store=#store{name=Name}) ->
    gen_server:start_link({local, Name}, ?MODULE, Store, []).

%%%===================================================================
%%% Storage API functions
%%%===================================================================

call(StoreName, Op, Params) ->
    gen_server:call(StoreName, {call, Op, Params}).

call(StoreName, Op, Params, Timeout) ->
    gen_server:call(StoreName, {call, Op, Params}, Timeout).

cast(StoreName, Op, Params) ->
    gen_server:cast(StoreName, {call, Op, Params}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Store=#store{name=Name, backend=Backend, options=Options}) ->
    case Backend:start(Name, Options) of
        {ok, State} ->
            {ok, Store#store{state=State}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({call, Op, Params}, From, Store) ->
    {ok, _} = redets_kv_call_fsm_sup:call(Store, Op, Params, From),
    {noreply, Store};
handle_call(_Request, _From, Store) ->
    {reply, {error, undef}, Store}.

handle_cast({call, Op, Params}, Store) ->
    {ok, _} = redets_kv_call_fsm_sup:call(Store, Op, Params, undefined),
    {noreply, Store};
handle_cast({reply, Reply, From}, Store) ->
    handle_reply(Reply, From, Store);
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _Store=#store{backend=Backend, state=State}) ->
    catch Backend:stop(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_reply({ok, State}, From, Store) ->
    gen_server:reply(From, ok),
    {noreply, Store#store{state=State}};
handle_reply({ok, Reply, State}, From, Store) ->
    gen_server:reply(From, {ok, Reply}),
    {noreply, Store#store{state=State}};
handle_reply({error, Reason, State}, From, Store) ->
    gen_server:reply(From, {error, Reason}),
    {noreply, Store#store{state=State}};
handle_reply(Reply, From, Store) ->
    gen_server:reply(From, Reply),
    {noreply, Store}.
