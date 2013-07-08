%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   2 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_storage).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    ref           = undefined :: undefined | any(),
    backend       = undefined :: undefined | module(),
    backend_state = undefined :: undefined | any()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Ref, Backend, BackendState) ->
    gen_server:start_link(?MODULE, [Ref, Backend, BackendState], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Ref, Backend, BackendState]) ->
    ok = redets_server:set_storage(Ref, self()),
    State = #state{ref=Ref, backend=Backend, backend_state=BackendState},
    {ok, State}.

handle_call({execute, Method, Args}, From, State) ->
    handle_execute(Method, Args, From, State);
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({execute, Method, Args}, State) ->
    handle_execute(Method, Args, undefined, State);
handle_cast({reply, From, {ok, BackendState}}, State) ->
    gen_server:reply(From, ok),
    {noreply, State#state{backend_state=BackendState}};
handle_cast({reply, From, {ok, Reply, BackendState}}, State) ->
    gen_server:reply(From, {ok, Reply}),
    {noreply, State#state{backend_state=BackendState}};
handle_cast({reply, From, {error, Reason, BackendState}}, State) ->
    gen_server:reply(From, {error, Reason}),
    {noreply, State#state{backend_state=BackendState}};
handle_cast({reply, From, Reply}, State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{backend=Backend, backend_state=BackendState}) ->
    catch Backend:stop(BackendState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_execute(Method, Args, From, State=#state{ref=Ref, backend=Backend,
        backend_state=BackendState}) ->
    supervisor:start_child(redets_fsm_sup, [From, Ref, Backend, Method, Args, BackendState]),
    {noreply, State}.
