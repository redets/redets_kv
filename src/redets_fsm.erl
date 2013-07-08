%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/6]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([execute/2]).

-record(state, {
    from    = undefined :: undefined | pid() | {pid(), any()},
    ref     = undefined :: undefined | any(),
    backend = undefined :: undefined | module(),
    command = undefined :: undefined | atom(),
    args    = undefined :: undefined | [any()],
    bstate  = undefined :: undefined | any()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(From, Ref, Backend, Command, Args, BackendState) ->
    gen_fsm:start_link(?MODULE, [From, Ref, Backend, Command, Args, BackendState], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([From, Ref, Backend, Command, Args, BackendState]) ->
    % erlang:process_flag(trap_exit, true),
    State = #state{from=From, ref=Ref, backend=Backend, command=Command,
        args=Args, bstate=BackendState},
    {ok, execute, State, 0}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, 0}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData, 0}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData, 0}.

terminate(Reason, _StateName, State) ->
    {ok, _} = do_reply(State, {error, Reason}),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

execute(timeout, State=#state{backend=Backend, command=Command,
        args=Args, bstate=BackendState}) ->
    Reply = do_apply(Backend, Command, Args, BackendState),
    {ok, State2} = do_reply(State, Reply),
    {stop, normal, State2}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

do_apply(Backend, Method, [], BackendState) ->
    Backend:Method(BackendState);
do_apply(Backend, Method, [A], BackendState) ->
    Backend:Method(A, BackendState);
do_apply(Backend, Method, [A, B], BackendState) ->
    Backend:Method(A, B, BackendState);
do_apply(Backend, Method, [A, B, C], BackendState) ->
    Backend:Method(A, B, C, BackendState);
do_apply(Backend, Method, [A, B, C, D], BackendState) ->
    Backend:Method(A, B, C, D, BackendState).

do_reply(State=#state{from=undefined}, _Reply) ->
    {ok, State};
do_reply(State=#state{from=From, ref=Ref}, Reply) ->
    Pid = redets_server:get_storage(Ref),
    gen_server:cast(Pid, {reply, From, Reply}),
    {ok, State#state{from=undefined}}.
