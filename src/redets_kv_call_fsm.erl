%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   26 Jun 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_call_fsm).
-behaviour(gen_fsm).

-include("redets_kv.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([execute/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Call=#call{}) ->
    gen_fsm:start_link(?MODULE, Call, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init(Call=#call{}) ->
    {ok, execute, Call, 0}.

handle_event(_Event, StateName, Call) ->
    {next_state, StateName, Call, 0}.

handle_sync_event(_Event, _From, StateName, Call) ->
    {reply, ok, StateName, Call, 0}.

handle_info(_Info, StateName, Call) ->
    {next_state, StateName, Call, 0}.

terminate(Reason, _StateName, Call) ->
    _ = reply(Call, {error, Reason}),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

execute(timeout, Call) ->
    Result = backend_call(Call),
    Call2 = reply(Call, Result),
    {stop, normal, Call2}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
backend_call(#call{store=#store{backend=Backend, state=State}, op=Op, params=Params}) ->
    backend_call(Backend, Op, Params, State).

%% @private
backend_call(Backend, Op, [], State) ->
    Backend:Op(State);
backend_call(Backend, Op, [A], State) ->
    Backend:Op(A, State);
backend_call(Backend, Op, [A, B], State) ->
    Backend:Op(A, B, State);
backend_call(Backend, Op, [A, B, C], State) ->
    Backend:Op(A, B, C, State);
backend_call(Backend, Op, [A, B, C, D], State) ->
    Backend:Op(A, B, C, D, State).

%% @private
reply(Call=#call{replied=false, from=undefined}, _Reply) ->
    Call#call{replied=true};
reply(Call=#call{replied=false, from=From, store=#store{name=Name}}, Reply) ->
    ok = gen_server:cast(Name, {reply, Reply, From}),
    Call#call{replied=true};
reply(Call, _Reply) ->
    Call.
