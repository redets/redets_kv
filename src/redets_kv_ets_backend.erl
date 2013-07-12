%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   2 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_ets_backend).
-behaviour(redets_kv_backend).

%% redets_kv_backend callbacks
-export([start/2, stop/1]).
-export([nodes/1, status/1]). %% Status
-export([search/2]). %% Search
-export([del/3, get/3, mget/3, mset/3, set/4]). %% Strings
-export([hdel/4, hget/4, hgetall/3, hmget/4, hmset/4, hset/5]). %% Hashes
-export([sadd/4, sismember/4, smembers/3, srem/4]). %% Sets

-define(SNAME(R), list_to_atom("redets_kv_set_"++atom_to_list(R))).

-record(state, {
    set = undefined :: undefined | ets:tid()
}).

%%%===================================================================
%%% redets_kv_backend callbacks
%%%===================================================================

start(StoreName, _Config) ->
    Set = ets:new(?SNAME(StoreName), [ordered_set, public, named_table]),
    {ok, #state{set=Set}}.

stop(#state{set=Set}) ->
    catch ets:delete(Set),
    ok.

%%%===================================================================
%%% Status
%%%===================================================================

nodes(#state{}) ->
    erlang:nodes().

status(#state{set=Set}) ->
    [{set, ets:info(Set)}].

%%%===================================================================
%%% Search
%%%===================================================================

search(MatchSpec, State=#state{set=Set}) ->
    {ok, ets:select(Set, MatchSpec), State}.

%%%===================================================================
%%% Strings
%%%===================================================================

del(_Bucket, [], State) ->
    {ok, State};
del(Bucket, [Key | Keys], State=#state{set=Set}) ->
    true = ets:delete(Set, {Bucket, Key}), %% Strings
    true = ets:match_delete(Set, {{Bucket, {Key, '_'}}, '_'}), %% Hashes
    true = ets:match_delete(Set, {{Bucket, {Key, '_'}}}), %% Sets
    del(Bucket, Keys, State).

get(Bucket, Key, State=#state{set=Set}) ->
    case ets:lookup(Set, {Bucket, Key}) of
        [] ->
            {ok, undefined, State};
        [{{Bucket, Key}, Val}] ->
            {ok, Val, State};
        Error ->
            {error, Error, State}
    end.

mget(Bucket, Keys, State) ->
    mget(Bucket, Keys, State, []).

mset(Bucket, KeyVals, State) ->
    mset(Bucket, KeyVals, State, []).

set(Bucket, Key, Val, State=#state{set=Set}) ->
    true = ets:insert(Set, {{Bucket, Key}, Val}),
    {ok, State}.

%%%===================================================================
%%% Hashes
%%%===================================================================

hdel(_Bucket, _Key, [], State) ->
    {ok, State};
hdel(Bucket, Key, [Field | Fields], State=#state{set=Set}) ->
    true = ets:delete(Set, {Bucket, {Key, Field}}),
    hdel(Bucket, Key, Fields, State).

hget(Bucket, Key, Field, State=#state{set=Set}) ->
    case ets:lookup(Set, {Bucket, {Key, Field}}) of
        [] ->
            {ok, undefined, State};
        [{{Bucket, {Key, Field}}, Val}] ->
            {ok, Val, State};
        Error ->
            {error, Error, State}
    end.

hgetall(Bucket, Key, State=#state{set=Set}) ->
    case ets:match_object(Set, {{Bucket, {Key, '_'}}, '_'}) of
        Objects when is_list(Objects) ->
            hgetall(Objects, Bucket, Key, State, []);
        Error ->
            {error, Error, State}
    end.

hmget(Bucket, Key, Fields, State) ->
    hmget(Bucket, Key, Fields, State, []).

hmset(Bucket, Key, FieldVals, State) ->
    hmset(Bucket, Key, FieldVals, State, []).

hset(Bucket, Key, Field, Val, State=#state{set=Set}) ->
    true = ets:insert(Set, {{Bucket, {Key, Field}}, Val}),
    {ok, State}.

%%%===================================================================
%%% Sets
%%%===================================================================

sadd(Bucket, Key, Vals, State) ->
    sadd(Bucket, Key, Vals, State, []).

sismember(Bucket, Key, Val, State=#state{set=Set}) ->
    case ets:match_object(Set, {{Bucket, {Key, Val}}}) of
        [] ->
            {ok, false, State};
        [{{Bucket, {Key, Val}}}] ->
            {ok, true, State};
        Error ->
            {error, Error, State}
    end.

smembers(Bucket, Key, State=#state{set=Set}) ->
    case ets:match(Set, {{Bucket, {Key, '$1'}}}) of
        Vals when is_list(Vals) ->
            smembers(Vals, Bucket, Key, State, []);
        Error ->
            {error, Error, State}
    end.

srem(_Bucket, _Key, [], State) ->
    {ok, State};
srem(Bucket, Key, [Val | Vals], State=#state{set=Set}) ->
    true = ets:delete_object(Set, {{Bucket, {Key, Val}}}),
    srem(Bucket, Key, Vals, State).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Strings
mget(_Bucket, [], State, Acc) ->
    {ok, lists:reverse(Acc), State};
mget(Bucket, [Key | Keys], State=#state{set=Set}, Acc) ->
    case ets:lookup(Set, {Bucket, Key}) of
        [] ->
            mget(Bucket, Keys, State, [undefined | Acc]);
        [{{Bucket, Key}, Val}] ->
            mget(Bucket, Keys, State, [Val | Acc]);
        Error ->
            {error, Error, State}
    end.

mset(_Bucket, [], State=#state{set=Set}, Acc) ->
    true = ets:insert(Set, Acc),
    {ok, State};
mset(Bucket, [{Key, Val} | KeyVals], State, Acc) ->
    mset(Bucket, KeyVals, State, [{{Bucket, Key}, Val} | Acc]).

%% Hashes
hgetall([], _Bucket, _Key, State, Acc) ->
    {ok, lists:reverse(Acc), State};
hgetall([{{Bucket, {Key, Field}}, Val} | Objects], Bucket, Key, State, Acc) ->
    hgetall(Objects, Bucket, Key, State, [{Field, Val} | Acc]);
hgetall([Error | _Objects], _Bucket, _Key, State, _Acc) ->
    {error, Error, State}.

hmget(_Bucket, _Key, [], State, Acc) ->
    {ok, lists:reverse(Acc), State};
hmget(Bucket, Key, [Field | Fields], State=#state{set=Set}, Acc) ->
    case ets:lookup(Set, {Bucket, {Key, Field}}) of
        [] ->
            hmget(Bucket, Key, Fields, State, [undefined | Acc]);
        [{{Bucket, {Key, Field}}, Val}] ->
            hmget(Bucket, Key, Fields, State, [Val | Acc]);
        Error ->
            {error, Error, State}
    end.

hmset(_Bucket, _Key, [], State=#state{set=Set}, Acc) ->
    true = ets:insert(Set, Acc),
    {ok, State};
hmset(Bucket, Key, [{Field, Val} | FieldVals], State, Acc) ->
    hmset(Bucket, Key, FieldVals, State, [{{Bucket, {Key, Field}}, Val} | Acc]).

%% Sets
sadd(_Bucket, _Key, [], State=#state{set=Set}, Acc) ->
    true = ets:insert(Set, Acc),
    {ok, State};
sadd(Bucket, Key, [Val | Vals], State, Acc) ->
    sadd(Bucket, Key, Vals, State, [{{Bucket, {Key, Val}}} | Acc]).

smembers([], _Bucket, _Key, State, Acc) ->
    {ok, lists:reverse(Acc), State};
smembers([[Val] | Objects], Bucket, Key, State, Acc) ->
    smembers(Objects, Bucket, Key, State, [Val | Acc]);
smembers([Error | _Objects], _Bucket, _Key, State, _Acc) ->
    {error, Error, State}.
