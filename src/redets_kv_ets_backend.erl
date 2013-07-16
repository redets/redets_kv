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
-export([handle_timeout/2, match_object/3, search/2]). %% Persistence & Search
-export([del/3, get/3, getdel/3, getset/4, incr/3, incrby/4, mget/3, mset/3, set/4]). %% Strings
-export([hdel/4, hget/4, hgetall/3, hincr/4, hincrby/5, hmget/4, hmset/4, hset/5]). %% Hashes
-export([sadd/4, sismember/4, smembers/3, srem/4]). %% Sets

-define(SNAME(R), list_to_atom("redets_kv_set_"++atom_to_list(R))).

-record(state, {
    name = undefined :: undefined | atom(),
    set  = undefined :: undefined | ets:tid()
}).

%%%===================================================================
%%% redets_kv_backend callbacks
%%%===================================================================

start(StoreName, _Config) ->
    Set = ets:new(?SNAME(StoreName), [ordered_set, public, named_table]),
    {ok, #state{name=StoreName, set=Set}}.

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
%%% Persistence & Search
%%%===================================================================

handle_timeout(Ref, State) ->
    case match_object('$redets_kv_timeout', {'_', {Ref, '_'}}, State) of
        {ok, [{{Op, Params}, {Ref, _Timestamp}}], State2} ->
            case del('$redets_kv_timeout', [{Op, Params}], State2) of
                {ok, State3} ->
                    erlang:apply(?MODULE, Op, Params ++ [State3]);
                DelError ->
                    DelError
            end;
        {ok, BadResult, State2} ->
            {error, BadResult, State2};
        MatchObjectError ->
            MatchObjectError
    end.

match_object(Bucket, MatchPattern, State) ->
    case pattern_to_spec(Bucket, MatchPattern) of
        MatchSpec when is_list(MatchSpec) ->
            bucket_select(Bucket, MatchSpec, State);
        Error ->
            {error, Error, State}
    end.

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

getdel(Bucket, Key, State) ->
    case get(Bucket, Key, State) of
        {ok, OldVal, State2} ->
            case del(Bucket, [Key], State2) of
                {ok, State3} ->
                    {ok, OldVal, State3};
                DelError ->
                    DelError
            end;
        GetError ->
            GetError
    end.

getset(Bucket, Key, Val, State) ->
    case get(Bucket, Key, State) of
        {ok, OldVal, State2} ->
            case set(Bucket, Key, Val, State2) of
                {ok, State3} ->
                    {ok, OldVal, State3};
                SetError ->
                    SetError
            end;
        GetError ->
            GetError
    end.

incr(Bucket, Key, State) ->
    incrby(Bucket, Key, 1, State).

incrby(Bucket, Key, Increment, State=#state{set=Set}) ->
    case get(Bucket, Key, State) of
        {ok, OldVal, State2} when is_integer(OldVal) ->
            case catch ets:update_counter(Set, {Bucket, Key}, {2, Increment}) of
                Val when is_integer(Val) ->
                    {ok, Val, State2};
                Error ->
                    {error, Error, State2}
            end;
        {ok, undefined, State2} ->
            case set(Bucket, Key, Increment, State2) of
                {ok, State3} ->
                    {ok, Increment, State3};
                {error, Reason, State3} ->
                    {error, Reason, State3}
            end;
        {ok, OldVal, State2} ->
            {error, {non_integer_value, OldVal}, State2};
        {error, Reason, State2} ->
            {error, Reason, State2}
    end.

mget(Bucket, Keys, State) ->
    mget(Bucket, Keys, State, []).

mset(Bucket, KeyVals, State) ->
    mset(Bucket, KeyVals, State, []).

set(Bucket, Key, {'$redets_kv_function', Fun}, State) when is_function(Fun) ->
    set(Bucket, Key, Fun(), State);
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

hincr(Bucket, Key, Field, State) ->
    hincrby(Bucket, Key, Field, 1, State).

hincrby(Bucket, Key, Field, Increment, State=#state{set=Set}) ->
    case hget(Bucket, Key, Field, State) of
        {ok, OldVal, State2} when is_integer(OldVal) ->
            case catch ets:update_counter(Set, {Bucket, {Key, Field}}, {2, Increment}) of
                Val when is_integer(Val) ->
                    {ok, Val, State2};
                Error ->
                    {error, Error, State2}
            end;
        {ok, undefined, State2} ->
            case hset(Bucket, Key, Field, Increment, State2) of
                {ok, State3} ->
                    {ok, Increment, State3};
                {error, Reason, State3} ->
                    {error, Reason, State3}
            end;
        {ok, OldVal, State2} ->
            {error, {non_integer_value, OldVal}, State2};
        {error, Reason, State2} ->
            {error, Reason, State2}
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

%% Persistence & Search
bucket_select(Bucket, MatchSpec, State=#state{set=Set}) ->
    bucket_select(ets:select(Set, MatchSpec), Bucket, State, []).

bucket_select([], _Bucket, State, Acc) ->
    {ok, lists:reverse(Acc), State};
bucket_select([{{Bucket, Key}} | Results], Bucket, State, Acc) ->
    bucket_select(Results, Bucket, State, [{Key} | Acc]);
bucket_select([{{Bucket, Key}, Val} | Results], Bucket, State, Acc) ->
    bucket_select(Results, Bucket, State, [{Key, Val} | Acc]);
bucket_select([BadResult | _], _Bucket, State, _Acc) ->
    {error, BadResult, State}.

compile_pattern(Bucket, '_') ->
    [
        {{Bucket, '_'}, '_'}, %% Strings
        {{Bucket, {'_', '_'}}, '_'}, %% Hashes
        {{Bucket, {'_', '_'}}} %% Sets
    ];
compile_pattern(Bucket, Atom) when is_atom(Atom) ->
    case atom_to_binary(Atom, utf8) of
        << $$, Rest/binary >> ->
            case catch binary_to_integer(Rest) of
                Integer when is_integer(Integer) ->
                    compile_pattern(Bucket, '_');
                _ ->
                    [Atom]
            end;
        _ ->
            [Atom]
    end;
compile_pattern(Bucket, {Key}) ->
    [{{Bucket, Key}}];
compile_pattern(Bucket, {Key, Val}) ->
    [{{Bucket, Key}, Val}];
compile_pattern(_Bucket, _BadArg) ->
    [].

pattern_to_spec(Bucket, Pattern) ->
    pattern_to_match_spec(compile_pattern(Bucket, Pattern), []).

pattern_to_match_spec([], Spec) ->
    lists:reverse(Spec);
pattern_to_match_spec([Pattern | Patterns], Spec) ->
    pattern_to_match_spec(Patterns, [{Pattern, [], ['$_']} | Spec]).

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
