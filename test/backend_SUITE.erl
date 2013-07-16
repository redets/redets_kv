-module(backend_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([strings_single/1, strings_multiple/1, hashes_single/1, hashes_multiple/1, sets_single/1, sets_multiple/1, handle_timeouts/1, increments/1]).

all() ->
    [
        {group, test_ets}
    ].

groups() ->
    Tests = [
        strings_single,
        strings_multiple,
        hashes_single,
        hashes_multiple,
        sets_single,
        sets_multiple,
        handle_timeouts,
        increments
    ],
    [
        {test_ets, [parallel], Tests}
    ].

init_per_suite(Config) ->
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    application:stop(sasl),
    ok.

init_per_group(Name, Config) ->
    application:start(redets_kv),
    Backend = backend_for_group(Name),
    Bucket = bucket_for_group(Name),
    Store = store_for_group(Name),
    redets_kv:new_store([{name, Store}, {backend, Backend}, {options, []}]),
    [{bucket, Bucket}, {store, Store} | Config].

end_per_group(Name, _Config) ->
    Store = store_for_group(Name),
    redets_kv:rm_store(Store),
    application:stop(redets_kv),
    ok.

%%====================================================================
%% Tests
%%====================================================================

strings_single(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, undefined} = redets_kv:call(Store, get, [Bucket, string]),
    ok = redets_kv:call(Store, set, [Bucket, string, val]),
    {ok, val} = redets_kv:call(Store, get, [Bucket, string]),
    ok = redets_kv:call(Store, del, [Bucket, [string]]),
    {ok, undefined} = redets_kv:call(Store, get, [Bucket, string]),
    {ok, undefined} = redets_kv:call(Store, getset, [Bucket, string, val0]),
    {ok, val0} = redets_kv:call(Store, getset, [Bucket, string, val1]),
    {ok, val1} = redets_kv:call(Store, getdel, [Bucket, string]),
    ok.

strings_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, [undefined, undefined, undefined]} = redets_kv:call(Store, mget, [Bucket, [a,b,c]]),
    ok = redets_kv:call(Store, mset, [Bucket, [{a,1},{b,2},{c,3}]]),
    {ok, [1,2,3]} = redets_kv:call(Store, mget, [Bucket, [a,b,c]]),
    ok = redets_kv:call(Store, del, [Bucket, [a,b,c]]),
    {ok, [undefined, undefined, undefined]} = redets_kv:call(Store, mget, [Bucket, [a,b,c]]),
    ok.

hashes_single(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, undefined} = redets_kv:call(Store, hget, [Bucket, hash, field]),
    {ok, []} = redets_kv:call(Store, hgetall, [Bucket, hash]),
    ok = redets_kv:call(Store, hset, [Bucket, hash, field, val]),
    {ok, val} = redets_kv:call(Store, hget, [Bucket, hash, field]),
    {ok, [{field,val}]} = redets_kv:call(Store, hgetall, [Bucket, hash]),
    ok = redets_kv:call(Store, hdel, [Bucket, hash, [field]]),
    {ok, undefined} = redets_kv:call(Store, hget, [Bucket, hash, field]),
    {ok, []} = redets_kv:call(Store, hgetall, [Bucket, hash]),
    ok.

hashes_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, [undefined, undefined, undefined]} = redets_kv:call(Store, hmget, [Bucket, mhash, [a,b,c]]),
    ok = redets_kv:call(Store, hmset, [Bucket, mhash, [{a,1},{b,2},{c,3}]]),
    {ok, [1,2,3]} = redets_kv:call(Store, hmget, [Bucket, mhash, [a,b,c]]),
    ok = redets_kv:call(Store, del, [Bucket, [mhash]]),
    {ok, [undefined, undefined, undefined]} = redets_kv:call(Store, hmget, [Bucket, mhash, [a,b,c]]),
    ok.

sets_single(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, false} = redets_kv:call(Store, sismember, [Bucket, set, a]),
    ok = redets_kv:call(Store, sadd, [Bucket, set, [a]]),
    {ok, true} = redets_kv:call(Store, sismember, [Bucket, set, a]),
    ok = redets_kv:call(Store, srem, [Bucket, set, [a]]),
    {ok, false} = redets_kv:call(Store, sismember, [Bucket, set, a]),
    ok.

sets_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, []} = redets_kv:call(Store, smembers, [Bucket, mset]),
    ok = redets_kv:call(Store, sadd, [Bucket, mset, [a,b,c]]),
    {ok, Ret} = redets_kv:call(Store, smembers, [Bucket, mset]),
    [a,b,c] = lists:sort(Ret),
    ok = redets_kv:call(Store, srem, [Bucket, mset, [b,c]]),
    {ok, [a]} = redets_kv:call(Store, smembers, [Bucket, mset]),
    ok = redets_kv:call(Store, del, [Bucket, [mset]]),
    {ok, []} = redets_kv:call(Store, smembers, [Bucket, mset]),
    ok.

handle_timeouts(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, []} = redets_kv:call(Store, smembers, [Bucket, tokens]),
    ok = redets_kv:call(Store, sadd, [Bucket, tokens, [a,b,c]]),
    {ok, Ret0} = redets_kv:call(Store, smembers, [Bucket, tokens]),
    [a,b,c] = lists:sort(Ret0),
    ok = redets_kv:call_after(1000, Store, srem, [Bucket, tokens, [a,c]]),
    {ok, Ret1} = redets_kv:call(Store, smembers, [Bucket, tokens]),
    [a,b,c] = lists:sort(Ret1),
    _ = timer:sleep(1100),
    {ok, Ret2} = redets_kv:call(Store, smembers, [Bucket, tokens]),
    [b] = lists:sort(Ret2),
    ok = redets_kv:call(Store, del, [Bucket, [tokens]]),
    {ok, []} = redets_kv:call(Store, smembers, [Bucket, tokens]),
    ok.

increments(Config) ->
    Bucket = ?config(bucket, Config),
    Store = ?config(store, Config),
    {ok, undefined} = redets_kv:call(Store, get, [Bucket, inc]),
    ok = redets_kv:call(Store, set, [Bucket, inc, 0]),
    {ok, 0} = redets_kv:call(Store, get, [Bucket, inc]),
    {ok, 1} = redets_kv:call(Store, incr, [Bucket, inc]),
    {ok, 1} = redets_kv:call(Store, get, [Bucket, inc]),
    {ok, 101} = redets_kv:call(Store, incrby, [Bucket, inc, 100]),
    {ok, 51} = redets_kv:call(Store, incrby, [Bucket, inc, -50]),
    ok = redets_kv:call(Store, del, [Bucket, [inc]]),
    {ok, undefined} = redets_kv:call(Store, get, [Bucket, inc]),
    {ok, undefined} = redets_kv:call(Store, hget, [Bucket, hinc, key]),
    ok = redets_kv:call(Store, hset, [Bucket, hinc, key, 0]),
    {ok, 0} = redets_kv:call(Store, hget, [Bucket, hinc, key]),
    {ok, 1} = redets_kv:call(Store, hincr, [Bucket, hinc, key]),
    {ok, 1} = redets_kv:call(Store, hget, [Bucket, hinc, key]),
    {ok, 101} = redets_kv:call(Store, hincrby, [Bucket, hinc, key, 100]),
    {ok, 51} = redets_kv:call(Store, hincrby, [Bucket, hinc, key, -50]),
    ok = redets_kv:call(Store, del, [Bucket, [hinc]]),
    {ok, undefined} = redets_kv:call(Store, get, [Bucket, hinc]),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

backend_for_group(test_ets) ->
    redets_kv_ets_backend.

bucket_for_group(test_ets) ->
    bucket.

store_for_group(Group) ->
    Group.
