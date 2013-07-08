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
-export([strings_single/1, strings_multiple/1, hashes_single/1, hashes_multiple/1, sets_single/1, sets_multiple/1]).

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
        sets_multiple
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
    application:start(redets),
    Backend = backend_for_group(Name),
    Bucket = bucket_for_group(Name),
    Storage = storage_for_group(Name),
    redets:start_storage(Storage, Backend, []),
    [{bucket, Bucket}, {storage, Storage} | Config].

end_per_group(Name, _Config) ->
    Storage = storage_for_group(Name),
    redets:stop_storage(Storage),
    application:stop(redets),
    ok.

%%====================================================================
%% Tests
%%====================================================================

strings_single(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, undefined} = redets:call(Storage, get, [Bucket, string]),
    ok = redets:call(Storage, set, [Bucket, string, val]),
    {ok, val} = redets:call(Storage, get, [Bucket, string]),
    ok = redets:call(Storage, del, [Bucket, [string]]),
    {ok, undefined} = redets:call(Storage, get, [Bucket, string]),
    ok.

strings_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, [undefined, undefined, undefined]} = redets:call(Storage, mget, [Bucket, [a,b,c]]),
    ok = redets:call(Storage, mset, [Bucket, [{a,1},{b,2},{c,3}]]),
    {ok, [1,2,3]} = redets:call(Storage, mget, [Bucket, [a,b,c]]),
    ok = redets:call(Storage, del, [Bucket, [a,b,c]]),
    {ok, [undefined, undefined, undefined]} = redets:call(Storage, mget, [Bucket, [a,b,c]]),
    ok.

hashes_single(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, undefined} = redets:call(Storage, hget, [Bucket, hash, field]),
    {ok, []} = redets:call(Storage, hgetall, [Bucket, hash]),
    ok = redets:call(Storage, hset, [Bucket, hash, field, val]),
    {ok, val} = redets:call(Storage, hget, [Bucket, hash, field]),
    {ok, [{field,val}]} = redets:call(Storage, hgetall, [Bucket, hash]),
    ok = redets:call(Storage, hdel, [Bucket, hash, [field]]),
    {ok, undefined} = redets:call(Storage, hget, [Bucket, hash, field]),
    {ok, []} = redets:call(Storage, hgetall, [Bucket, hash]),
    ok.

hashes_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, [undefined, undefined, undefined]} = redets:call(Storage, hmget, [Bucket, mhash, [a,b,c]]),
    ok = redets:call(Storage, hmset, [Bucket, mhash, [{a,1},{b,2},{c,3}]]),
    {ok, [1,2,3]} = redets:call(Storage, hmget, [Bucket, mhash, [a,b,c]]),
    ok = redets:call(Storage, del, [Bucket, [mhash]]),
    {ok, [undefined, undefined, undefined]} = redets:call(Storage, hmget, [Bucket, mhash, [a,b,c]]),
    ok.

sets_single(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, false} = redets:call(Storage, sismember, [Bucket, set, a]),
    ok = redets:call(Storage, sadd, [Bucket, set, [a]]),
    {ok, true} = redets:call(Storage, sismember, [Bucket, set, a]),
    ok = redets:call(Storage, srem, [Bucket, set, [a]]),
    {ok, false} = redets:call(Storage, sismember, [Bucket, set, a]),
    ok.

sets_multiple(Config) ->
    Bucket = ?config(bucket, Config),
    Storage = ?config(storage, Config),
    {ok, []} = redets:call(Storage, smembers, [Bucket, mset]),
    ok = redets:call(Storage, sadd, [Bucket, mset, [a,b,c]]),
    {ok, Ret} = redets:call(Storage, smembers, [Bucket, mset]),
    [a,b,c] = lists:sort(Ret),
    ok = redets:call(Storage, srem, [Bucket, mset, [b,c]]),
    {ok, [a]} = redets:call(Storage, smembers, [Bucket, mset]),
    ok = redets:call(Storage, del, [Bucket, [mset]]),
    {ok, []} = redets:call(Storage, smembers, [Bucket, mset]),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

backend_for_group(test_ets) ->
    redets_ets_backend.

bucket_for_group(test_ets) ->
    bucket.

storage_for_group(Group) ->
    Group.
