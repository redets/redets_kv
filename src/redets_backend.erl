%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   2 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_backend).

-type bucket() :: atom() | {atom(), bucket()}.
-type match_spec() :: ets:match_spec() | {atom(), match_spec()}.

-callback start(Partition::integer(), Config::[proplists:property()])
    -> {ok, State::any()} | {error, Reason::any()}.
-callback stop(State::any())
    -> ok.
-callback status(State::any())
    -> [proplists:property()].

%% Search
-callback search(MatchSpec::match_spec(), State::any())
    -> {ok, Vals::[any()], State::any()} | {error, Reason::any(), State::any()}.

%% Strings
-callback del(Bucket::bucket(), Keys::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback get(Bucket::bucket(), Key::any(), State::any())
    -> {ok, Val::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback set(Bucket::bucket(), Key::any(), Val::any(), State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.

%% Hashes
-callback hdel(Bucket::bucket(), Key::any(), Fields::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback hget(Bucket::bucket(), Key::any(), Field::any(), State::any())
    -> {ok, Val::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback hgetall(Bucket::bucket(), Key::any(), State::any())
    -> {ok, FieldVals::[{any(),any()}], State::any()} | {error, Reason::any(), State::any()}.
-callback hmget(Bucket::bucket(), Key::any(), Fields::[any()], State::any())
    -> {ok, Vals::[any()], State::any()} | {error, Reason::any(), State::any()}.
-callback hmset(Bucket::bucket(), Key::any(), FieldVals::[{any(), any()}], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback hset(Bucket::bucket(), Key::any(), Field::any(), Val::any(), State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.

%% Sets
-callback sadd(Bucket::bucket(), Key::any(), Vals::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback sismember(Bucket::bucket(), Key::any(), Val::any(), State::any())
    -> {ok, boolean(), State::any()} | {error, Reason::any(), State::any()}.
-callback smembers(Bucket::bucket(), Key::any(), State::any())
    -> {ok, Vals::[any()], State::any()} | {error, Reason::any(), State::any()}.
-callback srem(Bucket::bucket(), Key::any(), Vals::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
