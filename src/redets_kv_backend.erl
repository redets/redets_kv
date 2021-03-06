%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @copyright 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :   2 Jul 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(redets_kv_backend).

-type bucket() :: atom() | {atom(), atom()}.
-type match_spec() :: ets:match_spec() | {atom(), match_spec()}.

-callback start(StoreName::atom(), Config::[proplists:property()])
    -> {ok, State::any()} | {error, Reason::any()}.
-callback stop(State::any())
    -> ok.

%% Status
-callback nodes(State::any())
    -> [node()].
-callback status(State::any())
    -> [proplists:property()].

%% Persistence & Search
-callback handle_timeout(Ref::reference(), State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback match_delete(Bucket::bucket(), MatchPattern::ets:match_pattern(), State::any())
    -> {ok, NumDeleted::integer(), State::any()} | {error, Reason::any(), State::any()}.
-callback match_object(Bucket::bucket(), MatchPattern::ets:match_pattern(), State::any())
    -> {ok, Records::[any()], State::any()} | {error, Reason::any(), State::any()}.
-callback search(MatchSpec::match_spec(), State::any())
    -> {ok, Vals::[any()], State::any()} | {error, Reason::any(), State::any()}.

%% Strings
-callback del(Bucket::bucket(), Keys::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback get(Bucket::bucket(), Key::any(), State::any())
    -> {ok, Val::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback getdel(Bucket::bucket(), Key::any(), State::any())
    -> {ok, Val::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback getset(Bucket::bucket(), Key::any(), Val::any(), State::any())
    -> {ok, OldVal::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback incr(Bucket::bucket(), Key::any(), State::any())
    -> {ok, Val::integer(), State::any()} | {error, Reason::any(), State::any()}.
-callback incrby(Bucket::bucket(), Key::any(), Increment::integer(), State::any())
    -> {ok, Val::integer(), State::any()} | {error, Reason::any(), State::any()}.
-callback mget(Bucket::bucket(), Keys::[any()], State::any())
    -> {ok, Vals::[any()], State::any()} | {error, Reason::any(), State::any()}.
-callback mset(Bucket::bucket(), KeyVals::[{any(), any()}], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback set(Bucket::bucket(), Key::any(), Val::any(), State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.

%% Hashes
-callback hdel(Bucket::bucket(), Key::any(), Fields::[any()], State::any())
    -> {ok, State::any()} | {error, Reason::any(), State::any()}.
-callback hget(Bucket::bucket(), Key::any(), Field::any(), State::any())
    -> {ok, Val::any(), State::any()} | {error, Reason::any(), State::any()}.
-callback hgetall(Bucket::bucket(), Key::any(), State::any())
    -> {ok, FieldVals::[{any(),any()}], State::any()} | {error, Reason::any(), State::any()}.
-callback hincr(Bucket::bucket(), Key::any(), Field::any(), State::any())
    -> {ok, Val::integer(), State::any()} | {error, Reason::any(), State::any()}.
-callback hincrby(Bucket::bucket(), Key::any(), Field::any(), Increment::integer(), State::any())
    -> {ok, Val::integer(), State::any()} | {error, Reason::any(), State::any()}.
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
