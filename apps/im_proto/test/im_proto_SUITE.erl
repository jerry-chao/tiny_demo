%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created : 31 Jan 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(im_proto_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{varint, [sequence], [
                           encode_varint,
                           decode_varint
                        ]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, varint}].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() ->
    [].

benchmark_rand(Config) when is_list(Config) ->
    %% run timer:sleep(1000) for 5 second, 4 runners
    {ok, Job} = erlperf_job:start_link(#{runner => {timer, sleep, [1000]}}),
    Handle = erlperf_job:handle(Job),
    ok = erlperf_job:set_concurrency(Job, 4), %% 4 runner instances
    InitialIterations = erlperf_job:sample(Handle),
    timer:sleep(5000),
    IterationsIn5Sec = erlperf_job:sample(Handle) - InitialIterations,
    erlperf_job:request_stop(Job), %% use gen:stop(Job) for synchronous call
    %% expect at least 16 iterations (and up to 20)
    ?assert(IterationsIn5Sec >= 16, {too_slow, IterationsIn5Sec}),
    ?assert(IterationsIn5Sec =< 20, {too_fast, IterationsIn5Sec}).

benchmark_ets_insert() ->
    catch ets:delete(room_user),
    ets:new(room_user, [named_table, duplicate_bag, public, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, Job} = erlperf_job:start_link(#{runner => fun() ->
                                                           catch ets:insert(room_user, {<<"test">>, rand:uniform()})
                                                   end}),
    Handle = erlperf_job:handle(Job),
    ok = erlperf_job:set_concurrency(Job, 4), %% 4 runner instances
    InitialIterations = erlperf_job:sample(Handle),
    timer:sleep(5000),
    IterationsIn5Sec = erlperf_job:sample(Handle) - InitialIterations,
    erlperf_job:request_stop(Job), %% use gen:stop(Job) for synchronous call
    %% expect at least 16 iterations (and up to 20)
    IterationsIn5Sec.

decode_varint(Config) ->
    ?assertEqual({1, <<>>}, im_proto:decode_varint(<<1:8>>)),
    ?assertEqual({1, <<"foo">>}, im_proto:decode_varint(<<1:8, "foo">>)),
    ?assertEqual({300, <<>>}, im_proto:decode_varint(<<44034:16>>)),
    ?assertEqual({300, <<"bar">>}, im_proto:decode_varint(<<44034:16, "bar">>)),
    Config.

encode_varint(Config) ->
    ?assertEqual(<<1:8>>, im_proto:encode_varint(1)),
    ?assertEqual(<<44034:16>>, im_proto:encode_varint(300)),
    ?assertError({badarg, -7}, im_proto:encode_varint(-7)),
    Config.

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
my_test_case(_Config) ->
    ok.
