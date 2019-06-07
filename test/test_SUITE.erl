-module(test_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([timed_batch/1,
         sized_batch/1,
         invalid_batch/1,
         invalid_batch_format/1]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {minutes, 2}},
   {require, {test_harness, [webserver_port,
                             db_dir]}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) -> Config1
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before the suite.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  [application:set_env(test_harness, Name, Value) ||
    {Name, Value} <- ct:get_config(test_harness)],
  {ok, _GunStarted} = application:ensure_all_started(gun),
  {ok, _TestHarnessStarted} = application:ensure_all_started(test_harness),
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> term()
%%
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) -> Config1
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case.
%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
  WebserverPort = ct:get_config({test_harness, webserver_port}),
  {ok, ConnPid} = gun:open("localhost", WebserverPort),
  {ok, _Protocol} = gun:await_up(ConnPid),
  test_harness_db:delete_all(),
  [{connection, ConnPid} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> term()
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_Case, Config) ->
  ConnPid = ?config(connection, Config),
  gun:close(ConnPid),
  ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
  [sized_batch,
   timed_batch,
   invalid_batch,
   invalid_batch_format].

%% helpers
post_json(ConnPid, Body) ->
  StreamRef = gun:post(ConnPid, "/speeds", [], Body),
  gun:await(ConnPid, StreamRef).

utc_time() ->
  SysTime = erlang:system_time(millisecond),
  FormattedTime = calendar:system_time_to_rfc3339(SysTime, [{unit, millisecond},
                                                            {offset, "Z"}]),
  unicode:characters_to_binary(FormattedTime).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

sized_batch(Config) ->
  ConnPid = ?config(connection, Config),
  lists:map(fun
              (I) ->
                Body = jiffy:encode(#{speed => I * 1.3, timestamp => utc_time()}),
                {response, _, 200, _Headers} = post_json(ConnPid, Body)
            end, lists:seq(1, 20)),
  ct:print("One batch should have been written."),
  Speeds = [#{<<"speed">> := 1.3}, #{<<"speed">> := 2.6} | _RestSpeed] =
    test_harness_speeds_entity:list(),
  19 = length(Speeds).

timed_batch(Config) ->
  ConnPid = ?config(connection, Config),
  lists:map(fun
              (I) ->
                Body = jiffy:encode(#{speed => I * 1.3, timestamp => utc_time()}),
                {response, _, 200, _Headers} = post_json(ConnPid, Body)
            end, lists:seq(1, 3)),
  ct:print("Waiting 62 seconds for the batch to flush! Do not adjust your television."),
  ct:sleep({seconds, 62}),
  [#{<<"speed">> := 1.3}, #{<<"speed">> := 2.6} | _RestSpeed] =
    test_harness_speeds_entity:list().

invalid_batch(Config) ->
  ConnPid = ?config(connection, Config),
  {response, _, 400, _Headers} = post_json(ConnPid, <<"not json">>).

invalid_batch_format(Config) ->
  ConnPid = ?config(connection, Config),
  Body = jiffy:encode(#{speed => <<"wrong!">>, timestamp => utc_time()}),
  {response, _, 422, _Headers} = post_json(ConnPid, Body).
