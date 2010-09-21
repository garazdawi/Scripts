%%%-------------------------------------------------------------------
%%% @author Adam Lindberg <adam@erlang-consulting.com>
%%% @copyright (C) 2010, Erlang Training and Consulting Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2010 by Adam Lindberg <adam@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(test_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{suite_callbacks, [{ct_junit_report, []}]}].

groups() ->
    [{group, [sequence], [test_case, test_case1]}].

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    [{whatever,test}|Config].

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, group}].

%% Test cases starts here.
%%--------------------------------------------------------------------
test_case() ->
    [{doc, "Describe the main purpose of this test case"}].

test_case(Config) when is_list(Config) ->
    timer:sleep(2000),
    ok = ok.


test_case1() ->
    [{doc, "Describe the main purpose of this test case"}].

test_case1(Config) when is_list(Config) ->
    timer:sleep(2000),
    ok = nok.
