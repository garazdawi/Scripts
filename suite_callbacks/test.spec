{logdir, "logdir"}.
{suites, ".", [test_SUITE]}.
{suite_callbacks, [{ct_junit_report,[]}]}.
{event_handler, [test_event]}.
