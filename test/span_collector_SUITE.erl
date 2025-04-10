-module(span_collector_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("test_logs/include/test_logs.hrl").

%% otel_tracer.hrl defines tracing marcos (e.g. ?with_span())
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
%% opentelemetry.hrl contains #span_ctx{} record declaration
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
%% otel_span.hrl contains #span{} record declaration
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

-export([build_span_tree_prop_test/1,
         ensure_started_test/1,
         build_span_tree_test/1,
         get_spans_by_name_test/1,
         span_is_not_unique_test/1,
         wait_for_span_test/1,
         reset_test/1,
         failing_to_start_test/1,
         unknown_call_test/1,
         unknown_cast_test/1,
         unknown_info_test/1]).

-define(NUMBER_OF_REPETITIONS, 100).
-define(TABLE,                 '$spans_table').
-define(GEN_SERVER_NAME,       span_collector).

-define(MILLISECONDS(Action),
        element(1, timer:tc(fun() -> Action end, millisecond))).

-define(assertNoDelay(Action), ?assert(?MILLISECONDS(Action) < 2)).
-define(assertTimeout(Action, Timeout), ?assert(?MILLISECONDS(Action) >= Timeout)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [{group, basic},
     {group, errors_logging},
     {group, proper}].


groups() ->
    [{basic, [],
             [ensure_started_test,
              reset_test,
              wait_for_span_test,
              get_spans_by_name_test,
              build_span_tree_test]},
     {errors_logging, [],
                      [failing_to_start_test,
                       span_is_not_unique_test,
                       unknown_call_test,
                       unknown_cast_test,
                       unknown_info_test]},
     {proper, [parallel],
              [build_span_tree_prop_test,
               build_span_tree_prop_test,
               build_span_tree_prop_test,
               build_span_tree_prop_test]}].


init_per_suite(Config) ->
    opentelemetry_testing:ensure_started(),
    Config.


end_per_suite(Config) ->
    Config.


init_per_group(proper, Config) ->
    span_collector:reset(),
    Config;
init_per_group(errors_logging, Config) ->
    test_logs:add_handler(),
    span_collector:reset(),
    Config;
init_per_group(_Group, Config) ->
    Config.


end_per_group(proper, Config) ->
    span_collector:reset(),
    Config;
end_per_group(errors_logging, Config) ->
    test_logs:remove_handler(),
    span_collector:ensure_started(),
    Config;
end_per_group(_Group, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


reset_test(_Config) ->
    Size = ets:info(?TABLE, size),
    SpanTreeInputData = {#{name => <<"some_span">>}, []},
    generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual(Size + 1, ets:info(?TABLE, size)),
    span_collector:reset(),
    ?assertEqual(0, ets:info(?TABLE, size)).


wait_for_span_test(_Config) ->
    Timeout = 300,
    SpanTreeInputData = {#{name => <<"some_span">>}, []},
    {#span{trace_id = TraceId, span_id = SpanId} = ExpectedSpan, []} =
        generate_span_tree(SpanTreeInputData),
    Ret = span_collector:wait_for_span(TraceId, SpanId, Timeout),
    ?assertMatch({ok, _}, Ret),
    {ok, Span} = Ret,
    ?assertEqual(ExpectedSpan, remove_end_time(Span)),
    %% subsequent calls to span_collector:wait_for_span/3 must
    %% immediately return the same span
    ?assertNoDelay(
      ?assertEqual(Ret,
                   span_collector:wait_for_span(TraceId, SpanId, Timeout))),
    ?assertNoDelay(
      ?assertEqual(Ret,
                   span_collector:wait_for_span(TraceId, SpanId, 0))),

    %% check that timeout works as expected
    span_collector:reset(),
    ?assertTimeout(?assertEqual(
                     {error, timeout},
                     span_collector:wait_for_span(TraceId, SpanId, Timeout)),
                   Timeout),
    ?assertNoDelay(?assertEqual({error, timeout},
                                span_collector:wait_for_span(TraceId, SpanId, 0))).


get_spans_by_name_test(_Config) ->
    SpanName = <<"some_span">>,
    SpanTreeInputData = {#{name => SpanName}, []},
    span_collector:reset(),
    ?assertEqual({error, not_found}, span_collector:get_span_ids_by_name(SpanName)),
    ?assertEqual([], span_collector:get_spans_by_name(SpanName)),
    {#span{trace_id = TraceId, span_id = SpanId} = ExpectedSpan1, _} =
        generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual({ok, {TraceId, SpanId}},
                 span_collector:get_span_ids_by_name(SpanName)),
    [Span] = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(ExpectedSpan1, remove_end_time(Span)),
    {ExpectedSpan2, _} = generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual({error, span_is_not_unique},
                 span_collector:get_span_ids_by_name(SpanName)),
    Spans = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(lists:sort([ExpectedSpan1, ExpectedSpan2]),
                 lists:sort([ remove_end_time(S) || S <- Spans ])).


build_span_tree_test(_Config) ->
    SpanTreeInputData = {#{name => <<"some_span">>},
                         [{#{name => <<"another_span">>},
                           [{#{name => <<"yet_another_span">>}, []}]},
                          {#{name => <<"another_span">>}, []}]},
    {#span{trace_id = TraceId, span_id = RootSpanId}, _} = ExpectedSpanTree =
        generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual(
      {ok, ExpectedSpanTree},
      span_collector:build_span_tree(TraceId, RootSpanId, fun remove_end_time/1)),
    %% span_collector:build_span_tree/2 returns the same tree if called twice.
    ?assertEqual(
      {ok, ExpectedSpanTree},
      span_collector:build_span_tree(TraceId, RootSpanId, fun remove_end_time/1)),

    %% span tree is building successfully for non-root spans
    [ ?assertEqual(
        {ok, SubTree},
        span_collector:build_span_tree(TraceId, SpanId, fun remove_end_time/1))
      || {#span{span_id = SpanId}, _} = SubTree <- element(2, ExpectedSpanTree) ],

    span_collector:reset(),
    ?assertEqual(
      {error, not_found},
      span_collector:build_span_tree(TraceId, RootSpanId, fun remove_end_time/1)).


ensure_started_test(_Config) ->
    %% basic check if span_collector:ensure_started/0 interface is idempotent
    ?assertEqual(ok, span_collector:ensure_started()),
    ?assertEqual(ok, span_collector:ensure_started()).


span_is_not_unique_test(_Config) ->
    test_logs:set_pid(),
    span_collector:ensure_started(),
    {#span{trace_id = TraceId, span_id = SpanId} = Span, []} =
        generate_span_tree_and_wait({#{name => <<"some_span">>}, []}),

    %% since Span has no end time, it is different from the record stored
    %% in span_collector's ETS. because the ETS type is bag, span_collector
    %% will successfully store the Span record to ETS.
    erlang:send(?GEN_SERVER_NAME, {span, Span}),
    timer:sleep(100),

    ?assertEqual(
      {error, span_is_not_unique},
      span_collector:build_span_tree(TraceId, SpanId, fun remove_end_time/1)),
    ?assertLogEvent({"multiple spans matched:" ++ _, _},
                    error,
                    #{mfa := {span_collector, build_span_tree, 3}}),

    ?assertEqual({error, span_is_not_unique},
                 span_collector:wait_for_span(TraceId, SpanId, 0)),
    ?assertLogEvent({"multiple spans matched:" ++ _, _},
                    error,
                    #{mfa := {span_collector, wait_for_span, 3}}).


failing_to_start_test(_Config) ->
    test_logs:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    span_collector:stop(),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    %% create ets table with the same name as span_collector uses, so
    %% span_collector:init/1 crashes on attempt to create ETS table.
    %% there is no need to drop the created ETS, it is automatically
    %% removed when the testcase process stops.
    ets:new(?TABLE, [named_table]),
    ?assertEqual({error, failed_to_start_span_collector},
                 span_collector:ensure_started()),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogEvent({"failed to start span_collector:" ++ _, _},
                    error,
                    #{mfa := {span_collector, ensure_started, 0}}).


unknown_call_test(_Config) ->
    test_logs:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertEqual(not_implemented, gen_server:call(?GEN_SERVER_NAME, some_call)),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogEvent({"unexpected call request:" ++ _, _}, error, _).


unknown_cast_test(_Config) ->
    test_logs:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    gen_server:cast(?GEN_SERVER_NAME, some_cast),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogEvent({"unexpected cast request:" ++ _, _}, error, _).


unknown_info_test(_Config) ->
    test_logs:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    erlang:send(?GEN_SERVER_NAME, some_call),
    timer:sleep(100),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogEvent({"unexpected info message:" ++ _, _}, error, _).


build_span_tree_prop_test(_Config) ->

    %% this test is running in parallel multiple times (see CT 'proper' group
    %% definition) and it was occasionally failing for unexpected reasons.
    %% after spending a day debugging and reading OpenTelemetry and PropEr code,
    %% the real root cause was found. Investigation showed that:
    %%   * PropEr conditionally (if there's no random generator state stored in
    %%     a process dictionary) seeds the random number generator.
    %%   * this conditional seeding is based solely on the value returned by the
    %%     os:timestamp/0 function, for more details see:
    %%     - the implementation of the proper_arith:rand_restart/1 function.
    %%     - the definition of the #opts{} record in the 'proper' module.
    %%     - the implementation of the global_state_init/1 function in the
    %%       'proper' module and its usage
    %%   * If the os:timestamp/0 function returns identical values ​​in two or more
    %%     parallel build_span_tree_prop_test/1 tests, this results in identical input
    %%     generation for those tests
    %%   * but identical test input does not in itself lead to a race condition.
    %%     the real problem is that the random generator seed is still identical
    %%     in those processes, so open telemetry generates identical trace_id and
    %%     span_id for spans (see the 'otel_id_generator' module and its usage).
    %%
    %% the simplest solution would be to add a random delay at the beginning of the
    %% build_span_tree_prop_test/1 test, e.g. timer:sleep(rand:uniform(500)). however,
    %% calling random:uniform/1 initializes the random generator state for the default
    %% algorithm and stores it in a process dictionary. by default, the 'rand' module
    %% generates a seed value based on the process ID (see the default_seed/0 function
    %% in the 'rand' module), so it is guaranteed to be unique for each process. this
    %% solves the issue altogether as the seeding of the random number generator is
    %% conditional in the proper_arith:rand_restart/1 function.
    %%
    %% So instead of introducing a random delay, let's just seed the default random
    %% number generator. since this behavior of PropEr is not documented, it's also
    %% a good idea to re-seed the default random number generator at the beginning
    %% of the build_span_tree_property/1 function
    rand:seed(default),

    PropTest = build_span_tree_property(),
    ?assertEqual(true,
                 proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_span_tree_property() ->
    ?FORALL(SpanTreeInputData,
            span_tree_generator:span_tree_input_data_gen(30, 6, 6),
            build_span_tree_property(SpanTreeInputData)).


build_span_tree_property(SpanTreeInputData) ->
    %% see comments in the build_span_tree_prop_test/1 testcase
    rand:seed(default),

    {#span{trace_id = TraceId, span_id = SpanId}, _} = ExpectedSpanTree =
        generate_span_tree_and_wait(SpanTreeInputData),

    ?assertEqual(
      {ok, ExpectedSpanTree},
      span_collector:build_span_tree(TraceId, SpanId, fun remove_end_time/1)),

    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree_and_wait(SpanTreeInputData) ->
    ExpectedSpanTree = generate_span_tree(SpanTreeInputData),
    {#span{trace_id = TraceId, span_id = SpanId}, _Children} = ExpectedSpanTree,
    ?assertMatch({ok, _}, span_collector:wait_for_span(TraceId, SpanId, 500)),
    ExpectedSpanTree.


generate_span_tree(SpanTreeInputData) ->
    span_tree_generator:generate_span_tree(SpanTreeInputData, fun get_span/1).


get_span(#{span_id := SpanId}) ->
    %% 'otel_span_table' is a table used by the otel_span_ets module.
    case ets:lookup(otel_span_table, SpanId) of
        [] ->
            error({failed_to_get_span, SpanId});
        [Span] -> Span
    end.


remove_end_time(Span) ->
    Span#span{end_time = undefined, is_recording = true}.
