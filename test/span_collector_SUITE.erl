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
         span_id_is_not_unique_test/1,
         wait_for_span_test/1,
         reset_test/1,
         failing_to_start_test/1,
         unknown_call_test/1,
         unknown_cast_test/1,
         unknown_info_test/1]).

-define(NUMBER_OF_REPETITIONS, 50).
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
              build_span_tree_test,
              span_id_is_not_unique_test]},
     {errors_logging, [],
                      [failing_to_start_test,
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
    ct:log("'$spans_table' ETS size: ~p", [ets:info(?TABLE, size)]),
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
    SpanTreeInputData = {#{name => <<"some_span">>}, []},
    generate_span_tree_and_wait(SpanTreeInputData),
    ?assert(ets:info(?TABLE, size) > 0),
    span_collector:reset(),
    ?assertEqual(0, ets:info(?TABLE, size)).


wait_for_span_test(_Config) ->
    Timeout = 300,
    SpanTreeInputData = {#{name => <<"some_span">>}, []},
    {#span{span_id = SpanId} = ExpectedSpan, []} =
        generate_span_tree(SpanTreeInputData),
    Ret = span_collector:wait_for_span(SpanId, Timeout),
    ?assertMatch({ok, _}, Ret),
    {ok, Span} = Ret,
    ?assertEqual(ExpectedSpan, remove_end_time(Span)),
    %% subsequent calls to span_collector:wait_for_span/2 must
    %% immediately return the same span
    ?assertNoDelay(?assertEqual(Ret, span_collector:wait_for_span(SpanId, Timeout))),
    ?assertNoDelay(?assertEqual(Ret, span_collector:wait_for_span(SpanId, 0))),

    %% check that timeout works as expected
    span_collector:reset(),
    ?assertTimeout(?assertEqual({error, timeout},
                                span_collector:wait_for_span(SpanId, Timeout)),
                   Timeout),
    ?assertNoDelay(?assertEqual({error, timeout},
                                span_collector:wait_for_span(SpanId, 0))).


get_spans_by_name_test(_Config) ->
    SpanName = <<"some_span">>,
    SpanTreeInputData = {#{name => SpanName}, []},
    span_collector:reset(),
    ?assertEqual({error, not_found}, span_collector:get_span_id_by_name(SpanName)),
    ?assertEqual([], span_collector:get_spans_by_name(SpanName)),
    {RootSpanId, {ExpectedSpan1, _}} = generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual({ok, RootSpanId}, span_collector:get_span_id_by_name(SpanName)),
    [Span] = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(ExpectedSpan1, remove_end_time(Span)),
    {_, {ExpectedSpan2, _}} = generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual({error, name_is_not_unique},
                 span_collector:get_span_id_by_name(SpanName)),
    Spans = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(lists:sort([ExpectedSpan1, ExpectedSpan2]),
                 lists:sort([ remove_end_time(S) || S <- Spans ])).


build_span_tree_test(_Config) ->
    SpanTreeInputData = {#{name => <<"some_span">>},
                         [{#{name => <<"another_span">>},
                           [{#{name => <<"yet_another_span">>}, []}]},
                          {#{name => <<"another_span">>}, []}]},
    {RootSpanId, ExpectedSpanTree} = generate_span_tree_and_wait(SpanTreeInputData),
    ?assertEqual({ok, ExpectedSpanTree},
                 span_collector:build_span_tree(RootSpanId, fun remove_end_time/1)),
    %% span_collector:build_span_tree/2 returns the same tree if called twice.
    ?assertEqual({ok, ExpectedSpanTree},
                 span_collector:build_span_tree(RootSpanId, fun remove_end_time/1)),

    %% span tree is building successfully for non-root spans
    [ begin
          ct:log("SubTree = ~p", [SubTree]),
          ?assertEqual({ok, SubTree},
                       span_collector:build_span_tree(SpanId, fun remove_end_time/1))
      end || {#span{span_id = SpanId}, _} = SubTree <- element(2, ExpectedSpanTree) ],

    span_collector:reset(),
    ?assertEqual({error, not_found},
                 span_collector:build_span_tree(RootSpanId, fun remove_end_time/1)).


ensure_started_test(_Config) ->
    %% basic check if span_collector:ensure_started/0 interface is idempotent
    ?assertEqual(ok, span_collector:ensure_started()),
    ?assertEqual(ok, span_collector:ensure_started()).


span_id_is_not_unique_test(_Config) ->
    {RootSpanId, {Span, []}} =
        generate_span_tree_and_wait({#{name => <<"some_span">>}, []}),

    %% since Span has no end time, it is different from the record stored
    %% in span_collector's ETS. because the ETS type is bag, span_collector
    %% will successfully store the Span record to ETS.
    erlang:send(?GEN_SERVER_NAME, {span, Span}),
    timer:sleep(100),

    ?assertEqual({error, span_id_is_not_unique},
                 span_collector:build_span_tree(RootSpanId, fun remove_end_time/1)),
    ?assertEqual({error, span_id_is_not_unique},
                 span_collector:wait_for_span(RootSpanId, 0)).


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
    PropTest = build_span_tree_property(),
    ?assertEqual(true, proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_span_tree_property() ->
    ?FORALL(SpanTreeInputData,
            span_tree_generator:span_tree_input_data_gen(30, 6, 6),
            begin
                ct:log("SpanTreeInputData = ~p", [SpanTreeInputData]),
                {RootSpanId, ExpectedSpanTree} =
                    generate_span_tree_and_wait(SpanTreeInputData),
                ?assertEqual(
                  {ok, ExpectedSpanTree},
                  span_collector:build_span_tree(RootSpanId, fun remove_end_time/1)),
                true
            end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree_and_wait(SpanTreeInputData) ->
    ExpectedSpanTree = generate_span_tree(SpanTreeInputData),
    {#span{span_id = RootSpanId}, _Children} = ExpectedSpanTree,
    ?assertMatch({ok, _}, span_collector:wait_for_span(RootSpanId, 500)),
    {RootSpanId, ExpectedSpanTree}.


generate_span_tree(SpanTreeInputData) ->
    span_tree_generator:generate_span_tree(SpanTreeInputData, fun get_span/1).


get_span(#{span_id := SpanId}) ->
    get_span(SpanId, 300).


get_span(SpanId, Timeout) when Timeout > 0 ->
    %% 'otel_span_table' is a table used by the otel_span_ets module.
    case ets:lookup(otel_span_table, SpanId) of
        [] ->
            ct:log("failed to get span ~p", [SpanId]),
            timer:sleep(50),
            get_span(SpanId, Timeout - 50);
        [Span] -> Span
    end;
get_span(SpanId, _Timeout) ->
    error({failed_to_get_span, SpanId}).


remove_end_time(Span) ->
    Span#span{end_time = undefined, is_recording = true}.
