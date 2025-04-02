-module(span_collector_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-include("test_logger_handler.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

-export([build_span_tree_without_conversion_prop_test/1,
         build_span_tree_with_conversion_prop_test/1,
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

-define(NUMBER_OF_REPETITIONS, 100).
-define(TABLE,                 '$spans_table').
-define(GEN_SERVER_NAME,       span_collector).

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
              [build_span_tree_without_conversion_prop_test,
               build_span_tree_with_conversion_prop_test]}].


init_per_suite(Config) ->
    opentelemetry_testing:ensure_started(),
    Config.


end_per_suite(Config) ->
    Config.


init_per_group(proper, Config) ->
    span_collector:reset(),
    Config;
init_per_group(errors_logging, Config) ->
    test_logger_handler:add_handler(),
    span_collector:reset(),
    Config;
init_per_group(_Group, Config) ->
    Config.


end_per_group(proper, Config) ->
    span_collector:reset(),
    Config;
end_per_group(errors_logging, Config) ->
    test_logger_handler:remove_handler(),
    span_collector:ensure_started(),
    Config;
end_per_group(_Group, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


reset_test(_Config) ->
    span_tree({<<"some_span">>, []}),
    SpanTable1 = ets:tab2list(?TABLE),
    ?assert(length(SpanTable1) > 0),
    span_collector:reset(),
    SpanTable2 = ets:tab2list(?TABLE),
    ?assertEqual(0, length(SpanTable2)).


wait_for_span_test(_Config) ->
    Timeout = 300,
    RetryAfter = 150,
    {#span{span_id = SpanId} = ExpectedSpan, []} = span_tree({<<"some_span">>, []}),
    Ret = span_collector:wait_for_span(SpanId, Timeout),
    ?assertMatch({ok, _}, Ret),
    {ok, Span} = Ret,
    ?assertEqual(ExpectedSpan, remove_end_time(Span)),
    %% subsequent calls to span_collector:wait_for_span/2 must
    %% return the same span
    ?assertEqual(Ret, span_collector:wait_for_span(SpanId, 0)),
    ?assertEqual(Ret, span_collector:wait_for_span(SpanId, 0)),

    %% check that timeout works as expected
    span_collector:reset(),
    {Time1, Value1} =
        timer:tc(span_collector, wait_for_span, [SpanId, Timeout], millisecond),
    ?assertEqual({error, timeout}, Value1),
    ?assert(Time1 >= Timeout),
    {Time2, Value2} =
        timer:tc(span_collector, wait_for_span, [SpanId, 0], millisecond),
    ?assertEqual({error, timeout}, Value2),
    ?assert(Time2 < RetryAfter).


get_spans_by_name_test(_Config) ->
    SpanName = <<"some_span">>,
    span_collector:reset(),
    ?assertEqual({error, not_found}, span_collector:get_span_id_by_name(SpanName)),
    ?assertEqual([], span_collector:get_spans_by_name(SpanName)),
    {#span{span_id = RootSpanId} = ExpectedSpan1, []} = span_tree({SpanName, []}),
    ?assertEqual({ok, RootSpanId}, span_collector:get_span_id_by_name(SpanName)),
    [Span] = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(ExpectedSpan1, remove_end_time(Span)),
    {#span{} = ExpectedSpan2, []} = span_tree({SpanName, []}),
    ?assertEqual({error, name_is_not_unique},
                 span_collector:get_span_id_by_name(SpanName)),
    Spans = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(lists:sort([ExpectedSpan1, ExpectedSpan2]),
                 lists:sort([ remove_end_time(S) || S <- Spans ])).


build_span_tree_test(_Config) ->
    SpanTreeInput = {<<"some_span">>,
                     [{<<"another_span">>,
                       [{<<"yet_another_span">>, []}]},
                      {<<"another_span">>, []}]},
    {RootSpanId, ExpectedSpanTree} = generate_span_tree(SpanTreeInput),
    SpanTree1 = span_collector:build_span_tree(RootSpanId),
    ?assertEqual(ExpectedSpanTree, remove_end_time_recursively(SpanTree1)),
    SpanTree2 = span_collector:build_span_tree(RootSpanId, fun remove_end_time/1),
    ?assertEqual(ExpectedSpanTree, SpanTree2),
    SpanTree3 = span_collector:build_span_tree(RootSpanId, fun(X) -> X end),
    ?assertEqual(SpanTree1, SpanTree3),

    %% span tree is building for non-root spans
    {_, [{#span{span_id = SubSpanId}, _} = SubTree | _]} = ExpectedSpanTree,
    ct:log("SubTree = ~p", [SubTree]),
    ?assertEqual(SubTree,
                 span_collector:build_span_tree(SubSpanId,
                                                fun remove_end_time/1)),

    span_collector:reset(),
    ?assertEqual({error, not_found}, span_collector:build_span_tree(RootSpanId)).


ensure_started_test(_Config) ->
    %% basic check if span_collector:ensure_started/0 interface is idempotent
    ?assertEqual(ok, span_collector:ensure_started()),
    ?assertEqual(ok, span_collector:ensure_started()).


span_id_is_not_unique_test(_Config) ->
    SpanName = <<"some_span">>,
    {#span{span_id = RootSpanId} = Span, []} = span_tree({SpanName, []}),
    %% since Span has no end time, it is different from the record stored
    %% in span_collector's ETS. because the ETS type is bag, span_collector
    %% will successfully store the Span record to ETS.
    erlang:send(?GEN_SERVER_NAME, {span, Span}),
    timer:sleep(100),
    ?assertEqual({error, span_id_is_not_unique},
                 span_collector:build_span_tree(RootSpanId)),
    ?assertEqual({error, span_id_is_not_unique},
                 span_collector:wait_for_span(RootSpanId, 0)).


failing_to_start_test(_Config) ->
    test_logger_handler:set_pid(),
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
    ?assertLogMessage("failed to start span_collector:",
                      error,
                      {span_collector, ensure_started, 0}).


unknown_call_test(_Config) ->
    test_logger_handler:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertEqual(not_implemented, gen_server:call(?GEN_SERVER_NAME, some_call)),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogMessage("unexpected call request:", error, _).


unknown_cast_test(_Config) ->
    test_logger_handler:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    gen_server:cast(?GEN_SERVER_NAME, some_cast),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogMessage("unexpected cast request:", error, _).


unknown_info_test(_Config) ->
    test_logger_handler:set_pid(),
    span_collector:ensure_started(),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    erlang:send(?GEN_SERVER_NAME, some_call),
    timer:sleep(100),
    ?assertNotEqual(undefined, whereis(?GEN_SERVER_NAME)),
    ?assertLogMessage("unexpected info message:", error, _).


build_span_tree_without_conversion_prop_test(_Config) ->
    PropTest = build_span_tree_without_conversion_property(),
    ?assertEqual(true, proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


build_span_tree_with_conversion_prop_test(_Config) ->
    PropTest = build_span_tree_with_conversion_property(),
    ?assertEqual(true, proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_span_tree_without_conversion_property() ->
    ?FORALL(GeneratedSpanTree,
            span_tree_gen(3),
            begin
                {RootSpanId, ExpectedSpanTree} = generate_span_tree(GeneratedSpanTree),
                SpanTree = span_collector:build_span_tree(RootSpanId),
                ?assertEqual(ExpectedSpanTree, remove_end_time_recursively(SpanTree)),
                true
            end).


build_span_tree_with_conversion_property() ->
    ?FORALL(GeneratedSpanTree,
            span_tree_gen(3),
            begin
                {RootSpanId, ExpectedSpanTree} = generate_span_tree(GeneratedSpanTree),
                SpanTree = span_collector:build_span_tree(RootSpanId, fun remove_end_time/1),
                ?assertEqual(ExpectedSpanTree, SpanTree),
                true
            end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_tree_gen(MaxDepth) ->
    {span_gen(), span_subtree_gen(MaxDepth)}.


span_subtree_gen(MaxDepth) ->
    BasicWeight = 1000,
    DepthWeight = max(MaxDepth * BasicWeight, 0),
    ?LAZY(frequency([{DepthWeight, small_list_gen(span_tree_gen(MaxDepth - 1), 5)},
                     {BasicWeight, []}])).


small_list_gen(Type, Limit) ->
    ?SUCHTHATMAYBE(List, non_empty(list(Type)), (length(List) =< Limit)).


span_gen() ->
    oneof([<<"some_span">>, <<"another_span">>, <<"yet_another_span">>]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree(GeneratedSpanTree) ->
    ExpectedSpanTree = span_tree(GeneratedSpanTree),
    {#span{span_id = RootSpanId}, _Children} = ExpectedSpanTree,
    ?assertMatch({ok, _}, span_collector:wait_for_span(RootSpanId, 500)),
    {RootSpanId, ExpectedSpanTree}.


span_tree({Span, Children}) ->
    ?with_span(Span,
               #{},
               fun(SpanCtx) ->
                       Branches = [ span_tree(C) || C <- Children ],
                       SpanId = SpanCtx#span_ctx.span_id,
                       timer:sleep(100),
                       {get_span(SpanId, 300), Branches}
               end).


get_span(SpanId, Timeout) when Timeout > 0 ->
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


remove_end_time_recursively({Span, Children}) ->
    ProcessedChildren = [ remove_end_time_recursively(Child) || Child <- Children ],
    ProcessedSpan = Span#span{end_time = undefined, is_recording = true},
    {ProcessedSpan, ProcessedChildren}.
