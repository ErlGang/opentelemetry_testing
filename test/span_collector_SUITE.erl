-module(span_collector_SUITE).
-behavior(ct_suite).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

-export([build_span_tree_without_conversion_prop_test/1,
         build_span_tree_with_conversion_prop_test/1,
         ensure_started_test/1, build_span_tree_test/1,
         get_spans_by_name_test/1, wait_for_span_test/1,
         reset_test/1]).

-define(NUMBER_OF_REPETITIONS, 100).
-define(TABLE, '$spans_table').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [{group, basic},
     {group, proper}].

groups() ->
    [{basic, [],
      [ensure_started_test,
       reset_test,
       wait_for_span_test,
       get_spans_by_name_test,
       build_span_tree_test]},
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
init_per_group(_Group, Config) ->
    Config.

end_per_group(proper, Config) ->
    span_collector:reset(),
    Config;
end_per_group(_Group, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_test(_Config) ->
    span_tree({<<"some_span">>,[]}),
    SpanTable1 = ets:tab2list(?TABLE),
    ?assert(length(SpanTable1) > 0),
    span_collector:reset(),
    SpanTable2 = ets:tab2list(?TABLE),
    ?assertEqual(0, length(SpanTable2)).

wait_for_span_test(_Config) ->
    Timeout = 300,
    {#span{span_id = RootSpanId}, []} = span_tree({<<"some_span">>, []}),
    ?assertEqual(ok, span_collector:wait_for_span(RootSpanId, Timeout)),
    span_collector:reset(),
    {Time, Value} = timer:tc(span_collector, wait_for_span,
                             [RootSpanId, Timeout], millisecond),
    ?assertEqual({error, timeout}, Value),
    ?assert(Time > Timeout).

get_spans_by_name_test(_Config) ->
    SpanName = <<"some_span">>,
    span_collector:reset(),
    ?assertEqual({error,not_found}, span_collector:get_span_id_by_name(SpanName)),
    ?assertEqual([], span_collector:get_spans_by_name(SpanName)),
    {#span{span_id = RootSpanId} = ExpectedSpan1, []} = span_tree({SpanName, []}),
    ?assertEqual({ok, RootSpanId}, span_collector:get_span_id_by_name(SpanName)),
    [Span] = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(ExpectedSpan1,  remove_end_time(Span)),
    {#span{} = ExpectedSpan2, []} = span_tree({SpanName, []}),
    ?assertEqual({error,name_is_not_unique},
                 span_collector:get_span_id_by_name(SpanName)),
    Spans = span_collector:get_spans_by_name(SpanName),
    ?assertEqual(lists:sort([ExpectedSpan1, ExpectedSpan2]),
                 lists:sort([remove_end_time(S) || S <- Spans])).

build_span_tree_test(_Config) ->
    SpanTreeInput = {<<"some_span">>, [
                        {<<"another_span">>, [
                            {<<"yet_another_span">>,[]}
                        ]},
                        {<<"another_span">>, []}
                    ]},
    {RootSpanId, ExpectedSpanTree} = generate_span_tree(SpanTreeInput),
    SpanTree1 = span_collector:build_span_tree(RootSpanId),
    ?assertEqual(ExpectedSpanTree, remove_end_time_recursively(SpanTree1)),
    SpanTree2 = span_collector:build_span_tree(RootSpanId, fun remove_end_time/1),
    ?assertEqual(ExpectedSpanTree, SpanTree2),
    SpanTree3 = span_collector:build_span_tree(RootSpanId, fun(X) -> X end),
    ?assertEqual(SpanTree1, SpanTree3).

ensure_started_test(_Config) ->
    %% basic check if span_collector:ensure_started/0 interface is idempotent
    ?assertEqual(ok, span_collector:ensure_started()),
    ?assertEqual(ok, span_collector:ensure_started()).

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
    ?assertEqual(ok, span_collector:wait_for_span(RootSpanId, 500)),
    {RootSpanId, ExpectedSpanTree}.

span_tree({Span, Children}) ->
    ?with_span(Span,
               #{},
               fun(SpanCtx) ->
                   Branches = [span_tree(C) || C <- Children],
                   SpanId = SpanCtx#span_ctx.span_id,
                   {hd(ets:lookup(otel_span_table, SpanId)), Branches}
               end).

remove_end_time(Span) ->
    Span#span{end_time = undefined, is_recording = true}.

remove_end_time_recursively({Span, Children}) ->
    ProcessedChildren = [remove_end_time_recursively(Child) || Child <-  Children],
    ProcessedSpan = Span#span{end_time = undefined, is_recording = true},
    {ProcessedSpan, ProcessedChildren}.
