-module(opentelemetry_testing_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

%% otel_span.hrl contains #span{} record declaration
-include_lib("opentelemetry/include/otel_span.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

-export([ensure_started_test/1,
         get_spans_by_name_test/1,
         wait_for_span_test/1,
         build_span_tree_test/1,
         match_test/1,
         convert_span_test/1,
         build_span_tree_prop_test/1]).

-define(MILLISECONDS(Action),
        element(1, timer:tc(fun() -> Action end, millisecond))).

-define(assertNoDelay(Action), ?assert(?MILLISECONDS(Action) < 2)).
-define(assertTimeout(Action, Timeout), ?assert(?MILLISECONDS(Action) >= Timeout)).

-define(NUMBER_OF_REPETITIONS, 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [ensure_started_test,
     wait_for_span_test,
     get_spans_by_name_test,
     build_span_tree_test,
     convert_span_test,
     match_test,
     {group, proper}].


groups() ->
    [{proper, [parallel],
              [build_span_tree_prop_test,
               build_span_tree_prop_test,
               build_span_tree_prop_test,
               build_span_tree_prop_test]}].


init_per_suite(Config) ->
    ct:log("loaded_applications = ~p", [application:loaded_applications()]),
    ct:log("which_applications = ~p", [application:which_applications()]),

    opentelemetry_testing:ensure_started(),
    Config.


end_per_suite(Config) ->
    Config.


init_per_group(proper, Config) ->
    opentelemetry_testing:reset(),
    Config;
init_per_group(_, Config) ->
    Config.


end_per_group(proper, Config) ->
    opentelemetry_testing:reset(),
    Config;
end_per_group(_, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ensure_started_test(_Config) ->
    %% basic check if opentelemetry_testing:ensure_started/0
    %% interface is idempotent
    ?assertEqual(ok, opentelemetry_testing:ensure_started()),
    ?assertEqual(ok, opentelemetry_testing:ensure_started()).


wait_for_span_test(_Config) ->
    SpanTreeInputData = span_tree_input_data([{#{name => child1}, []}]),
    Timeout = 300,
    {#{trace_id := TraceId, span_id := SpanId} = SpanPattern, [_]} =
        generate_span_tree(SpanTreeInputData),
    Ret = opentelemetry_testing:wait_for_span(TraceId, SpanId, Timeout),
    ?assertMatch({ok, #{trace_id := TraceId, span_id := SpanId}}, Ret),
    {ok, Span} = Ret,

    %% ensure that span is converted by opentelemetry_testing:wait_for_span/3
    ?assert(opentelemetry_testing:match(Span, SpanPattern)),

    %% ensure that opentelemetry_testing:wait_for_span/3 returns immediately
    %% if Span is already reported
    ?assertNoDelay(
      ?assertEqual(Ret,
                   opentelemetry_testing:wait_for_span(TraceId, SpanId, Timeout))),

    %% check that TraceId is optional
    ?assertNoDelay(
      ?assertEqual(Ret,
                   opentelemetry_testing:wait_for_span('_', SpanId, Timeout))),

    %% span_is_not_unique error test, there are 2 spans with such TraceId.
    ?assertNoDelay(
      ?assertEqual({error, span_is_not_unique},
                   opentelemetry_testing:wait_for_span(TraceId, '_', Timeout))),
    ?assertNoDelay(
      ?assertEqual({error, span_is_not_unique},
                   opentelemetry_testing:wait_for_span('_', '_', Timeout))),

    opentelemetry_testing:reset(),
    %% ensure that opentelemetry_testing:wait_for_span/3 returns after a Timeout
    %% if there's no span with such SpanId
    ?assertTimeout(?assertEqual(
                     {error, timeout},
                     opentelemetry_testing:wait_for_span(TraceId, SpanId, Timeout)),
                   Timeout).


get_spans_by_name_test(_Config) ->
    opentelemetry_testing:reset(),
    SpanTreeInputData = span_tree_input_data([]),

    {#{name := Name, trace_id := TraceId1, span_id := SpanId1} = SpanPattern1, []} =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}},
                 opentelemetry_testing:wait_for_span(TraceId1, SpanId1, 300)),
    ?assertEqual({ok, {TraceId1, SpanId1}},
                 opentelemetry_testing:get_span_ids_by_name(Name)),
    Spans1 = opentelemetry_testing:get_spans_by_name(Name),
    %% ensure that spans are converted by opentelemetry_testing:get_spans_by_name/1
    ?assert(opentelemetry_testing:match(Spans1, [SpanPattern1])),

    {#{name := Name, trace_id := TraceId2, span_id := SpanId2} = SpanPattern2, []} =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}},
                 opentelemetry_testing:wait_for_span(TraceId2, SpanId2, 300)),
    ?assertEqual({error, span_is_not_unique},
                 opentelemetry_testing:get_span_ids_by_name(Name)),
    Spans2 = opentelemetry_testing:get_spans_by_name(Name),
    %% ensure that spans are converted by opentelemetry_testing:get_spans_by_name/1
    ?assert(opentelemetry_testing:match(Spans2, [SpanPattern1, SpanPattern2])),

    opentelemetry_testing:reset(),
    ?assertEqual({error, not_found},
                 opentelemetry_testing:get_span_ids_by_name(Name)),
    ?assertEqual([],
                 opentelemetry_testing:get_spans_by_name(Name)).


build_span_tree_test(_Config) ->
    SpanTreeInputData = span_tree_input_data([{#{name => child1}, []}]),
    {#{trace_id := TraceId, span_id := SpanId}, [_]} = SpanTreePattern =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}},
                 opentelemetry_testing:wait_for_span(TraceId, SpanId, 500)),
    Ret = opentelemetry_testing:build_span_tree(TraceId, SpanId),
    ?assertMatch({ok, {#{trace_id := TraceId, span_id := SpanId}, [_]}}, Ret),
    {ok, SpanTree} = Ret,

    %% ensure that span tree is converted by opentelemetry_testing:build_span_tree/2
    ?assert(opentelemetry_testing:match(SpanTree, SpanTreePattern)),

    %% ensure that opentelemetry_testing:build_span_tree/2 returns the same tree
    %% if called twice
    ?assertEqual(Ret,
                 opentelemetry_testing:build_span_tree(TraceId, SpanId)),

    %% check that TraceId is optional
    ?assertEqual(Ret,
                 opentelemetry_testing:build_span_tree('_', SpanId)),

    %% span_is_not_unique error test, there are 2 spans with such TraceId.
    ?assertEqual({error, span_is_not_unique},
                 opentelemetry_testing:build_span_tree(TraceId, '_')),
    ?assertEqual({error, span_is_not_unique},
                 opentelemetry_testing:build_span_tree('_', '_')),

    opentelemetry_testing:reset(),
    %% not_found error test
    ?assertEqual(
      {error, not_found},
      opentelemetry_testing:build_span_tree(TraceId, SpanId)).


convert_span_test(_Config) ->
    %% ensure that convert_span function doesn't crash on unexpected input
    ?assertEqual([], opentelemetry_testing:convert_span([])),
    ?assertEqual(undefined, opentelemetry_testing:convert_span(undefined)),
    ?assertEqual({}, opentelemetry_testing:convert_span({})),
    ?assertEqual(#{}, opentelemetry_testing:convert_span(#{})),
    ?assertMatch(#{attributes := undefined, events := undefined, links := undefined},
                 opentelemetry_testing:convert_span(#span{})).


match_test(_Config) ->
    SpanTreeInputData = span_tree_input_data([{#{name => child1}, []}]),
    {#{span_id := SpanId} = RootSpanPattern, [_]} =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}},
                 opentelemetry_testing:wait_for_span('_', SpanId, 500)),
    Ret = opentelemetry_testing:build_span_tree('_', SpanId),
    ?assertMatch({ok, {#{span_id := SpanId}, [_]}}, Ret),
    {ok, {_, Children} = SpanTree} = Ret,

    %% testing only negative match in this testcase
    SpanTreePattern = {RootSpanPattern, []},
    ?assertEqual(
      {false,
       [match_failure(SpanTree, SpanTreePattern, match_tuple, #{}),
        match_failure(Children, [], match_tuple_element, #{position => 2}),
        match_failure(Children, [], match_list, #{reason => list_is_not_empty})]},
      opentelemetry_testing:match(SpanTree, SpanTreePattern)).


build_span_tree_prop_test(_Config) ->
    %% see comments in the span_collector_SUITE:build_span_tree_prop_test/1 testcase
    rand:seed(default),

    PropTest = build_span_tree_property(),
    ?assert(proper:quickcheck(PropTest, [?NUMBER_OF_REPETITIONS, noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_span_tree_property() ->
    ?FORALL(SpanTreesInputData,
            vector(4, span_tree_generator:span_tree_input_data_gen(4, 4)),
            build_span_tree_property(SpanTreesInputData)).


build_span_tree_property(SpanTreesInputData) ->
    %% see comments in the span_collector_SUITE:build_span_tree_prop_test/1 testcase
    rand:seed(default),

    TreePatterns =
        span_tree_builder:generate_linked_span_trees(SpanTreesInputData,
                                                     fun randomize_pattern/1),
    [ begin
          {#{trace_id := TraceId, span_id := RootSpanId}, _} = Pattern,
          ?assertMatch(
            {ok, #{}},
            opentelemetry_testing:wait_for_span(TraceId, RootSpanId, 500)),
          {ok, SpanTree} =
              opentelemetry_testing:build_span_tree(TraceId, RootSpanId),
          ?assert(opentelemetry_testing:match(SpanTree, Pattern))
      end || Pattern <- TreePatterns ],
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree(SpanTreeInputData) ->
    span_tree_builder:generate_span_tree(SpanTreeInputData, fun(X) -> X end).


randomize_pattern(Pattern) ->
    span_tree_builder:randomize_span_pattern(Pattern).


match_failure(Value, Pattern, Matcher, ExtraFields) ->
    maps:merge(
      #{
        reason => match_failed,
        value => Value,
        pattern => Pattern,
        matcher => Matcher
       },
      ExtraFields).


span_tree_input_data(Children) ->
    {#{
       name => root_span,
       attributes => #{
                       attr1 => value1,
                       attr2 => value2
                      },
       events => [#{
                    name => event1,
                    attributes => #{
                                    attr3 => value3,
                                    attr4 => value4
                                   }
                   }]
      },
     Children}.
