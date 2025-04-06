-module(opentelemetry_testing_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

-export([ensure_started_test/1,
         get_spans_by_name_test/1,
         wait_for_span_test/1,
         build_span_tree_prop_test/1]).

-define(MILLISECONDS(Action),
        element(1, timer:tc(fun() -> Action end, millisecond))).

-define(assertNoDelay(Action), ?assert(?MILLISECONDS(Action) < 2)).
-define(assertTimeout(Action, Timeout), ?assert(?MILLISECONDS(Action) >= Timeout)).

-define(NUMBER_OF_REPETITIONS, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [ensure_started_test,
     wait_for_span_test,
     get_spans_by_name_test,
     {group, proper}].


groups() ->
    [{proper, [parallel],
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
    opentelemetry_testing:reset(),
    Config;
init_per_group(_, Config) ->
    Config.


end_per_group(proper, Config) ->
    ct:log("'$spans_table' ETS size: ~p", [ets:info('$spans_table', size)]),
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
    %% generate a tree with only one root span
    SpanTreeInputData = single_span_tree(),
    Timeout = 300,
    {#{span_id := SpanId} = SpanPattern, []} =
        generate_span_tree(SpanTreeInputData),
    Ret = opentelemetry_testing:wait_for_span(SpanId, Timeout),
    ?assertMatch({ok, #{span_id := SpanId}}, Ret),
    {ok, Span} = Ret,
    %% ensure that span is converted by opentelemetry_testing:wait_for_span/1
    ?assert(opentelemetry_testing:match(Span, SpanPattern)),
    %% ensure that opentelemetry_testing:wait_for_span/1 returns immediately
    %% if Span is already reported
    ?assertNoDelay(
      ?assertEqual(Ret, opentelemetry_testing:wait_for_span(SpanId, Timeout))),

    opentelemetry_testing:reset(),
    %% ensure that opentelemetry_testing:wait_for_span/1 returns after a Timeout
    %% if there's no span with such SpanId
    ?assertTimeout(?assertEqual({error, timeout},
                                opentelemetry_testing:wait_for_span(SpanId, Timeout)),
                   Timeout).


get_spans_by_name_test(_Config) ->
    opentelemetry_testing:reset(),
    %% generate one tree with only one span
    SpanTreeInputData = single_span_tree(),

    {#{name := Name, span_id := SpanId1} = SpanPattern1, []} =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}}, opentelemetry_testing:wait_for_span(SpanId1, 300)),
    ?assertEqual({ok, SpanId1}, opentelemetry_testing:get_span_id_by_name(Name)),
    Spans1 = opentelemetry_testing:get_spans_by_name(Name),
    %% ensure that spans are converted by opentelemetry_testing:get_spans_by_name/1
    ?assert(opentelemetry_testing:match(Spans1, [SpanPattern1])),

    {#{name := Name, span_id := SpanId2} = SpanPattern2, []} =
        generate_span_tree(SpanTreeInputData),
    ?assertMatch({ok, #{}}, opentelemetry_testing:wait_for_span(SpanId2, 300)),
    ?assertEqual({error, name_is_not_unique},
                 opentelemetry_testing:get_span_id_by_name(Name)),
    Spans2 = opentelemetry_testing:get_spans_by_name(Name),
    %% ensure that spans are converted by opentelemetry_testing:get_spans_by_name/1
    ?assert(opentelemetry_testing:match(Spans2, [SpanPattern1, SpanPattern2])),

    opentelemetry_testing:reset(),
    ?assertEqual({error, not_found},
                 opentelemetry_testing:get_span_id_by_name(Name)),
    ?assertEqual([],
                 opentelemetry_testing:get_spans_by_name(Name)).


build_span_tree_prop_test(_Config) ->
    PropTest = build_span_tree_property(),
    ?assertEqual(true,
                 proper:quickcheck(PropTest,
                                   [?NUMBER_OF_REPETITIONS,
                                    % {on_output, fun ct:pal/2},
                                    noshrink])),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


build_span_tree_property() ->
    ?FORALL(SpanTreesInputData,
            vector(4, span_tree_generator:span_tree_input_data_gen(30, 4, 4)),
            build_span_tree_property(SpanTreesInputData)).


build_span_tree_property(SpanTreesInputData) ->
    TreePatterns =
        span_tree_generator:generate_linked_span_trees(SpanTreesInputData,
                                                       fun randomize_pattern/1),
    [ begin
          {#{span_id := RootSpanId}, _} = Pattern,
          ?assertMatch({ok, #{}},
                       opentelemetry_testing:wait_for_span(RootSpanId, 500)),
          {ok, SpanTree} =
              opentelemetry_testing:build_span_tree(RootSpanId),
          ct:log("SpanTree = ~p", [SpanTree]),
          ct:log("Pattern = ~p", [Pattern]),
          ?assert(opentelemetry_testing:match(SpanTree, Pattern))
      end || Pattern <- TreePatterns ],
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree(SpanTreeInputData) ->
    span_tree_generator:generate_span_tree(SpanTreeInputData, fun(X) -> X end).


randomize_pattern(Pattern) ->
    span_tree_generator:randomize_span_pattern(Pattern).


single_span_tree() ->
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
     []}.
