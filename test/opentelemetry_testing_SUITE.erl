-module(opentelemetry_testing_SUITE).
-behavior(ct_suite).

-include_lib("stdlib/include/assert.hrl").

%% otel_tracer.hrl defines tracing marcos (e.g. ?with_span())
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
%% opentelemetry.hrl contains #span_ctx{} record declaration
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

-export([ensure_started_test/1,
         get_spans_by_name_test/1,
         wait_for_span_test/1,
         build_span_tree_test/1]).

-define(MILLISECONDS(Action),
        element(1, timer:tc(fun() -> Action end, millisecond))).
-define(assertNoDelay(Action), ?assert(?MILLISECONDS(Action) < 2)).
-define(assertTimeout(Action, Timeout), ?assert(?MILLISECONDS(Action) >= Timeout)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ct_suite callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all() ->
    [ensure_started_test,
     {group, match_test}].


groups() ->
    [{match_test, [{repeat_until_any_fail, 50}],
                  [wait_for_span_test,
                   get_spans_by_name_test,
                   {group, build_span_tree}]},
     {build_span_tree, [parallel],
                       [build_span_tree_test,
                        build_span_tree_test]}].


init_per_suite(Config) ->
    opentelemetry_testing:ensure_started(),
    Config.


end_per_suite(Config) ->
    Config.


init_per_group(match_test, Config) ->
    opentelemetry_testing:reset(),
    [{span_trees_data, generate_branches(3)} | Config];
init_per_group(_, Config) ->
    Config.


end_per_group(_, Config) ->
    opentelemetry_testing:reset(),
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
    %% generate one tree with only one span
    [SpanTreeData] = generate_branches(1),
    Timeout = 300,
    {{#{span_id := SpanId} = SpanPattern, []}, _} =
        generate_span_tree(SpanTreeData, [], fun(X) -> X end),
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
    [SpanTreeData] = generate_branches(1),

    {{#{name := Name, span_id := SpanId1} = SpanPattern1, []}, _} =
        generate_span_tree(SpanTreeData, [], fun(X) -> X end),
    ?assertMatch({ok, #{}}, opentelemetry_testing:wait_for_span(SpanId1, 300)),
    ?assertEqual({ok, SpanId1}, opentelemetry_testing:get_span_id_by_name(Name)),
    Spans1 = opentelemetry_testing:get_spans_by_name(Name),
    %% ensure that spans are converted by opentelemetry_testing:get_spans_by_name/1
    ?assert(opentelemetry_testing:match(Spans1, [SpanPattern1])),

    {{#{name := Name, span_id := SpanId2} = SpanPattern2, []}, _} =
        generate_span_tree(SpanTreeData, [], fun(X) -> X end),
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


build_span_tree_test(Config) ->
    SpanTreesData = proplists:get_value(span_trees_data, Config),
    {TreePatterns, _Links} = generate_span_trees(SpanTreesData, fun randomize_pattern/1),
    [ begin
          {#{span_id := RootSpanId}, _} = Pattern,
          ?assertMatch({ok, #{}}, opentelemetry_testing:wait_for_span(RootSpanId, 500)),
          {ok, SpanTree} = opentelemetry_testing:build_span_tree(RootSpanId),
          ct:log("SpanTree = ~p", [SpanTree]),
          ct:log("Pattern = ~p", [Pattern]),
          ?assert(opentelemetry_testing:match(SpanTree, Pattern))
      end || Pattern <- TreePatterns ],
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


randomize_pattern(#{
                    links := Links,
                    events := Events,
                    attributes := Attributes
                   } = SpanPattern) ->
    SpanPattern#{
      %% randomize the Span pattern a bit
      trace_id => '_',
      links => pick_random_items(Links, 2, false),
      events => pick_random_items(Events, 2, false),
      attributes => pick_random_attributes(Attributes)
     }.


generate_branches(N) ->
    Attributes = #{ <<"attr", I>> => <<"value", I>> || I <- lists:seq($1, $9) },
    Events = [ #{
                 name => <<"event", I>>,
                 attributes => pick_random_attributes(Attributes)
                } || I <- lists:seq($1, $9) ],
    Statuses = [#{code => ?OTEL_STATUS_ERROR, message => <<"some error">>},
                %% message is ignored for ?OTEL_STATUS_OK
                #{code => ?OTEL_STATUS_OK, message => <<"">>},
                undefined],
    Spans = [ #{
                name => <<"span", (integer_to_binary(I))/binary>>,
                attributes => pick_random_attributes(Attributes),
                events => pick_random_events(Events),
                status => pick_any_status(Statuses)
               } || I <- lists:seq(1, N * 10) ],
    generate_branches(Spans, N).


generate_branches(Spans, N) ->
    [ {Span, generate_branches(Spans, N - 1)}
      || Span <- pick_random_items(Spans, N, false) ].


generate_span_trees(SpanTrees, ConvertPatternFn) ->
    FoldFn =
        fun(SpanTree, {SpanTreePatterns, Links}) ->
                {NewSpanTreePattern, NewLinks} =
                    generate_span_tree(SpanTree, Links, ConvertPatternFn),
                {[NewSpanTreePattern | SpanTreePatterns],
                 lists:append(NewLinks, Links)}
        end,
    lists:foldl(FoldFn, {[], []}, SpanTrees).


generate_span_tree({#{name := Name} = Span, Children}, Links, ConvertPatternFn) ->
    SpanLinks = pick_random_links(Links),
    Attributes = maps:get(attributes, Span, #{}),
    Events = maps:get(events, Span, []),
    Status = maps:get(status, Span, undefined),
    ?with_span(
      Name,
      #{links => SpanLinks, attributes => Attributes},
      fun(SpanCtx) ->
              BranchesAndNewLinks = [ generate_span_tree(C, Links, ConvertPatternFn)
                                      || C <- Children ],
              Branches = [ Branch || {Branch, _} <- BranchesAndNewLinks ],
              NewLinks = lists:append([ Link || {_, Link} <- BranchesAndNewLinks ]),
              SpanId = SpanCtx#span_ctx.span_id,
              TraceId = SpanCtx#span_ctx.trace_id,
              SpanLinksPattern = [ link_to_pattern(L) || L <- SpanLinks ],
              CompleteSpanPattern = Span#{
                                      span_id => SpanId,
                                      trace_id => TraceId,
                                      links => SpanLinksPattern,
                                      events => Events,
                                      attributes => Attributes
                                     },
              SpanPattern = ConvertPatternFn(CompleteSpanPattern),
              ?add_events(Events),
              set_status(Status),
              NewLink = create_new_link(SpanCtx, Attributes),
              {{SpanPattern, Branches}, [NewLink | NewLinks]}
      end).


set_status(undefined) ->
    ok;
set_status(#{code := Code, message := Message}) ->
    ?set_status(Code, Message).


create_new_link(SpanCtx, Attributes) ->
    RandomAttributes = pick_random_attributes(Attributes),
    opentelemetry:link(SpanCtx, RandomAttributes).


link_to_pattern(Link) ->
    %% note that the opentelemetry:link() type returned by opentelemetry:link/2 is a map.
    %% While the #span{} record stores links as #link{} records (see record definitions
    %% in the otel_span.hrl header and the definition of the otel_events:t() type), after
    %% converting the #span{} record using the opentelemetry_testing:convert/1 function,
    %% the format of the links in the resulting map is similar to the opentelemetry:link()
    %% type (in fact, it is identical).
    maps:with([trace_id, span_id, attributes], Link).


pick_random_links(Links) ->
    pick_random_items(Links, 4, true).


pick_random_events(Events) ->
    pick_random_items(Events, 4, true).


pick_random_attributes(Attributes) ->
    Keys = maps:keys(Attributes),
    RandomKeys = pick_random_items(Keys, 4, true),
    maps:with(RandomKeys, Attributes).


pick_any_status(Statuses) ->
    %% pick only one status
    hd(pick_random_items(Statuses, 1, false)).


pick_random_items([], _MaxN, _AllowEmptyResult) ->
    [];
pick_random_items(_, 0, _AllowEmptyResult) ->
    [];
pick_random_items(List, MaxN, true) ->
    Length = length(List),
    case (rand:uniform(min(Length, MaxN) + 1) - 1) of
        0 -> [];
        N ->
            RandomIndexes = lists:uniq([ rand:uniform(Length) || _ <- lists:seq(1, N) ]),
            [ lists:nth(Index, List) || Index <- RandomIndexes ]
    end;
pick_random_items(List, MaxN, false) ->
    Length = length(List),
    N = rand:uniform(min(Length, MaxN)),
    RandomIndexes = lists:uniq([ rand:uniform(Length) || _ <- lists:seq(1, N) ]),
    [ lists:nth(Index, List) || Index <- RandomIndexes ].
