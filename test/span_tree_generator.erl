-module(span_tree_generator).

%% PropEr generators
-export([span_tree_input_data_gen/3]).

%% API
-export([randomize_span_pattern/1,
         generate_linked_span_trees/2,
         generate_span_tree/2]).

%% otel_tracer.hrl defines tracing marcos (e.g. ?with_span())
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
%% opentelemetry.hrl contains #span_ctx{} record declaration
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropEr generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


span_tree_input_data_gen(NumberOfSpans, MaxBranchWidth, MaxBranchDepth) ->
    ?LET({Spans, BranchDepth},
         {vector(NumberOfSpans, span_input_data_gen()), integer(0, MaxBranchDepth)},
         generate_span_tree_input_data(Spans, MaxBranchWidth, BranchDepth)).


span_input_data_gen() ->
    ?LET({Name, Attributes, Events, Status},
         {name_gen(), attributes_gen(), events_gen(), status_gen()},
         #{name => Name, attributes => Attributes, events => Events, status => Status}).


name_gen() ->
    non_empty(binary()).


attributes_gen() ->
    map(oneof([atom(), name_gen()]),
        binary()).


events_gen() ->
    ?LET(Length, integer(0, 5), vector(Length, event_gen())).


event_gen() ->
    ?LET({Name, Attributes},
         {name_gen(), attributes_gen()},
         #{name => Name, attributes => Attributes}).


status_gen() ->
    oneof([#{code => ?OTEL_STATUS_ERROR, message => <<"any error">>},
           %% message is ignored for ?OTEL_STATUS_OK
           #{code => ?OTEL_STATUS_OK, message => <<"">>},
           undefined]).


generate_span_tree_input_data(Spans, MaxBranchWidth, MaxBranchDepth) ->
    {pick_random_item(Spans),
     generate_span_tree_branches_input_data(Spans, MaxBranchWidth, MaxBranchDepth)}.


generate_span_tree_branches_input_data(_Spans, _Width, Depth) when Depth =< 0 ->
    [];
generate_span_tree_branches_input_data(Spans, Width, Depth) ->
    [ {Span, generate_span_tree_branches_input_data(Spans, Width, Depth - 1)}
      || Span <- pick_random_items(Spans, Width, true) ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


randomize_span_pattern(#{
                         parent_span_id := ParentSpanId,
                         links := Links,
                         events := Events,
                         attributes := Attributes,
                         status := Status
                        } = SpanPattern) ->
    %% randomize the Span pattern a bit
    MaybeRemoveKeys = [attributes, events, links, status, trace_id],
    RandomKeys = pick_random_items(MaybeRemoveKeys, length(MaybeRemoveKeys), true),
    RemoveKeys = case ParentSpanId of
                     undefined ->
                         %% keep span_id for the root span
                         [parent_span_id | RandomKeys];
                     _ ->
                         [span_id, parent_span_id | RandomKeys]
                 end,
    maps:without(RemoveKeys,
                 SpanPattern#{
                   trace_id => '_',
                   links => pick_random_items(Links, 2, false),
                   events => pick_random_items(Events, 2, false),
                   attributes => pick_random_map_keys(Attributes),
                   status => pick_random_map_keys(Status)
                  }).


generate_linked_span_trees(SpanTrees, ConvertPatternFn) ->
    FoldFn =
        fun(SpanTree, {SpanTreePatterns, Links}) ->
                {NewSpanTreePattern, NewLinks} =
                    generate_span_tree(SpanTree, Links, ConvertPatternFn),
                {[NewSpanTreePattern | SpanTreePatterns],
                 lists:append(NewLinks, Links)}
        end,
    {SpanTreePatterns, _Links} = lists:foldl(FoldFn, {[], []}, SpanTrees),
    SpanTreePatterns.


generate_span_tree(SpanTree, ConvertPatternFn) ->
    {SpanTreePattern, _Links} = generate_span_tree(SpanTree, [], ConvertPatternFn),
    SpanTreePattern.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_span_tree({#{name := Name} = Span, Children}, Links, ConvertPatternFn) ->
    assert_span_input_data(Span),
    SpanLinks = pick_random_items(Links, 4, true),
    Attributes = maps:get(attributes, Span, #{}),
    Events = maps:get(events, Span, []),
    Status = maps:get(status, Span, undefined),
    ParentSpanId = get_current_span_id(),
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
              ?add_events(Events),
              set_status(Status),
              CompleteSpanPattern = Span#{
                                      span_id => SpanId,
                                      trace_id => TraceId,
                                      parent_span_id => ParentSpanId,
                                      links => SpanLinksPattern,
                                      events => Events,
                                      attributes => Attributes
                                     },
              SpanPattern = ConvertPatternFn(CompleteSpanPattern),
              NewLink = create_new_link(SpanCtx, Attributes),
              {{SpanPattern, Branches}, [NewLink | NewLinks]}
      end).


assert_span_input_data(Span) ->
    Keys = maps:keys(Span),
    SupportedKeys = [name, status, attributes, events],
    case Keys -- SupportedKeys of
        [] -> ok;
        UnsupportedKeys -> error(span_input_data_keys, UnsupportedKeys)
    end.


set_status(undefined) ->
    ok;
set_status(#{code := Code, message := Message}) ->
    ?set_status(Code, Message).


create_new_link(SpanCtx, Attributes) ->
    RandomAttributes = pick_random_map_keys(Attributes),
    opentelemetry:link(SpanCtx, RandomAttributes).


get_current_span_id() ->
    case ?current_span_ctx of
        undefined -> undefined;
        #span_ctx{span_id = SpanId} -> SpanId
    end.


link_to_pattern(Link) ->
    %% note that the opentelemetry:link() type returned by opentelemetry:link/2 is a map.
    %% While the #span{} record stores links as #link{} records (see record definitions
    %% in the otel_span.hrl header and the definition of the otel_events:t() type), after
    %% converting the #span{} record using the opentelemetry_testing:convert/1 function,
    %% the format of the links in the resulting map is similar to the opentelemetry:link()
    %% type (in fact, it is identical).
    maps:with([trace_id, span_id, attributes], Link).


pick_random_map_keys(Map) when is_map(Map) ->
    KVList = maps:to_list(Map),
    RandomKVList = pick_random_items(KVList, 4, true),
    maps:from_list(RandomKVList);
pick_random_map_keys(Term) ->
    %% covers 'undefined' span status case
    Term.


pick_random_item(List) when length(List) > 0 ->
    %% pick only one item from the List
    hd(pick_random_items(List, 1, false)).


pick_random_items([], _MaxN, _AllowEmptyResult) ->
    [];
pick_random_items(_, 0, _AllowEmptyResult) ->
    [];
pick_random_items(List, MaxN, AllowEmptyResult) ->
    Length = length(List),
    N = case AllowEmptyResult of
            true -> rand:uniform(min(Length, MaxN));
            false -> rand:uniform(min(Length, MaxN)) + 1
        end,
    RandomIndexes = lists:uniq(tl([ rand:uniform(Length) || _ <- lists:seq(1, N) ])),
    [ lists:nth(Index, List) || Index <- RandomIndexes ].
