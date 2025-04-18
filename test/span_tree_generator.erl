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
    ?LET({Name, Kind, Status, Attributes, Events},
         {name_gen(), kind_gen(), status_gen(), attributes_gen(), events_gen()},
         #{
           name => Name,
           kind => Kind,
           attributes => Attributes,
           events => Events,
           status => Status
          }).


name_gen() ->
    not_empty_gen(oneof([atom(), small_binary()])).


not_empty_gen(Type) ->
    ?SUCHTHAT(Name, Type, Name =/= <<>> andalso Name =/= '' andalso Name =/= '_').


kind_gen() ->
    ?LET(X, opentelemetry:span_kind(), X).


status_gen() ->
    oneof([undefined,
           status_map_gen()]).


small_binary() ->
    ?LET(Length, integer(0, 10), binary(Length)).


status_map_gen() ->
    ?LET({StatusCode, Message},
         {opentelemetry:status_code(), small_binary()},
         case StatusCode of
             ?OTEL_STATUS_ERROR ->
                 #{code => StatusCode, message => Message};
             _ ->
                 %% the message is ignored and reset to <<"">>
                 %% for any status except ?OTEL_STATUS_ERROR
                 #{code => StatusCode, message => <<"">>}
         end).


attributes_gen() ->
    ?LET(PropList,
         ?LET(Length, integer(0, 5), vector(Length, attribute_gen())),
         %% ensure that invalid attributes are not added
         otel_attributes:process_attributes(PropList)).


attribute_gen() ->
    ?LET(X, {name_gen(), attribute_value()}, X).


attribute_value() ->
    %% opentelemetry:attribute_value() also supports tuple values, but they are
    %% immediately converted into lists by otel_attributes:process_attributes/1
    oneof([?LET(Length, integer(0, 5), vector(Length, attribute_simple_value())),
           attribute_simple_value()]).


attribute_simple_value() ->
    oneof([small_binary(), atom(), number(), boolean()]).


events_gen() ->
    ?LET(Length, integer(0, 5), vector(Length, event_gen())).


event_gen() ->
    ?LET({Name, Attributes},
         {name_gen(), attributes_gen()},
         #{name => Name, attributes => Attributes}).


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
    MaybeRemoveKeys = [kind, status, attributes, events, links],
    RandomKeys = pick_random_items(MaybeRemoveKeys, length(MaybeRemoveKeys), true),
    RemoveKeys = case ParentSpanId of
                     undefined ->
                         %% keep trace_id and span_id for the root span
                         [parent_span_id | RandomKeys];
                     _ ->
                         [trace_id, span_id, parent_span_id | RandomKeys]
                 end,
    maps:without(RemoveKeys,
                 SpanPattern#{
                   status => pick_random_map_keys(Status),
                   attributes => pick_random_map_keys(Attributes),
                   events => pick_random_items(Events, 2, false),
                   links => pick_random_items(Links, 2, false),
                   start_time => '_',
                   end_time => '_'
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
    ParentSpanId = get_current_span_id(),
    Kind = maps:get(kind, Span, ?SPAN_KIND_INTERNAL),
    Status = maps:get(status, Span, undefined),
    Attributes = maps:get(attributes, Span, #{}),
    Events = maps:get(events, Span, []),
    SpanLinks = pick_random_items(Links, 4, true),
    ?with_span(
      Name,
      #{kind => Kind, attributes => Attributes, links => SpanLinks},
      fun(SpanCtx) ->
              BranchesAndNewLinks = [ generate_span_tree(C, Links, ConvertPatternFn)
                                      || C <- Children ],
              Branches = [ Branch || {Branch, _} <- BranchesAndNewLinks ],
              NewLinks = lists:append([ Link || {_, Link} <- BranchesAndNewLinks ]),
              SpanId = SpanCtx#span_ctx.span_id,
              TraceId = SpanCtx#span_ctx.trace_id,

              %% the links are added to the span in reverse order, but we want to keep
              %% the order of the links in the pattern the same as in the span data.
              SpanLinksPattern =
                  [ link_to_pattern(L) || L <- lists:reverse(SpanLinks) ],

              %% the events are added to the span in reverse order, but we want to keep
              %% the order of the events in the pattern the same as in the span data.
              ?add_events(lists:reverse(Events)),

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
    SupportedKeys = [name, kind, status, attributes, events],
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
    %% in the original span pattern of the randomize_span_pattern/1 function,
    %% the lists of link and event patterns are ordered in the same way as
    %% the data instances (see the generate_span_tree/3 function), and we don't
    %% want a pattern list like this:
    %%    [#{ attributes => #{ attr1 => <<"value1">>}, name => event1 },
    %%     #{ attributes => #{}, name => event2 },
    %%     #{ attributes => #{}, name => event1 }]
    %% to turn into:
    %%    [#{ attributes => #{}, name => event1 },
    %%     #{ attributes => #{ attr1 => <<"value1">>}, name => event1 }]
    %%
    %% let's ensure the random indices are sorted in ascending order, so
    %% the randomize_span_pattern/1 function does not shuffle the patterns
    %% in the event/link list, and no accidental match can happen for a
    %% less restrictive pattern.
    RandomIndexes = lists:usort(tl([ rand:uniform(Length) || _ <- lists:seq(1, N) ])),
    [ lists:nth(Index, List) || Index <- RandomIndexes ].
