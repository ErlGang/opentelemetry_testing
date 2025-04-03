-module(opentelemetry_testing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([ensure_started/0,
         reset/0,
         get_span_id_by_name/1,
         get_spans_by_name/1,
         wait_for_span/2,
         build_span_tree/1, build_span_tree/2,
         convert_span/1,
         match/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ensure_started() ->
    application:load(opentelemetry),
    application:set_env(opentelemetry, traces_exporter, none),
    application:set_env(opentelemetry, processors, [{otel_simple_processor, #{}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ok = span_convertor:init(),
    ok = span_collector:ensure_started().


reset() ->
    span_collector:reset().


get_span_id_by_name(Name) ->
    span_collector:get_span_id_by_name(Name).


get_spans_by_name(Name) ->
    Spans = span_collector:get_spans_by_name(Name),
    [ convert_span(Span) || Span <- Spans ].


wait_for_span(SpanId, Timeout) ->
    case span_collector:wait_for_span(SpanId, Timeout) of
        {ok, Span} -> {ok, convert_span(Span)};
        Ret -> Ret
    end.


build_span_tree(SpanId) ->
    span_collector:build_span_tree(SpanId, fun convert_span/1).


build_span_tree(SpanId, ConvertSpanFn) ->
    span_collector:build_span_tree(SpanId, ConvertSpanFn).


convert_span(Span) ->
    ConversionFunctions = [fun span_convertor:records_to_maps/1,
                           fun maybe_simplify_attributes/1,
                           fun maybe_simplify_events/1,
                           fun maybe_simplify_links/1],
    FoldFn = fun(Fn, Acc) when is_function(Fn, 1) -> Fn(Acc) end,
    lists:foldl(FoldFn, Span, ConversionFunctions).


match(Value, Pattern) ->
    span_matcher:match(Value, Pattern).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maybe_simplify_attributes(#{
                            attributes := #{
                                            '$record_name' := attributes,
                                            map := Attributes
                                           }
                           } = Map) ->
    %% convert attributes to a format similar to opentelemetry:attributes_map() type
    Map#{attributes := Attributes};
maybe_simplify_attributes(Term) -> Term.


maybe_simplify_events(#{
                        events := #{
                                    '$record_name' := events,
                                    list := Events
                                   }
                       } = Map) ->
    %% convert events to a format similar to opentelemetry:event() type
    SimpleEvents = [ maybe_simplify_attributes(E) || E <- Events ],
    Map#{events := SimpleEvents};
maybe_simplify_events(Term) -> Term.


maybe_simplify_links(#{
                       links := #{
                                  '$record_name' := links,
                                  list := Links
                                 }
                      } = Map) ->
    %% convert links to a format similar to opentelemetry:link() type
    SimpleLinks = [ maybe_simplify_attributes(L) || L <- Links ],
    Map#{links := SimpleLinks};
maybe_simplify_links(Term) -> Term.
