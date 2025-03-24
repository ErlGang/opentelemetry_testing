-module(opentelemetry_testing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([ensure_started/0, reset/0,
         get_span_id_by_name/1,
         get_spans_by_name/1,
         wait_for_span/2,
         build_span_tree/1,
         build_span_tree/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started() ->
    application:load(opentelemetry),
    application:set_env(opentelemetry, traces_exporter, none),
    application:set_env(opentelemetry, processors, [{otel_simple_processor, #{}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    ok = span_collector:ensure_started().

reset() ->
    span_collector:reset().

get_span_id_by_name(Name) ->
    span_collector:get_span_id_by_name(Name).

get_spans_by_name(Name) ->
    span_collector:get_spans_by_name(Name).

wait_for_span(SpanId, Timeout) ->
    span_collector:wait_for_span(SpanId, Timeout).

build_span_tree(SpanId) ->
    span_collector:build_span_tree(SpanId).

build_span_tree(SpanId, ConvertSpanFn) ->
    span_collector:build_span_tree(SpanId, ConvertSpanFn).
