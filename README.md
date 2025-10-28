# OpentelemetryTesting

This application provides a toolset for testing `OpenTelemetry` spans reporting. The approach proposed by the `OpenTelemetry` framework is based on the interception of spans reporting and following manual analysis of the reported data. It works, but has a couple of flaws:
 * Parallel execution of tests is impossible.
 * Spans are reported as Erlang records, this introduces multiple issues:
    - Working with records is more complicated in Elixir (and other BEAM languages) than in Erlang.
    - Some of the records are declared internally in the modules (e.g. #links{} and #events{} records). It makes data analysis harder, even in Erlang.
 * A manual check of every span's `parent_span_id` is required to verify the nesting of the spans. This is one of the critical things to check because context propagation between processes is easy to break during code refactoring.

`opentelemetry_testing` application tries to address all of those issues. It consists of 3 main elements:
 * `span_collector` - an ETS-based `gen_server` that intercepts and stores all the reported spans. It also provides an interface to organize spans in a tree-like structure with preliminary span transformation.
 * `span_convertor` - this module provides interfaces for converting span records into maps.
 * `span_matcher` - can be used for pattern-matching data structures, incl. span tree-like structures.

## How to use it

Use interfaces from `OpentelemetryTesting` module for Elixir projects
and `opentelemetry_testing` module for Erlang projects.

1) Start `span_collector` server.
  * Add `ensure_started/0` call to `test_helper.exs` for ExUnit
  * or call `ensure_started/0` at the `SuiteModule:init_per_suite/1`
    interface for Common Test. Since CT doesn't run multiple suites
    in parallel, you may also want to reset `span_collector` ETS table
    using `reset/0` interface.

2) If possible, wrap your test scenario in a dummy root span.
  * Use `OpenTelemetry.Tracer.with_span/2` macro for Elixir projects
  * or `?with_span/3` macro from `otel_tracer.hrl` for Erlang projects.
  * Extract root span's `trace_id` and `span_id` (you can use
    `OpentelemetryTesting.get_span_ids/0` helper function for Elixir projects).

3) Wait for the root span to be reported and build the span tree for it.
  * Use `wait_for_span/3` and `build_span_tree/2` functions for this.
  * Span tree is a recursive tuple structure with two elements:
    * The first element of the tuple is the span map.
    * The second element is a list of span trees (child spans).

```elixir
test "demo test case" do
  ## wrap demo_scenario() in a root span and extract its trace_id and span_id.
  {trace_id, span_id} =
    Tracer.with_span "dummy root span" do
      ## add here your custom scenario code.
      OpentelemetryTesting.get_span_ids()
    end
  ## wait for the root span to be reported.
  timeout_ms = 1000
  %{trace_id: trace_id, span_id: span_id, name: "dummy root span"} =
    OpentelemetryTesting.wait_for_span!(trace_id, span_id, timeout_ms)
  ## build a span tree structure for the root span.
  span_tree = OpentelemetryTesting.build_span_tree!(trace_id, span_id)
  ## verify the span tree by matching it against the pattern.
  ## ...
end
```

```erlang
demo_test(_Config) ->
  %% wrap demo_scenario() in a root span and extract its trace_id and span_id.
  {TraceId, SpanId} =
    ?with_span(<<"dummy root span">>,
               fun(SpanCtx) ->
                 %% add here your custom scenario code.
                 #span_ctx{trace_id = TraceId, span_id = SpanId} = SpanCtx,
                 {TraceId, SpanId}
               end),
  %% wait for the root span to be reported.
  TimeoutMs = 300,
  ?assertMatch(
    {ok, #{trace_id := TraceId, span_id := SpanId, name := <<"dummy root span">>}},
    opentelemetry_testing:wait_for_span(TraceId, SpanId, TimeoutMs)),
  %% build a span tree structure for the root span.
  {ok, SpanTree} = opentelemetry_testing:build_span_tree(TraceId, SpanId),
  %% verify the span tree by matching it against the pattern.
  %% ...
  ok.
```
4) Validate the span tree by matching it against the pattern.
  * Create a span tree template with all the expected span attributes,
    links, events, etc.
  * Use `match/2` function for verification of the span tree.
