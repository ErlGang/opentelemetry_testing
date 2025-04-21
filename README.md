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
