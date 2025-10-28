defmodule DemoTest do
  ##############################################################
  ## make sure the OpentelemetryTesting.ensure_started() call ##
  ## is added to test_helper.exs                              ##
  ##############################################################

  use ExUnit.Case, async: true
  require OpenTelemetry.Tracer, as: Tracer

  test "demo test case" do
    ## wrap demo_scenario() in a root span and extract its trace_id and span_id.
    {trace_id, span_id} =
      Tracer.with_span "dummy root span" do
        demo_scenario()
        OpentelemetryTesting.get_span_ids()
      end

    ## wait for the root span to be reported.
    timeout_ms = 1000

    %{trace_id: trace_id, span_id: span_id, name: "dummy root span"} =
      OpentelemetryTesting.wait_for_span!(trace_id, span_id, timeout_ms)

    ## build a span tree structure for the root span.
    span_tree = OpentelemetryTesting.build_span_tree!(trace_id, span_id)

    ## create a span tree template with all the expected span attributes,
    ## links, events, etc. You can also use '$special_atoms' to extract
    ## some data, such as the trace_id/span_id of the linked spans.
    span_tree_pattern =
      {%{name: "dummy root span"},
       [
         {%{
            name: "main span",
            attributes: %{
              :"span attr 1" => "1",
              "span attr 3" => 3
            },
            status: %{code: :error},
            kind: :client,
            events: [
              %{
                name: :"event 2",
                attributes: %{:"event attr 2" => "2"}
              }
            ],
            links: [
              %{
                attributes: %{:"link attr 1" => "1"},
                trace_id: :"$linked_trace_id",
                span_id: :"$linked_span_id"
              }
            ]
          },
          [
            {%{
               name: :nested_span,
               attributes: %{"span attr 2" => 2}
             }, []}
          ]}
       ]}

    ## verify the span tree by matching it against the pattern.
    matched_values = OpentelemetryTesting.match!(span_tree, span_tree_pattern)

    ## use matched_values map for further analysis, e.g. check the linked spans.
    %{
      :"$linked_trace_id" => linked_trace_id,
      :"$linked_span_id" => linked_span_id
    } = matched_values

    ## get the linked span tree and verify it.
    linked_span_tree = OpentelemetryTesting.build_span_tree!(linked_trace_id, linked_span_id)

    linked_span_tree_pattern =
      {%{
         name: :linked_span,
         events: [
           %{
             name: "event 1",
             attributes: %{"event attr 1" => 1}
           }
         ]
       }, []}

    OpentelemetryTesting.match!(linked_span_tree, linked_span_tree_pattern)
  end

  defp demo_scenario do
    ## linked span
    task =
      Task.async(fn ->
        Tracer.with_span :linked_span do
          Tracer.add_event("event 1", [{"event attr 1", 1}])
          Tracer.current_span_ctx()
        end
      end)

    linked_span_ctx = Task.await(task)
    links = [{linked_span_ctx, [{:"link attr 1", <<"1">>}]}]

    ## nested span
    Tracer.with_span "main span", %{links: links, kind: :client} do
      Tracer.set_attributes([{:"span attr 1", "1"}])

      Tracer.with_span :nested_span do
        Tracer.set_attributes([{"span attr 2", 2}])
      end

      Tracer.set_attributes([{"span attr 3", 3}])
      Tracer.add_event(:"event 2", [{:"event attr 2", "2"}])
      Tracer.set_status(:error)
    end
  end
end
