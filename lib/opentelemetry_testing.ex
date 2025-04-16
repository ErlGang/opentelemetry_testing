defmodule OpentelemetryTesting do
  require Record

  #########################################################################
  ## attributes
  #########################################################################

  ## otel_span.hrl is included in both otel_links.erl and otel_events.erl,
  ## so there's no need parse it separately
  @records Enum.concat([
             Record.extract_all(from: "deps/opentelemetry_api/src/otel_attributes.erl"),
             Record.extract_all(
               from_lib: "opentelemetry/src/otel_links.erl",
               includes: ["#{:code.lib_dir(:opentelemetry)}/include/"]
             ),
             Record.extract_all(
               from_lib: "opentelemetry/src/otel_events.erl",
               includes: ["#{:code.lib_dir(:opentelemetry)}/include/"]
             )
           ])
           |> Enum.uniq()

  @record_definitions_map Enum.reduce(
                            @records,
                            %{},
                            fn {record, fields}, acc ->
                              fields = Keyword.keys(fields)
                              :record_convertor.add_record(record, fields, acc)
                            end
                          )

  #########################################################################
  ## API implementation
  #########################################################################

  def ensure_started do
    IO.inspect(Process.info(self(), :current_stacktrace))
    ## when running 'mix test,' dependency applications are normally started
    ## before test execution, so the test configuration for the :opentelemetry
    ## app should be provided in the "config/config.exs" file
    :ok = :span_collector.ensure_started()
  end

  defdelegate reset, to: :span_collector

  defdelegate get_span_ids_by_name(name), to: :span_collector

  def get_spans_by_name(name) do
    spans = :span_collector.get_spans_by_name(name)

    for span <- spans do
      convert_span(span)
    end
  end

  def wait_for_span(trace_id, span_id, timeout) do
    case :span_collector.wait_for_span(trace_id, span_id, timeout) do
      {:ok, span} -> {:ok, convert_span(span)}
      ret -> ret
    end
  end

  def build_span_tree(trace_id, span_id) do
    :span_collector.build_span_tree(trace_id, span_id, &convert_span/1)
  end

  def convert_span(span) do
    :record_convertor.records_to_maps(span, @record_definitions_map)
    |> maybe_simplify_attributes
    |> maybe_simplify_events
    |> maybe_simplify_links
  end

  defdelegate match(value, pattern), to: :span_matcher

  #########################################################################
  ## local functions
  #########################################################################

  defp maybe_simplify_attributes(
         %{
           attributes: %{
             "$record_name": :attributes,
             map: attributes
           }
         } = map
       ) do
    %{map | attributes: attributes}
  end

  defp maybe_simplify_attributes(term) do
    term
  end

  defp maybe_simplify_events(
         %{
           events: %{
             "$record_name": :events,
             list: events
           }
         } = map
       ) do
    simple_events = for e <- events, do: maybe_simplify_attributes(e)
    %{map | events: simple_events}
  end

  defp maybe_simplify_events(term) do
    term
  end

  defp maybe_simplify_links(
         %{
           links: %{
             "$record_name": :links,
             list: links
           }
         } = map
       ) do
    simple_links = for l <- links, do: maybe_simplify_attributes(l)
    %{map | links: simple_links}
  end

  defp maybe_simplify_links(term) do
    term
  end
end
