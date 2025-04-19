defmodule OpentelemetryTesting do
  #########################################################################
  ## API implementation
  #########################################################################

  def ensure_started do
    ## when running 'mix test,' dependency applications are normally started
    ## before test execution, so the test configuration for the :opentelemetry
    ## app should be provided in the "config/config.exs" file
    :ok = :span_collector.ensure_started()
  end

  defdelegate reset, to: :opentelemetry_testing
  defdelegate get_span_ids_by_name(name), to: :opentelemetry_testing
  defdelegate get_spans_by_name(name), to: :opentelemetry_testing
  defdelegate wait_for_span(trace_id, span_id, timeout), to: :opentelemetry_testing
  defdelegate build_span_tree(trace_id, span_id), to: :opentelemetry_testing
  defdelegate match(value, pattern), to: :opentelemetry_testing
end
