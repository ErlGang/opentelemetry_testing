defmodule OpentelemetryTesting do
  #########################################################################
  ## macros
  #########################################################################

  defmacro raise_error(error) do
    quote do
      {function, arity} = __ENV__.function

      error_msg =
        inspect(unquote(error), limit: :infinity, printable_limit: :infinity, pretty: true)
        |> then(&Regex.replace(~r"\n", &1, "\n\t\t"))

      raise "#{__MODULE__}.#{function}/#{arity}:\n\t\t#{error_msg}"
    end
  end

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

  def get_span_ids_by_name!(name) do
    case get_span_ids_by_name(name) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  def wait_for_span!(trace_id, span_id, timeout) do
    case wait_for_span(trace_id, span_id, timeout) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  def build_span_tree!(trace_id, span_id) do
    case build_span_tree(trace_id, span_id) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  def match!(value, pattern) do
    case match(value, pattern) do
      true -> true
      {false, error} -> raise_error(error)
    end
  end
end
