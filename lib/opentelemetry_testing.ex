defmodule OpentelemetryTesting do
  require Record

  @fields Record.extract(:span_ctx, from_lib: "opentelemetry_api/include/opentelemetry.hrl")
  Record.defrecordp(:span_ctx, @fields)

  #########################################################################
  ## local macros
  #########################################################################

  defmacrop raise_error(error) do
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
  @spec ensure_started() :: :ok
  def ensure_started do
    ## when running 'mix test,' dependency applications are normally started
    ## before test execution, so the test configuration for the :opentelemetry
    ## app should be provided in the "config/config.exs" file
    :ok = :span_collector.ensure_started()
  end

  @spec reset() :: :ok
  defdelegate reset, to: :opentelemetry_testing

  @spec get_span_ids_by_name(:opentelemetry.span_name()) ::
          {:ok, {:opentelemetry.trace_id(), :opentelemetry.span_id()}}
          | {:error, :not_found | :span_is_not_unique}
  defdelegate get_span_ids_by_name(name), to: :opentelemetry_testing

  @spec get_spans_by_name(:opentelemetry.span_name()) ::
          [:opentelemetry_testing.span_map()]
  defdelegate get_spans_by_name(name), to: :opentelemetry_testing

  @spec wait_for_span(:opentelemetry.trace_id(), :opentelemetry.span_id(), non_neg_integer()) ::
          {:ok, :opentelemetry_testing.span_map()}
          | {:error, :timeout | :span_is_not_unique}
  defdelegate wait_for_span(trace_id, span_id, timeout), to: :opentelemetry_testing

  @spec build_span_tree(:opentelemetry.trace_id(), :opentelemetry.span_id()) ::
          {:ok, :opentelemetry_testing.span_map_tree()}
          | {:error, :not_found | :span_is_not_unique}
  defdelegate build_span_tree(trace_id, span_id), to: :opentelemetry_testing

  @spec match(:span_matcher.value(), :span_matcher.pattern()) :: :span_matcher.match_result()
  defdelegate match(value, pattern), to: :opentelemetry_testing

  @spec get_span_ids_by_name!(:opentelemetry.span_name()) ::
          {:opentelemetry.trace_id(), :opentelemetry.span_id()}
  def get_span_ids_by_name!(name) do
    case get_span_ids_by_name(name) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  @spec wait_for_span!(:opentelemetry.trace_id(), :opentelemetry.span_id(), non_neg_integer()) ::
          :opentelemetry_testing.span_map()
  def wait_for_span!(trace_id, span_id, timeout) do
    case wait_for_span(trace_id, span_id, timeout) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  @spec build_span_tree!(:opentelemetry.trace_id(), :opentelemetry.span_id()) ::
          :opentelemetry_testing.span_map_tree()
  def build_span_tree!(trace_id, span_id) do
    case build_span_tree(trace_id, span_id) do
      {:ok, data} -> data
      {:error, error} -> raise_error(error)
    end
  end

  @spec match!(:span_matcher.value(), :span_matcher.pattern()) :: true
  def match!(value, pattern) do
    case match(value, pattern) do
      true -> true
      {false, error} -> raise_error(error)
    end
  end

  @spec get_span_ids() :: {:opentelemetry.trace_id(), :opentelemetry.span_id()} | :undefined
  def get_span_ids do
    case OpenTelemetry.Tracer.current_span_ctx() do
      span_ctx(trace_id: trace_id, span_id: span_id) -> {trace_id, span_id}
      :undefined -> :undefined
    end
  end
end

defmodule TestModule do
  def test_function do
    :ok
  end
end
