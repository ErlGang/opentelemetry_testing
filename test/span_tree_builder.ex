defmodule SpanTreeBuilder do
  require OpenTelemetry.Tracer, as: Tracer

  #########################################################################
  ## API
  #########################################################################

  def randomize_span_pattern(
        %{
          parent_span_id: parent_span_id,
          links: links,
          events: events,
          attributes: attributes,
          status: status
        } = span_pattern
      ) do
    ## randomize the span pattern a bit
    maybe_remove_keys = [:kind, :status, :attributes, :events, :links]
    random_keys = pick_random_items(maybe_remove_keys, length(maybe_remove_keys), true)

    remove_keys =
      case parent_span_id do
        :undefined ->
          ## keep trace_id and span_id for the root span
          [:parent_span_id | random_keys]

        _ ->
          [:trace_id, :span_id, :parent_span_id | random_keys]
      end

    %{
      span_pattern
      | status: pick_random_map_keys(status),
        attributes: pick_random_map_keys(attributes),
        events: pick_random_items(events, 2, false),
        links: pick_random_items(links, 2, false)
    }
    |> Map.merge(%{start_time: :_, end_time: :_})
    |> Map.drop(remove_keys)
  end

  def generate_linked_span_trees(span_trees, convert_pattern_fn) do
    fold_fn = fn span_tree, {span_tree_patterns, links} ->
      {new_span_tree_pattern, new_links} =
        generate_span_tree(span_tree, links, convert_pattern_fn)

      {[new_span_tree_pattern | span_tree_patterns], Enum.concat(new_links, links)}
    end

    {span_tree_patterns, _links} = List.foldl(span_trees, {[], []}, fold_fn)
    span_tree_patterns
  end

  def generate_span_tree(span_tree, convert_pattern_fn) do
    {span_tree_patterns, _links} = generate_span_tree(span_tree, [], convert_pattern_fn)
    span_tree_patterns
  end

  #########################################################################
  ## local interfaces
  #########################################################################

  defp generate_span_tree({%{name: name} = span, children}, links, convert_pattern_fn) do
    assert_span_input_data(span)
    parent_span_id = get_current_span_id()
    kind = Map.get(span, :kind, :internal)
    status = Map.get(span, :status, :undefined)
    attributes = Map.get(span, :attributes, %{})
    events = Map.get(span, :events, [])
    span_links = pick_random_items(links, 4, true)

    Tracer.with_span name, %{kind: kind, attributes: attributes, links: span_links} do
      {trace_id, span_id} = OpentelemetryTesting.get_span_ids()

      branches_and_new_links =
        Enum.map(children, &generate_span_tree(&1, links, convert_pattern_fn))

      {branches, new_links} = Enum.unzip(branches_and_new_links)

      ## the links are added to the span in reverse order, but we want to keep
      ## the order of the links in the pattern the same as in the span data.
      span_links_pattern = Enum.map(span_links, &link_to_pattern(&1)) |> Enum.reverse()

      ## the events are added to the span in reverse order, but we want to keep
      ## the order of the events in the pattern the same as in the span data.
      Tracer.add_events(Enum.reverse(events))
      set_status(status)

      span_pattern =
        span
        |> Map.merge(%{
          span_id: span_id,
          trace_id: trace_id,
          parent_span_id: parent_span_id,
          attributes: attributes,
          events: events,
          links: span_links_pattern
        })
        |> convert_pattern_fn.()

      link = create_new_link(attributes)
      {{span_pattern, branches}, [link | Enum.concat(new_links)]}
    end
  end

  defp assert_span_input_data(span) do
    supported_keys = [:name, :kind, :status, :attributes, :events]
    keys = Map.keys(span)

    case keys -- supported_keys do
      [] -> :ok
      unsupported_keys -> raise "unsupported_keys = #{unsupported_keys}"
    end
  end

  defp get_current_span_id do
    case OpentelemetryTesting.get_span_ids() do
      {_trace_id, span_id} -> span_id
      :undefined -> :undefined
    end
  end

  defp link_to_pattern(link) do
    Map.take(link, [:trace_id, :span_id, :attributes])
  end

  defp set_status(status) do
    case status do
      :undefined -> :ok
      %{code: code, message: message} -> Tracer.set_status(code, message)
    end
  end

  defp create_new_link(attributes) do
    context = Tracer.current_span_ctx()
    random_attributes = pick_random_map_keys(attributes)
    OpenTelemetry.link(context, random_attributes)
  end

  defp pick_random_map_keys(:undefined) do
    ## covers :undefined span status case
    :undefined
  end

  defp pick_random_map_keys(map) do
    keys = Map.keys(map)
    drop_keys = pick_random_items(keys, length(keys), true)
    Map.drop(map, drop_keys)
  end

  defp pick_random_items([], _max_n, _allow_empty) do
    []
  end

  defp pick_random_items(_list, 0, _allow_empty) do
    []
  end

  defp pick_random_items(list, max_n, allow_empty) do
    length = length(list)

    n =
      case allow_empty do
        true -> :rand.uniform(min(length, max_n) + 1) - 1
        false -> :rand.uniform(min(length, max_n))
      end

    ## indexes must be unique and sorted in ascending order
    random_indexes =
      sequence(n) |> Enum.map(fn _ -> :rand.uniform(length) - 1 end) |> Enum.sort() |> Enum.uniq()

    Enum.map(random_indexes, &Enum.at(list, &1))
  end

  defp sequence(0), do: []
  defp sequence(n) when n > 0, do: 1..n
end
