defmodule SpanTreeGenerator do
  alias StreamData, as: SD

  def span_tree_input_data_gen(max_branch_width, max_branch_depth)
      when max_branch_depth > 0 do
    SD.tuple(
      {span_input_data_gen(),
       SD.list_of(
         span_tree_input_data_gen(max_branch_width, max_branch_depth - 1),
         max_length: max_branch_width
       )}
    )
  end

  def span_tree_input_data_gen(_max_branch_width, _max_branch_depth) do
    SD.tuple({span_input_data_gen(), SD.constant([])})
  end

  defp span_input_data_gen do
    SD.bind(
      {name_gen(), kind_gen(), status_gen(), attributes_gen(), events_gen()},
      fn {name, kind, status, attributes, events} ->
        SD.constant(%{
          name: name,
          kind: kind,
          attributes: attributes,
          events: events,
          status: status
        })
      end
    )
  end

  defp name_gen do
    SD.one_of([
      SD.filter(
        SD.atom(:alphanumeric),
        fn atom -> atom != :_ and atom != :"" end
      ),
      SD.binary(min_length: 1, max_length: 10)
    ])
  end

  defp kind_gen do
    SD.one_of([:internal, :server, :client, :producer, :consumer])
  end

  defp status_gen do
    SD.one_of([:undefined, status_map_gen()])
  end

  defp status_map_gen do
    SD.bind(
      {status_code_gen(), SD.binary(max_length: 10)},
      fn {status_code, message} ->
        case status_code do
          :error ->
            SD.constant(%{code: status_code, message: message})

          _ ->
            ## the message is ignored and reset to <<"">>
            ## for any status except :error
            SD.constant(%{code: status_code, message: ""})
        end
      end
    )
  end

  defp status_code_gen do
    SD.one_of([:unset, :ok, :error])
  end

  defp attributes_gen do
    SD.bind(
      SD.list_of(attribute_gen(), max_length: 5),
      fn keyword_attributes ->
        ## ensure that invalid attributes are not added
        attribute_map = :otel_attributes.process_attributes(keyword_attributes)
        SD.constant(attribute_map)
      end
    )
  end

  defp attribute_gen do
    SD.tuple({name_gen(), attribute_value_gen()})
  end

  defp attribute_value_gen do
    SD.one_of([
      SD.list_of(simple_attribute_value_gen(), max_length: 5),
      simple_attribute_value_gen()
    ])
  end

  defp simple_attribute_value_gen do
    SD.one_of([
      SD.binary(max_length: 10),
      SD.atom(:alphanumeric),
      SD.integer(),
      SD.float(),
      SD.boolean()
    ])
  end

  defp events_gen do
    SD.list_of(event_gen(), max_length: 5)
  end

  defp event_gen do
    SD.bind(
      {name_gen(), attributes_gen()},
      fn {name, attributes} ->
        SD.constant(%{name: name, attributes: attributes})
      end
    )
  end
end
