defmodule SpanConvertorTest do
  use ExUnit.Case, async: true
  require Record

  #########################################################################
  ## attributes
  #########################################################################

  @records Enum.concat([
             Record.extract_all(from: "deps/opentelemetry/include/otel_span.hrl"),
             Record.extract_all(from: "deps/opentelemetry_api/src/otel_attributes.erl"),
             Record.extract_all(
               from: "deps/opentelemetry/src/otel_links.erl",
               includes: ["deps/opentelemetry/include/"]
             ),
             Record.extract_all(
               from: "deps/opentelemetry/src/otel_events.erl",
               includes: ["deps/opentelemetry/include/"]
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
  ## tests
  #########################################################################

  test "check that span_convertor adds all the necessary records" do
    assert @record_definitions_map == :span_convertor.get_record_definitions()
  end
end
