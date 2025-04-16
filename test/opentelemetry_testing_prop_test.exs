defmodule OpentelemetryTestingPropTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias StreamData, as: SD

  property "OpentelemetryTesting.match/2 returns :true for matching patterns" do
    check all(
            span_trees_input_data <-
              SD.list_of(SpanTreeGenerator.span_tree_input_data_gen(4, 4), length: 4)
          ) do
      tree_patterns =
        :span_tree_builder.generate_linked_span_trees(
          span_trees_input_data,
          &:span_tree_builder.randomize_span_pattern/1
        )

      for {%{trace_id: trace_id, span_id: span_id} = span_pattern, _} = tree_pattern <-
            tree_patterns do
        assert {:ok, span} = OpentelemetryTesting.wait_for_span(trace_id, span_id, 300)
        assert true == OpentelemetryTesting.match(span, span_pattern)

        assert {:ok, span_tree} = OpentelemetryTesting.build_span_tree(trace_id, span_id)
        assert true == OpentelemetryTesting.match(span_tree, tree_pattern)

        # IO.inspect(span_tree, label: :span_tree)
        # IO.inspect(tree_pattern, label: :tree_pattern)
      end
    end
  end
end
