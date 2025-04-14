defmodule OpentelemetryTestingTest do
  use ExUnit.Case
  doctest OpentelemetryTesting

  test "greets the world" do
    assert OpentelemetryTesting.hello() == :world
  end
end
