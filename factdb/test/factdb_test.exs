defmodule FactdbTest do
  use ExUnit.Case
  doctest Factdb

  test "greets the world" do
    assert Factdb.hello() == :world
  end
end
