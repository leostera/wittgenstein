defmodule StarwarsTest do
  use ExUnit.Case
  doctest Starwars

  test "greets the world" do
    assert Starwars.hello() == :world
  end
end
