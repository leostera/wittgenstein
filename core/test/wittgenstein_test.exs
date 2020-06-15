defmodule WittgensteinTest do
  use ExUnit.Case
  doctest Wittgenstein

  test "greets the world" do
    assert Wittgenstein.hello() == :world
  end
end
