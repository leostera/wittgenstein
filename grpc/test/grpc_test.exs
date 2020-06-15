defmodule GrpcTest do
  use ExUnit.Case
  doctest Grpc

  test "greets the world" do
    assert Grpc.hello() == :world
  end
end
