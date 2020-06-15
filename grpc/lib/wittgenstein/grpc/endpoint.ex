defmodule Wittgenstein.GRPC.Endpoint do
  use GRPC.Endpoint

  intercept GRPC.Logger.Server
  run Wittgenstein.GRPC.Server

end
