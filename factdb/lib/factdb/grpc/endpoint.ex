defmodule FactDB.GRPC.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  run(FactDB.GRPC.Server)
end
