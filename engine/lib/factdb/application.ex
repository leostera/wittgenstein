defmodule FactDB.Application do
  @moduledoc false

  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    :ok = :factdb.start()

    children = [
      supervisor(GRPC.Server.Supervisor, [{FactDB.GRPC.Endpoint, 50051}])
    ]

    opts = [strategy: :one_for_one, name: FactDB.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
