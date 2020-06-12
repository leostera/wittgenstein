defmodule Wittgenstein.GRPC.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(GRPC.Server.Supervisor, [{Wittgenstein.GRPC.Endpoint, 50051}])
    ]

    opts = [strategy: :one_for_one, name: Wittgenstein.GRPC.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
