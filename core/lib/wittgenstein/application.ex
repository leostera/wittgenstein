defmodule Wittgenstein.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Wittgenstein.Config.store_backend(), []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Wittgenstein.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
