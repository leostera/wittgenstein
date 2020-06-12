defmodule Wittgenstein.Application do
  @moduledoc false

  use Application

  def start, do: Application.ensure_all_started(:wittgenstein)

  def stop, do: Application.stop(:wittgenstein)

  def start(_type, _args) do
    _ = OpenTelemetry.register_application_tracer(:wittgenstein)

    children = [
      {Wittgenstein.Config.store_backend(), []},
      {Wittgenstein.Config.projection_transport_sender(), []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Wittgenstein.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
