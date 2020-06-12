defmodule Wittgenstein.Projection.Transport.MessageSender do
  use GenServer
  use Wittgenstein.Projection.Transport.Sender

  @impl true
  def project(uri), do: GenServer.call(__MODULE__, project: uri)

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args), do: {:ok, :no_state}

  @impl true
  def handle_call([project: uri], _, s), do: do_project_uri(uri, s)

  def do_project_uri(uri, state) do
    projection_list()
    |> Enum.each(fn
      receiver when is_atom(receiver) ->
        case Process.whereis(receiver) do
          nil -> :ok
          _ -> send(receiver, handle_uri: uri)
        end

      receiver when is_pid(receiver) ->
        send(receiver, handle_uri: uri)
    end)

    {:reply, :ok, state}
  end

  def projection_list() do
    Application.get_env(:wittgenstein, __MODULE__, [])
    |> Keyword.get(:projections, [])
  end
end
