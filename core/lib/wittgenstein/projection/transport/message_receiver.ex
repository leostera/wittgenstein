defmodule Wittgenstein.Projection.Transport.MessageReceiver do
  defmacro __using__(_opts \\ []) do
    quote do
      use GenServer
      use Wittgenstein.Projection.Transport.Receiver

      def start_link(opts) do
        GenServer.start_link(__MODULE__, opts, name: __MODULE__)
      end

      def init(_args), do: {:ok, :no_state}

      def handle_info([handle_uri: uri], state) do
        handle_uri(uri)
        {:noreply, state}
      end
    end
  end
end
