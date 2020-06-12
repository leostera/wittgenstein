defmodule Wittgenstein.Projection do
  alias Wittgenstein.Config
  alias Wittgenstein.Uri

  def project(uri) do
    Config.projection_transport_sender().project(uri)
  end

  @callback handle_uri(Uri.t()) :: :ok | {:error, term()}

  defmacro __using__(_opts \\ []) do
    receiver = Config.projection_transport_receiver()

    quote do
      @behaviour Wittgenstein.Projection
      use unquote(receiver)
    end
  end
end
