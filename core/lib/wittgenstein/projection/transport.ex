defmodule Wittgenstein.Projection.Transport do
  alias Wittgenstein.Uri

  defmodule Sender do
    @callback project(Uri.t()) :: :ok | {:error, term()}
    defmacro __using__(_opts) do
      quote do
        @behaviour Wittgenstein.Projection.Transport.Sender
      end
    end
  end

  defmodule Receiver do
    defmacro __using__(_opts) do
      quote do
      end
    end
  end
end
