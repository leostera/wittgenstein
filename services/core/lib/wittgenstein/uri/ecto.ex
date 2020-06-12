defmodule Wittgenstein.Uri.Ecto do
  use Ecto.Type

  alias Wittgenstein.Uri

  @impl Ecto.Type
  def type, do: :string

  @impl Ecto.Type
  def cast(uri) when is_binary(uri) do
    with {:error, :invalid_uri} <- Uri.from_string(uri) do
      {:error, [message: "binary #{inspect(uri)} is not a valid uri"]}
    end
  end

  def cast(uri) do
    case Uri.validate(uri) do
      {:error, :invalid_uri} -> {:error, [message: "'#{inspect(uri)}' is not a valid uri"]}
      :ok -> {:ok, uri}
    end
  end

  @impl Ecto.Type
  def load(str) when is_binary(str), do: Uri.from_string(str)

  @impl Ecto.Type
  def dump(uri) when is_binary(uri), do: {:ok, uri}
  def dump(uri), do: {:ok, Uri.to_string(uri)}

  def validate(key, uri) do
    case Uri.validate(uri) do
      {:error, :invalid_uri} ->
        [message: "value '#{inspect(uri)}' on key '#{inspect(key)}' is not a valid uri"]

      :ok ->
        []
    end
  end
end
