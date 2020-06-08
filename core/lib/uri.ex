defmodule Uri do
  @moduledoc """

  A Universal Resource Identifier.

  """

  @opaque t() :: {atom(), atom(), term() | UUID.t()}

  @spec kind(t()) :: atom()
  def kind({_, atom, _}), do: atom

  @spec namespace(t()) :: atom()
  def namespace({ns, _, _}), do: ns

  @spec new(atom(), atom()) :: t()
  def new(namespace, kind), do: new(namespace, kind, UUID.uuid4())

  @spec new(atom(), atom(), term()) :: t()
  def new(namespace, kind, id), do: {namespace, kind, id}

  @doc """
  Serialize a URI into a String. Useful for encoding into various transport
  formats.

  This serialization format is *stable* and RFC-3968[0] compatible, assuming an
  implied scheme:

  [0] https://tools.ietf.org/html/rfc3986
  """
  @spec to_string(t()) :: String.t()
  def to_string({ns, kind, id}), do: "#{ns}:#{kind}:#{id}"

  @doc """
  Given a string representation of an URI, try to parse it into a valid
  `Uri.t()`.

  """
  @spec from_string(String.t()) :: {:ok, t()} | {:error, :invalid_uri}
  def from_string(str) do
    with [ns, kind, id] <- String.split(str, ":", parts: 3),
         ns <- String.to_existing_atom(ns),
         kind <- String.to_existing_atom(kind),
         uri <- {ns, kind, id} do
      {:ok, uri}
    end
  end
end