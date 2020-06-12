defmodule Wittgenstein.Model.Entity do
  use Ecto.Schema
  import Ecto.Changeset
  alias Ecto.Changeset

  require Wittgenstein.Uri
  alias Wittgenstein.Uri
  alias Wittgenstein.Model.Fact

  @opaque t() :: %__MODULE__{
            uri: Uri.t(),
            namespace: atom(),
            kind: atom(),
            values: [Fact.t()]
          }

  @primary_key false
  embedded_schema do
    field(:uri, Uri.Ecto)
    field(:namespace, :string)
    field(:kind, :string)
    embeds_many(:values, Fact)
  end

  def new(uri) do
    %__MODULE__{
      uri: uri,
      namespace: uri |> Uri.namespace(),
      kind: uri |> Uri.kind(),
      values: []
    }
  end

  @spec uri(t()) :: Uri.t()
  def uri(%__MODULE__{uri: x}), do: x

  @spec kind(t()) :: atom()
  def kind(%__MODULE__{kind: x}), do: x

  @spec values(t()) :: [Fact.t()]
  def values(%__MODULE__{values: x}), do: x

  @spec set_uri(t(), Uri.t()) :: t()
  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}

  @spec set_values(t(), [Fact.t()]) :: t()
  def set_values(%__MODULE__{} = f, x) when is_list(x), do: %{f | values: x}

  @spec to_map(t()) :: map()
  def to_map(%__MODULE__{} = e) do
    e
    |> Map.from_struct()
    |> Map.update!(:uri, &Uri.to_string/1)
    |> Map.update!(:values, &Enum.map(&1, fn f -> Fact.to_map(f) end))
  end

  @spec from_map(map()) :: {:ok, t()} | {:error, :invalid_entity_map}
  def from_map(map) when is_map(map) do
    case apply_action(changeset(map), :insert) do
      {:error, changeset} ->
        {:error, {:invalid_entity_map, changeset}}

      {:ok, entity} ->
        {:ok, entity}
    end
  end

  defp changeset(attrs) do
    %__MODULE__{}
    |> Changeset.cast(attrs, [:uri, :namespace, :kind])
    |> Changeset.cast_embed(:values, required: true)
    |> Changeset.validate_required([:uri, :namespace, :kind, :values])
    |> Changeset.validate_change(:uri, &Uri.Ecto.validate/2)
  end
end
