defmodule Wittgenstein.Model.Entity do
  use Ecto.Schema
  import Ecto.Changeset
  alias Ecto.Changeset

  require Wittgenstein.Uri
  alias Wittgenstein.Uri
  alias Wittgenstein.Model.Fact

  @opaque t() :: %{
            required(:uri) => Uri.t(),
            required(:namespace) => Atom.t(),
            required(:kind) => Atom.t(),
            required(:values) => [Fact.t()]
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

  def uri(%__MODULE__{uri: x}), do: x
  def kind(%__MODULE__{kind: x}), do: x
  def values(%__MODULE__{values: x}), do: x

  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}
  def set_values(%__MODULE__{} = f, x) when is_list(x), do: %{f | values: x}

  def to_map(%__MODULE__{} = e) do
    e
    |> Map.from_struct()
    |> Map.update!(:uri, &Uri.to_string/1)
    |> Map.update!(:values, &Enum.map(&1, fn f -> Fact.to_map(f) end))
  end

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

  @spec consolidate(t()) :: Consolidated.t()
  def consolidate(%__MODULE__{uri: uri, values: values}) do
    values
    |> Enum.map(fn fact -> {Fact.field(fact), Fact.value(fact)} end)
    |> Map.new()
    |> Map.merge(%{"@uri" => uri |> Uri.to_string()})
  end

  defmodule Consolidated do
    @type t() :: %{binary() => binary()}
  end
end
