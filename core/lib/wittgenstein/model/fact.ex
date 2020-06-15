defmodule Wittgenstein.Model.Fact do
  use Ecto.Schema
  import Ecto.Changeset
  alias Ecto.Changeset

  alias Wittgenstein.Uri

  @opaque t() :: %__MODULE__{
            entity_uri: Uri.t() | :none,
            field: binary() | :none,
            source_uri: Uri.t() | :none,
            stated_at: NaiveDateTime.t(),
            uri: Uri.t(),
            value: term() | :none
          }

  @primary_key false
  embedded_schema do
    field(:entity_uri, Uri.Ecto)
    field(:uri, Uri.Ecto)
    field(:field, :string)
    field(:source_uri, Uri.Ecto)
    field(:stated_at, :naive_datetime)
    field(:value, :string)
  end

  def new do
    %__MODULE__{
      entity_uri: :none,
      field: :none,
      source_uri: :none,
      stated_at: NaiveDateTime.utc_now(),
      uri: Uri.new(:wittgenstein, :fact),
      value: :none
    }
  end

  @spec entity_uri(t()) :: Uri.t()
  def entity_uri(%__MODULE__{entity_uri: x}), do: x

  @spec field(t()) :: String.t()
  def field(%__MODULE__{field: x}), do: x

  @spec source_uri(t()) :: Uri.t()
  def source_uri(%__MODULE__{source_uri: x}), do: x

  @spec stated_at(t()) :: NaiveDateTime.t()
  def stated_at(%__MODULE__{stated_at: x}), do: x

  @spec uri(t()) :: Uri.t()
  def uri(%__MODULE__{uri: x}), do: x

  @spec value(t()) :: String.t()
  def value(%__MODULE__{value: x}), do: x

  @spec set_entity_uri(t(), Uri.t()) :: t()
  def set_entity_uri(%__MODULE__{} = f, x), do: %{f | entity_uri: x}

  @spec set_field(t(), String.t()) :: t()
  def set_field(%__MODULE__{} = f, x), do: %{f | field: x}

  @spec set_source_uri(t(), Uri.t()) :: t()
  def set_source_uri(%__MODULE__{} = f, x), do: %{f | source_uri: x}

  @spec set_stated_at(t(), NaiveDateTime.t()) :: t()
  def set_stated_at(%__MODULE__{} = f, x), do: %{f | stated_at: x}

  @spec set_uri(t(), Uri.t()) :: t()
  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}

  @spec set_value(t(), String.t()) :: t()
  def set_value(%__MODULE__{} = f, x), do: %{f | value: x}

  @spec to_map(t()) :: map()
  def to_map(f) do
    f
    |> Map.from_struct()
    |> Map.update!(:uri, &Uri.to_string/1)
    |> Map.update!(:source_uri, &Uri.to_string/1)
    |> Map.update!(:entity_uri, &Uri.to_string/1)
  end

  @spec from_map(map()) :: {:ok, t()} | {:error, :invalid_fact_map}
  def from_map(map) when is_map(map) do
    case apply_action(changeset(map), :insert) do
      {:error, changeset} ->
        {:error, {:invalid_fact_map, changeset}}

      {:ok, entity} ->
        {:ok, entity}
    end
  end

  def changeset(changeset \\ %__MODULE__{}, attrs) do
    changeset
    |> Changeset.cast(attrs, [:uri, :source_uri, :entity_uri, :field, :value, :stated_at])
    |> Changeset.validate_required([:entity_uri, :field, :source_uri, :stated_at, :uri, :value])
    |> Changeset.validate_change(:uri, &Uri.Ecto.validate/2)
    |> Changeset.validate_change(:entity_uri, &Uri.Ecto.validate/2)
    |> Changeset.validate_change(:source_uri, &Uri.Ecto.validate/2)
  end
end
