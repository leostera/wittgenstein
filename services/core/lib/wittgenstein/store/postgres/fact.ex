defmodule Wittgenstein.Store.Postgres.Fact do
  use Ecto.Schema
  import Ecto.Changeset

  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Uri

  @fields [
    :entity_uri,
    :field,
    :source_uri,
    :stated_at,
    :uri,
    :value
  ]

  @primary_key {:uri, Uri.Ecto, autogenerate: false}
  schema "facts" do
    field(:entity_uri, Uri.Ecto)
    field(:source_uri, Uri.Ecto)
    field(:field, :string)
    field(:value, :string)
    field(:stated_at, :naive_datetime)
  end

  @spec from_model(Fact.t()) :: {:ok, Ecto.Changeset.t()}
  def from_model(fact) do
    fields = Fact.to_map(fact)

    changeset =
      %__MODULE__{}
      |> cast(fields, @fields)
      |> validate_required(@fields)

    {:ok, changeset}
  end

  @spec to_model(%__MODULE__{}) :: Fact.t()
  def to_model(%__MODULE__{} = f) do
    {:ok, fact} = Fact.from_map(f)
    fact
  end
end
