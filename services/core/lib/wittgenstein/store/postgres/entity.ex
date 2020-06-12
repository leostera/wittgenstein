defmodule Wittgenstein.Store.Postgres.Entity do
  use Ecto.Schema
  import Ecto.Changeset

  alias Wittgenstein.Uri
  alias Wittgenstein.Model.Entity

  @fields [:uri, :namespace, :kind, :entity]

  @primary_key {:uri, Uri.Ecto, autogenerate: false}
  schema "entities" do
    field(:namespace, :string)
    field(:kind, :string)
    field(:entity, :map)
  end

  @spec from_model(Entity.t()) :: {:ok, Ecto.Changeset.t()}
  def from_model(entity) do
    fields = %{
      uri: entity |> Entity.uri(),
      namespace: entity |> Entity.uri() |> Uri.namespace() |> Atom.to_string(),
      kind: entity |> Entity.uri() |> Uri.kind() |> Atom.to_string(),
      entity: entity |> Entity.to_map()
    }

    changeset =
      %__MODULE__{}
      |> cast(fields, @fields)
      |> validate_required(@fields)

    {:ok, changeset}
  end

  @spec to_model(%__MODULE__{}) :: {:ok, Entity.t()}
  def to_model(%__MODULE__{entity: entity}) do
    {:ok, entity} = Entity.from_map(entity)
    {:ok, entity}
  end
end
