defmodule Wittgenstein.Client do
  @moduledoc """

  The Wittgenstein Client

  """

  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity
  alias Wittgenstein.Store
  alias Wittgenstein.Consolidation
  alias Wittgenstein.Projection

  def state(%Fact{} = fact), do: state_one(fact)
  def state([%Fact{} | _] = facts), do: Enum.map(facts, &state_one/1)

  def state_one(%Fact{} = fact) do
    entity_uri = Fact.entity_uri(fact)
    :ok = Store.persist_fact(fact)

    entity =
      case Store.fetch_entity(entity_uri) do
        {:error, :no_entity_found_with_uri} -> Entity.new(entity_uri)
        {:ok, entity} -> entity
      end

    {:ok, new_entity} = Consolidation.apply_fact(entity, fact)
    :ok = Store.persist_entity(new_entity)
    :ok = Projection.project(entity_uri)
  end

  @spec lookup(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def lookup(uri) do
    Store.fetch_entity(uri)
  end
end
