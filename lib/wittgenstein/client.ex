defmodule Wittgenstein.Client do
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity
  alias Wittgenstein.Store
  alias Wittgenstein.Consolidation
  alias Wittgenstein.Projection

  def state(%Fact{} = fact) do
    entity_uri = Fact.entity_uri(fact)
    :ok = Store.persist_fact(fact)
    {:ok, entity} = Store.fetch_entity(entity_uri)
    {:ok, entity} = Consolidation.apply_fact(entity, fact)
    :ok = Store.persist_entity(entity)
    :ok = Projection.project(entity_uri)
  end
end
