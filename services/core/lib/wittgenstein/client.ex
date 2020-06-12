defmodule Wittgenstein.Client do
  @moduledoc """

  The Wittgenstein Client

  """

  require Logger
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require OpenTelemetry.SpanUtils
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SpanUtils

  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity
  alias Wittgenstein.Store
  alias Wittgenstein.Consolidation
  alias Wittgenstein.Projection

  def state(%Fact{} = fact), do: state_one(fact)
  def state(facts), do: Stream.map(facts, &state_one/1)

  def state_one(%Fact{} = fact) do
    Tracer.with_span "wittgenstein.client.state_one" do
      SpanUtils.set_attributes(%{fact: fact |> Fact.to_map()})
    entity_uri = Fact.entity_uri(fact)
    :ok = Store.persist_fact(fact)

    entity =
      case Store.fetch_entity(entity_uri) do
        {:error, :uri_not_found} -> Entity.new(entity_uri)
        {:ok, entity} -> entity
      end

    {:ok, new_entity} = Consolidation.apply_fact(entity, fact)
    :ok = Store.persist_entity(new_entity)
    :ok = Projection.project(entity_uri)
    end
  end

  @spec lookup(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def lookup(uri) do
    Store.fetch_entity(uri)
  end

  def lookup!(uri) do
    {:ok, entity} = lookup(uri)
    entity
  end
end
