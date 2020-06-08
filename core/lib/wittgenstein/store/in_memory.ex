defmodule Wittgenstein.Store.InMemory do
  use GenServer

  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @behaviour Wittgenstein.Store

  @spec persist_fact(Fact.t()) :: :ok | {:error, term()}
  def persist_fact(f), do: GenServer.call(__MODULE__, persist_fact: f)

  @spec persist_entity(Entity.t()) :: :ok | {:error, term()}
  def persist_entity(e), do: GenServer.call(__MODULE__, persist_entity: e)

  @spec fetch_entity(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def fetch_entity(uri), do: GenServer.call(__MODULE__, fetch_entity: uri)

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(opts) do
    ets_opts = [:named_table]
    facts = :ets.new(:wittgenstein_store_inmemory_facts, ets_opts)
    entities = :ets.new(:wittgenstein_store_inmemory_entities, ets_opts)
    {:ok, %{facts: facts, entities: entities}}
  end

  def handle_call([persist_fact: f], _, s), do: do_persist_fact(f, s)
  def handle_call([persist_entity: e], _, s), do: do_persist_entity(e, s)
  def handle_call([fetch_entity: uri], _, s), do: do_fetch_entity(uri, s)

  def do_persist_fact(%Fact{} = fact, %{facts: facts} = state) do
    uri = fact |> Fact.uri()
    :ets.insert(facts, {uri, fact})
    {:reply, :ok, state}
  end

  def do_persist_entity(%Entity{} = entity, %{entities: entities} = state) do
    uri = entity |> Entity.uri()
    :ets.insert(entities, {uri, entity})
    {:reply, :ok, state}
  end

  def do_fetch_entity(uri, %{entities: entities} = state) do
    res =
      case :ets.lookup(entities, uri) do
        [] -> {:error, :no_entity_found_with_uri}
        [{_, e}] -> {:ok, e}
      end

    {:reply, res, state}
  end
end
