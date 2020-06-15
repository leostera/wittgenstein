defmodule Wittgenstein.Store.InMemory do
  use GenServer

  alias Wittgenstein.Uri
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @behaviour Wittgenstein.Store

  @spec persist_facts([Fact.t()]) :: :ok | {:error, term()}
  def persist_facts(f), do: GenServer.call(__MODULE__, persist_facts: f)

  @spec persist_entity(Entity.t()) :: :ok | {:error, term()}
  def persist_entity(e), do: GenServer.call(__MODULE__, persist_entity: e)

  @spec fetch_entity(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def fetch_entity(uri), do: GenServer.call(__MODULE__, fetch_entity: uri)

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    ets_opts = [:named_table]
    facts = :ets.new(:wittgenstein_store_inmemory_facts, ets_opts)
    entities = :ets.new(:wittgenstein_store_inmemory_entities, ets_opts)
    {:ok, %{facts: facts, entities: entities}}
  end

  def handle_call([persist_facts: f], _, s), do: do_persist_facts(f, s)
  def handle_call([persist_entity: e], _, s), do: do_persist_entity(e, s)
  def handle_call([fetch_entity: uri], _, s), do: do_fetch_entity(uri, s)

  def do_persist_facts(new_facts, %{facts: facts} = state) do
    new_facts
    |> Enum.each(fn fact ->
      uri = fact |> Fact.uri()
      :ets.insert(facts, {uri, fact})
    end)

    {:reply, :ok, state}
  end

  def do_persist_entity(entity, %{entities: entities} = state) do
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
