defmodule Wittgenstein.Store.Postgres do
  require Logger
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require OpenTelemetry.SpanUtils
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.Span
  alias OpenTelemetry.SpanUtils

  use Ecto.Repo, otp_app: :wittgenstein, adapter: Ecto.Adapters.Postgres
  use Wittgenstein.Store

  alias Wittgenstein.Model.Entity
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Store.Postgres
  alias Wittgenstein.Uri

  @spec persist_facts([Fact.t()]) :: :ok | {:error, term()}
  def persist_facts(facts) do
    Tracer.with_span "wittgenstein.store.postgres.persist_fact" do
      records =
        facts
        |> Enum.map(fn fact ->
          {:ok, fact_record} = fact |> Postgres.Fact.from_model() |> IO.inspect()
          fact_record
        end)

      {fact_count, _} = __MODULE__.insert_all(Postgres.Fact, records)

      Span.set_attribute(:fact_count, fact_count |> Integer.to_string())

      :ok
    end
  end

  @spec persist_entity(Entity.t()) :: :ok | {:error, term()}
  def persist_entity(entity) do
    Tracer.with_span "wittgenstein.store.postgres.persist_entity" do
      SpanUtils.set_attributes(%{entity: entity |> Entity.to_map()})

      {:ok, entity_record} = entity |> Postgres.Entity.from_model()

      {:ok, _} =
        __MODULE__.insert(entity_record,
          on_conflict: {:replace, [:entity]},
          conflict_target: :uri
        )

      :ok
    end
  end

  @spec fetch_entity(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def fetch_entity(uri) do
    Tracer.with_span "wittgenstein.store.postgres.fetch_entity" do
      SpanUtils.set_attributes(%{uri: uri |> Uri.to_string()})

      case __MODULE__.get(Postgres.Entity, uri, []) do
        nil -> {:error, :uri_not_found}
        entity -> entity |> Postgres.Entity.to_model()
      end
    end
  end
end
