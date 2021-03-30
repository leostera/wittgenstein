defmodule FactDB.GRPC.Server do
  require Logger
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require OpenTelemetry.SpanUtils
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SpanUtils

  use GRPC.Server, service: Dev.Abstractmachines.Wittgenstein.FactDB.Service

  alias FactDB.Fact

  alias Dev.Abstractmachines.Wittgenstein.StateFactRequest
  alias Dev.Abstractmachines.Wittgenstein.StateFactReply
  alias Dev.Abstractmachines.Wittgenstein.StreamedStateFactReply

  def state_facts(facts, stream) do
    Tracer.with_span "factdb.grpc.state_facts" do
      fact_count =
        facts
        |> Stream.map(&req_to_fact/1)
        |> Stream.chunk_every(1000)
        |> Stream.map(&FactDB.Client.state/1)
        |> Stream.map(
          fn {:ok, uris} -> Enum.count(uris)
            error ->
              Logger.error(error)
              0
          end
        )
        |> Enum.reduce(0, &(&1 + &2))

      IO.inspect("#{NaiveDateTime.utc_now()} - Processed #{fact_count} facts")
      StreamedStateFactReply.new(fact_count: fact_count)
    end
  end

  defp req_to_fact(req) do
    Fact.new()
    |> Fact.set_entity_uri(req.entity_uri)
    |> Fact.set_field_uri(req.field_uri)
    |> Fact.set_source_uri(req.source_uri)
    |> Fact.set_value(req.value)
  end

  def state_fact(req, _stream) do
    Tracer.with_span "factdb.grpc.state_fact" do
      SpanUtils.set_attributes(%{request: req |> Map.from_struct()})

      fact = req_to_fact(req)

      case FactDB.Client.state([fact]) do
        {:ok, [_]} ->
          StateFactReply.new(response: {:fact_uri, fact |> Fact.uri()})

        {:error, reason} ->
          SpanUtils.set_attributes(%{error: reason})
          StateFactReply.new(response: [error: reason |> inspect()])
      end
    end
  end
end
