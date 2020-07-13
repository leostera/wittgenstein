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
  alias Dev.Abstractmachines.Wittgenstein.ProjectDescription
  alias Dev.Abstractmachines.Wittgenstein.ProjectedEntity

  def project(_proj_desc, stream) do
    Tracer.with_span "factdb.grpc.project" do
      :ok = FactDB.Client.subscribe_to_projection()

      poll = fn poll_fn, count ->
        receive do
          {:entity, entity} ->
            uri = entity["uri"]
            reply = ProjectedEntity.new(entity_uri: uri, fields: entity)
            GRPC.Server.send_reply(stream, reply)
            poll_fn.(poll_fn, count + 1)
        after
          1000 ->
            poll_fn.(poll_fn, count)
        end
      end

      poll.(poll, 0)
    end
  rescue
    error -> IO.inspect("projection error: #{inspect(error)}")
  end

  def state_facts(facts, stream) do
    Tracer.with_span "factdb.grpc.state_facts" do
      fact_count =
        facts
        |> Stream.map(fn req ->
          Fact.new()
          |> Fact.set_entity_uri(req.entity_uri)
          |> Fact.set_field_uri(req.field_uri)
          |> Fact.set_source_uri(req.source_uri)
          |> Fact.set_value(req.value)
        end)
        |> Stream.chunk_every(50)
        |> Stream.flat_map(fn bucket ->
          case FactDB.Client.state(bucket) do
            {:ok, uris} ->
              uris
              |> Enum.map(fn uri -> StateFactReply.new(response: {:fact_uri, uri}) end)

            {:error, reason} ->
              StateFactReply.new(response: [error: reason |> IO.inspect()])
          end
        end)
        |> Stream.map(fn reply -> GRPC.Server.send_reply(stream, reply) end)
        |> Enum.count()

      IO.inspect("Processed #{fact_count} facts")
    end
  end

  def state_fact(req, _stream) do
    Tracer.with_span "factdb.grpc.state_fact" do
      SpanUtils.set_attributes(%{request: req |> Map.from_struct()})

      fact =
        Fact.new()
        |> Fact.set_entity_uri(req.entity_uri)
        |> Fact.set_field_uri(req.field_uri)
        |> Fact.set_source_uri(req.source_uri)
        |> Fact.set_value(req.value)

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
