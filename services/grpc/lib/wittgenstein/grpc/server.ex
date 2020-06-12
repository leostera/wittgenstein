defmodule Wittgenstein.GRPC.Server do
  require Logger
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require OpenTelemetry.SpanUtils
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SpanUtils

  use GRPC.Server, service: Dev.Abstractmachines.Wittgenstein.Wittgenstein.Service

  alias Dev.Abstractmachines.Wittgenstein.Wittgenstein

  alias Wittgenstein.Model.Fact

  @spec state_fact(Wittgenstein.StateFactRequest.t(), GRPC.Server.Stream.t()) :: Wittgenstein.StateFactReply.t()
  def state_fact(req, _stream) do
    Tracer.with_span "wittgenstein.grpc.state_fact" do
      SpanUtils.set_attributes(%{ request: req |> Map.from_struct() })

      fact = Fact.new()
             |> Fact.set_entity_uri(req.entity_uri)
             |> Fact.set_value(req.value)
             |> Fact.set_field(req.field)
             |> Fact.set_source_uri(req.source_uri)

      case Wittgenstein.Client.state(fact) do
        :ok ->
          Wittgenstein.StateFactReply.new(fact_uri: fact |> Fact.uri() |> Uri.to_string())

        {:error, reason} ->
          SpanUtils.set_attributes(%{ error: reason })
          Wittgenstein.StateFactReply.new(error: reason |> inspect())
      end

    end
  end

end
