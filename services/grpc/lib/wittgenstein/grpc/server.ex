defmodule Wittgenstein.GRPC.Server do
  require Logger
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require OpenTelemetry.SpanUtils
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SpanUtils

  use GRPC.Server, service: Dev.Abstractmachines.Wittgenstein.Wittgenstein.Service

  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Uri

  alias Dev.Abstractmachines.Wittgenstein.StateFactRequest
  alias Dev.Abstractmachines.Wittgenstein.StateFactReply

  @spec state_fact(StateFactRequest.t(), GRPC.Server.Stream.t()) :: StateFactReply.t()
  def state_fact(req, _stream) do
    Tracer.with_span "wittgenstein.grpc.state_fact" do
      SpanUtils.set_attributes(%{ request: req |> Map.from_struct() })

      fact = Fact.new()
             |> Fact.set_entity_uri(req.entity_uri |> Uri.from_string!())
             |> Fact.set_value(req.value)
             |> Fact.set_field(req.field)
             |> Fact.set_source_uri(req.source_uri |> Uri.from_string!())

      case Wittgenstein.Client.state(fact) do
        :ok ->
          StateFactReply.new(response: { :fact_uri, fact |> Fact.uri() |> Uri.to_string() })

        {:error, reason} ->
          SpanUtils.set_attributes(%{ error: reason })
          StateFactReply.new(response: [error: reason |> inspect()])
      end

    end
  end

end
