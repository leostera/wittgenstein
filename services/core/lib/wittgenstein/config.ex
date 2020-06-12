defmodule Wittgenstein.Config do
  def store_backend do
    Application.get_env(:wittgenstein, Store, [])
    |> Keyword.get(:backend, Wittgenstein.Store.InMemory)
  end

  def consolidation_strategy do
    Application.get_env(:wittgenstein, Consolidation, [])
    |> Keyword.get(:strategy, Wittgenstein.Consolidation.Strategy.LastFactWins)
  end

  def projection_transport_sender do
    Application.get_env(:wittgenstein, Projection, [])
    |> Keyword.get(:transport, Wittgenstein.Projection.Transport.MessageSender)
  end

  def projection_transport_receiver do
    Application.get_env(:wittgenstein, Projection, [])
    |> Keyword.get(:transport, Wittgenstein.Projection.Transport.MessageReceiver)
  end
end
