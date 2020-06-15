defmodule Wittgenstein.Config do
  @moduledoc """

  Configuration module.

  Every relevant piece of runtime configuration should be abstracted by a
  function in or a submodule to this module.

  """

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
