defmodule Wittgenstein.Config do
  def store_backend do
    Application.get_env(:wittgenstein, Store, [])
    |> Keyword.get(:backend, Wittgenstein.Store.InMemory)
  end

  def consolidation_strategy do
    Application.get_env(:wittgenstein, Consolidation, [])
    |> Keyword.get(:strategy, Wittgenstein.Consolidation.Strategy.LastFactWins)
  end

  def projections do
    Application.get_env(:wittgenstein, Projection, [])
    |> Keyword.get(:projections, [])
  end
end
