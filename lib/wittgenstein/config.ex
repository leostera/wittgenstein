defmodule Wittgenstein.Config do
  defmodule Store do
    def backend,
      do: Application.get_env(Wittgenstein, Store)[:backend] || Wittgenstein.Store.InMemory
  end

  defmodule Consolidation do
    def strategy,
      do:
        Application.get_env(Wittgenstein, Consolidation)[:strategy] ||
          Wittgenstein.Consolidation.Strategy.LastFactWins
  end

  defmodule Projection do
    def projections,
      do:
        Application.get_env(Wittgenstein, Consolidation)[:projections] ||
          []
  end
end
