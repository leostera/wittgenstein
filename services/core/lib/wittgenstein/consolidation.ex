defmodule Wittgenstein.Consolidation do
  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @spec apply_fact(Entity.t(), Fact.t()) :: {:ok, Entity.t()} | {:error, term()}
  def apply_fact(e, f) do
    Config.consolidation_strategy().consolidate(e, f)
  end
end
