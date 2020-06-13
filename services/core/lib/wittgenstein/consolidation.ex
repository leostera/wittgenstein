defmodule Wittgenstein.Consolidation do
  @moduledoc """

  Consolidation is the process of pretty much smushing a new fact atop an
  existing entity.

  This process can be done in several different ways, so you can define the
  strategy by which two conflicting facts will be resolved.

  """

  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @spec apply_facts(Entity.t(), [Fact.t()]) :: {:ok, Entity.t()} | {:error, term()}
  def apply_facts(entity, facts) do
    apply_facts_with_strategy(Config.consolidation_strategy(), entity, facts)
  end

  @spec apply_facts_with_strategy(atom(), Entity.t(), [Fact.t()]) ::
          {:ok, Entity.t()} | {:error, term()}
  def apply_facts_with_strategy(strategy, entity, facts) do
    strategy.consolidate(entity, facts)
  end
end
