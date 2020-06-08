defmodule Wittgenstein.Consolidation.Strategy do
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @callback consolidate(Entity.t(), Fact.t()) ::
              {:ok, Entity.t()} | {:error, term()}
end
