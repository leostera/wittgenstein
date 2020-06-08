defmodule Wittgenstein.Consolidation.Strategy.LastFactWins do
  @behaviour Wittgenstein.Consolidation.Strategy
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  def consolidate(%Entity{} = entity, %Fact{} = new_fact) do
    entity
    |> Entity.values()
    |> Enum.map(fn
      old_fact ->
        with true <- Fact.field(new_fact) == Fact.field(old_fact),
             true <- Fact.stated_at(new_fact) > Fact.stated_at(old_fact) do
          new_fact
        else
          _ -> old_fact
        end
    end)
  end
end
