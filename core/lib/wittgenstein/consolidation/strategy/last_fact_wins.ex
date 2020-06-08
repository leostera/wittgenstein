defmodule Wittgenstein.Consolidation.Strategy.LastFactWins do
  @behaviour Wittgenstein.Consolidation.Strategy
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  def consolidate(%Entity{} = entity, %Fact{} = new_fact) do
    old_values = entity |> Entity.values()

    old_fact = old_values |> Enum.find(fn f -> Fact.field(f) == Fact.field(new_fact) end)

    new_values =
      case old_fact do
        nil ->
          [new_fact | old_values]

        _ ->
          Enum.map(old_values, fn
            old_fact ->
              case Fact.field(old_fact) == Fact.field(new_fact) and
                     Fact.stated_at(new_fact) > Fact.stated_at(old_fact) do
                true -> new_fact
                _ -> old_fact
              end
          end)
      end
          |> IO.inspect()

    {:ok, entity |> Entity.set_values(new_values)}
  end
end
