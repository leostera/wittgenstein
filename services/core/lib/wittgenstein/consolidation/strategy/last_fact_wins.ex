defmodule Wittgenstein.Consolidation.Strategy.LastFactWins do
  use Wittgenstein.Consolidation.Strategy

  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @spec consolidate(Entity.t(), [Fact.t()]) :: {:ok, Entity.t()} | {:error, term()}
  def consolidate(entity, new_facts) do
    entity =
      Enum.reduce(new_facts, entity, fn
        new_fact, entity ->
          case Entity.uri(entity) == Fact.entity_uri(new_fact) do
            true -> do_consolidate(entity, new_fact)
            false -> entity
          end
      end)

    {:ok, entity}
  end

  defp do_consolidate(entity, new_fact) do
    old_values = entity |> Entity.values()

    old_fact = old_values |> Enum.find(fn f -> Fact.field(f) == Fact.field(new_fact) end)

    new_values =
      case old_fact do
        nil ->
          [new_fact | old_values]

        _ ->
          replace_old_fact_with_new(old_values, new_fact)
      end

    entity |> Entity.set_values(new_values)
  end

  defp replace_old_fact_with_new(old_values, new_fact) do
    Enum.map(old_values, fn
      old_fact ->
        case Fact.field(old_fact) == Fact.field(new_fact) and
               Fact.stated_at(new_fact) > Fact.stated_at(old_fact) do
          true -> new_fact
          _ -> old_fact
        end
    end)
  end
end
