defmodule Starwars do
  alias Wittgenstein.Uri
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  def one_fact do
    system_uri = Uri.new(:abstractmachines, :system)
    luke_uri = Uri.new(:disney, :character)

    Fact.new()
    |> Fact.set_source_uri(system_uri)
    |> Fact.set_entity_uri(luke_uri)
    |> Fact.set_field("com.disney.films.starwars.character.name")
    |> Fact.set_value("Luke")
  end

  def one_entity do
    fact = one_fact()

    {:ok, entity} =
      Wittgenstein.Model.Entity.new(fact |> Fact.entity_uri())
      |> Wittgenstein.Consolidation.apply_fact(fact)

    entity
  end

  def map_around do
    one_entity() |> Entity.to_map() |> Entity.from_map()
  end

  def state_stuff do
    IO.inspect("#{NaiveDateTime.utc_now()} begin!")
    test_fact() |> Wittgenstein.Client.state() |> Enum.to_list()
  end

  def test_fact do
    system_uri = Uri.new(:abstractmachines, :system)
    luke_uri = Uri.new(:disney, :character)

    1..100
    |> Stream.flat_map(fn _ ->
      [
        Fact.new()
        |> Fact.set_source_uri(system_uri)
        |> Fact.set_entity_uri(luke_uri)
        |> Fact.set_field("com.disney.films.starwars.character.name")
        |> Fact.set_value("Luke"),
        Fact.new()
        |> Fact.set_source_uri(system_uri)
        |> Fact.set_entity_uri(luke_uri)
        |> Fact.set_field("com.disney.films.starwars.character.midiclorian_levels")
        |> Fact.set_value("high")
      ]
    end)
  end
end
