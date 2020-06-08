defmodule Starwars do

  alias Wittgenstein.Model.Fact

  def test_fact do
    system_uri = Uri.new(:abstractmachines, :system)
    luke_uri = Uri.new(:disney, :character)
    [
      Fact.new
      |> Fact.set_source_uri(system_uri)
      |> Fact.set_entity_uri(luke_uri)
      |> Fact.set_field("com.disney.films.starwars.character.name")
      |> Fact.set_value("Luke"),

      Fact.new
      |> Fact.set_source_uri(system_uri)
      |> Fact.set_entity_uri(luke_uri)
      |> Fact.set_field("com.disney.films.starwars.character.midiclorian_levels")
      |> Fact.set_value("high"),
    ]
  end
end
