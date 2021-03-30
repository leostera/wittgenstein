defmodule FactDB.Client do
  defdelegate state(_facts), to: :factdb
  defdelegate entity_by_uri(_uri), to: :factdb
  defdelegate subscribe_to_projection(), to: :factdb
end
