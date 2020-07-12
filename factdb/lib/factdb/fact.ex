defmodule FactDB.Fact do
  defdelegate new(), to: :fdb_fact

  defdelegate set_entity_uri(_fact, _entity_uri), to: :fdb_fact
  defdelegate set_field_uri(_fact, _field_uri), to: :fdb_fact
  defdelegate set_source_uri(_fact, _source_uri), to: :fdb_fact
  defdelegate set_value(_fact, _value), to: :fdb_fact

  defdelegate entity_uri(_fact), to: :fdb_fact
  defdelegate field_uri(_fact), to: :fdb_fact
  defdelegate source_uri(_fact), to: :fdb_fact
  defdelegate uri(_fact), to: :fdb_fact
  defdelegate value(_fact), to: :fdb_fact
end
