defmodule Wittgenstein.Store.Postgres.Migrations.CreateFactsTable do
  use Ecto.Migration

  def change do
    create table(:facts, primary_key: false) do
      add(:uri, :string, null: false, primary_key: true)
      add(:entity_uri, :string, null: false)
      add(:source_uri, :string, null: false)
      add(:field, :string, null: false)
      add(:value, :string, null: false)
      add(:stated_at, :naive_datetime, null: false)
    end
    create unique_index(:facts, [:uri])
  end
end
