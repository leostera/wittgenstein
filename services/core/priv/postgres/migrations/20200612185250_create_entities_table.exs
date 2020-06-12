defmodule Wittgenstein.Store.Postgres.Migrations.CreateEntitiesTable do
  use Ecto.Migration

  def change do
    create table(:entities, primary_key: false) do
      add(:uri, :string, null: false, primary_key: true)
      add(:namespace, :string, null: false)
      add(:kind, :string, null: false)
      add(:entity, :jsonb, null: false)
    end
    create unique_index(:entities, [:uri])
  end
end
