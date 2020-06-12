defmodule Wittgenstein.Store do
  alias Wittgenstein.Uri
  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  @callback persist_fact(Fact.t()) :: :ok | {:error, term()}
  @callback persist_entity(Entity.t()) :: :ok | {:error, term()}
  @callback fetch_entity(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}

  @spec persist_fact(Fact.t()) :: :ok | {:error, term()}
  def persist_fact(f), do: Config.store_backend().persist_fact(f)

  @spec persist_entity(Entity.t()) :: :ok | {:error, term()}
  def persist_entity(e), do: Config.store_backend().persist_entity(e)

  @spec fetch_entity(Uri.t()) :: {:ok, Entity.t()} | {:error, term()}
  def fetch_entity(uri), do: Config.store_backend().fetch_entity(uri)

  defmacro __using__(_opts \\ []) do
    quote do
      @behaviour Wittgenstein.Store
    end
  end
end
