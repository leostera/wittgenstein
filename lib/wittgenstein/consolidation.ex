defmodule Wittgenstein.Consolidation do
  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  def apply_fact(%Entity{} = e, %Fact{} = f) do
    Config.Consolidation.strategy().consolidate(e, f)
  end
end
