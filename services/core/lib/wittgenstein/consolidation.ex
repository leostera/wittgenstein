defmodule Wittgenstein.Consolidation do
  alias Wittgenstein.Config
  alias Wittgenstein.Model.Fact
  alias Wittgenstein.Model.Entity

  def apply_fact(%Entity{} = e, %Fact{} = f) do
    Config.consolidation_strategy().consolidate(e, f)
  end
end
