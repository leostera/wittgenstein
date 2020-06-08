defmodule Wittgenstein.Model.Entity do
  alias Wittgenstein.Model.Fact

  defstruct [:uri, :kind, :values]

  @opaque t() :: %{
            required(:uri) => Uri.t(),
            required(:kind) => Atom.t(),
            required(:values) => [Fact.t()]
          }

  def uri(%__MODULE__{uri: x}), do: x
  def kind(%__MODULE__{kind: x}), do: x
  def values(%__MODULE__{values: x}), do: x

  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}
  def set_values(%__MODULE__{} = f, x) when is_list(x), do: %{f | values: x}
end
