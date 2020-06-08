defmodule Wittgenstein.Model.Entity do
  alias Wittgenstein.Model.Fact

  defstruct [:uri, :kind, :values]

  @opaque t() :: %{
            required(:uri) => Uri.t(),
            required(:kind) => Atom.t(),
            required(:values) => [Fact.t()]
          }

  def new(uri) do
    %__MODULE__{
      uri: uri,
      kind: uri |> Uri.kind(),
      values: []
    }
  end

  def uri(%__MODULE__{uri: x}), do: x
  def kind(%__MODULE__{kind: x}), do: x
  def values(%__MODULE__{values: x}), do: x

  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}
  def set_values(%__MODULE__{} = f, x) when is_list(x), do: %{f | values: x}

  def to_map(%__MODULE__{uri: uri, kind: kind, values: values }) do
    values
    |> Enum.map(fn fact -> { Fact.field(fact), Fact.value(fact) } end)
    |> Map.new()
    |> Map.merge(%{ "@uri" => uri |> Uri.to_string() })
  end
end
