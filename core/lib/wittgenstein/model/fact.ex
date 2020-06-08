defmodule Wittgenstein.Model.Fact do
  defstruct [:stated_at, :uri, :source_uri, :entity_uri, :field, :value]

  @opaque t() :: %{
            required(:stated_at) => NaiveDateTime.t(),
            required(:uri) => Uri.t(),
            required(:source_uri) => Uri.t() | :none,
            required(:entity_uri) => Uri.t() | :none,
            required(:field) => binary() | :none,
            required(:value) => term() | :none
          }

  def is_valid?(%__MODULE__{
        stated_at: stated_at,
        uri: uri,
        source_uri: source_uri,
        entity_uri: entity_uri,
        field: field,
        value: value
      })
      when source_uri != :none and entity_uri != :none and field != :none and value != :none,
      do: true

  def is_valid?(_), do: false

  def new do
    %__MODULE__{
      stated_at: NaiveDateTime.utc_now(),
      uri: Uri.new(:wittgenstein, :fact),
      source_uri: :none,
      entity_uri: :none,
      field: :none,
      value: :none
    }
  end

  def stated_at(%__MODULE__{stated_at: x}), do: x
  def uri(%__MODULE__{uri: x}), do: x
  def source_uri(%__MODULE__{source_uri: x}), do: x
  def entity_uri(%__MODULE__{entity_uri: x}), do: x
  def field(%__MODULE__{field: x}), do: x
  def value(%__MODULE__{value: x}), do: x

  def set_stated_at(%__MODULE__{} = f, x), do: %{f | stated_at: x}
  def set_uri(%__MODULE__{} = f, x), do: %{f | uri: x}
  def set_source_uri(%__MODULE__{} = f, x), do: %{f | source_uri: x}
  def set_entity_uri(%__MODULE__{} = f, x), do: %{f | entity_uri: x}
  def set_field(%__MODULE__{} = f, x), do: %{f | field: x}
  def set_value(%__MODULE__{} = f, x), do: %{f | value: x}
end
