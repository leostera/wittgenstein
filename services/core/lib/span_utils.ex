defmodule OpenTelemetry.SpanUtils do
  defmacro set_attributes(map) do
    quote do
      unquote(map)
      |> OpenTelemetry.SpanUtils.map_to_paths()
      |> Enum.map(fn {k, v} ->
        OpenTelemetry.Span.set_attribute(
          k,
          v |> OpenTelemetry.SpanUtils.string() |> :unicode.characters_to_binary()
        )
      end)
    end
  end

  def string(x) when is_binary(x), do: x
  def string(x) when is_atom(x), do: Atom.to_string(x)
  def string(x) when is_integer(x), do: Integer.to_string(x)
  def string(x) when is_float(x), do: Float.to_string(x)

  def map_to_paths(m) when is_map(m), do: m |> Map.to_list() |> map_to_paths([])

  def map_to_paths([], acc), do: acc

  def map_to_paths([{key, %NaiveDateTime{} = value} | ms], acc) do
    map_to_paths(ms, [{key, NaiveDateTime.to_iso8601(value)} | acc])
  end

  def map_to_paths([{key, value} | ms], acc) when is_list(value) do
    map_to_paths(
      ms,
      value
      |> Enum.with_index()
      |> Enum.map(fn {v, i} when is_map(v)->
        v
        |> map_to_paths()
        |> Enum.map(fn {k, v2} -> {"#{key}.#{i}.#{k}", v2} end)

        {v, i} -> {"#{key}.#{i}", v}
      end)
      |> List.flatten()
      |> Enum.concat(acc)
    )
  end

  def map_to_paths([{key, value} | ms], acc) when is_map(value) do
    map_to_paths(
      ms,
      value
      |> map_to_paths()
      |> Enum.map(fn {k, v} -> {"#{key}.#{k}", v} end)
      |> Enum.concat(acc)
    )
  end

  def map_to_paths([{key, value} | ms], acc) do
    map_to_paths(ms, [{key |> to_string(), value} | acc])
  end
end
