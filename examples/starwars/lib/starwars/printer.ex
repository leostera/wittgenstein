defmodule Starwars.Printer do
  use Wittgenstein.Projection

  alias Wittgenstein.Uri

  def handle_uri(uri) do
    "#{NaiveDateTime.utc_now()} - #{uri |> Uri.to_string()}"
    |> IO.inspect(label: __MODULE__ |> Atom.to_string())

    :ok
  end
end
