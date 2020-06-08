defmodule Starwars.Printer do

  @behaviour Wittgenstein.Projection

  @impl true
  def project(uri) do
    uri
    |> Uri.to_string()
    |> IO.inspect()
  end
end
