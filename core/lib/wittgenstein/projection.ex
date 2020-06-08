defmodule Wittgenstein.Projection do
  alias Wittgenstein.Config

  @callback project(Uri.t()) :: :ok | {:error, term()}

  def project(uri) do
    Config.projections()
    |> Stream.map(fn proj -> apply(proj, :project, [uri]) end)
    |> Enum.to_list()

    :ok
  end
end
