defmodule Wittgenstein.Projection do
  alias Wittgenstein.Config

  def project(uri) do
    Config.Projection.projections()
    |> Stream.each(fn proj -> apply(proj, :project, [uri]) end)
  end
end
