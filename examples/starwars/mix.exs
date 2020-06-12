defmodule Starwars.MixProject do
  use Mix.Project

  def project do
    [
      app: :starwars,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :opentelemetry, :opentelemetry_zipkin, :wittgenstein],
      mod: {Starwars.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:wittgenstein, path: "../../services/core"},
      {:opentelemetry, "~> 0.4.0"},
      {:opentelemetry_zipkin, ">= 0.0.0"},
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
