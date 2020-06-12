defmodule Wittgenstein.MixProject do
  use Mix.Project

  def project do
    [
      app: :wittgenstein,
      version: "0.0.2",
      description: "A semantic, real-time, distributed, knowledge database.",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "Wittgenstein",
      source_url: "https://github.com/AbstractMachinesLab/wittgenstein",
      homepage_url: "https://abstractmachines.dev/wittgenstein",
      aliases: aliases()
    ]
  end

  def aliases do
    [
      check: [
        "dialyzer --halt-exit-status",
        "credo --strict",
        "format --check-formatted --dry-run",
        "hex.audit",
        "sobelow --config",
      ]
    ]
  end

  def application do
    [
      mod: {Wittgenstein.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0", only: :dev, runtime: false},
      {:ecto, "~> 3.4"},
      {:ecto_sql, "~> 3.0"},
      {:jason, "~> 1.1"},
      {:opentelemetry, "~> 0.4.0"},
      {:postgrex, ">= 0.0.0"},
      {:propcheck, "~> 1.2.0", only: [:dev, :test]},
      {:uuid, "~> 1.1"}
    ]
  end
end
