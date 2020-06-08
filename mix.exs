defmodule Wittgenstein.MixProject do
  use Mix.Project

  def project do
    [
      app: :wittgenstein,
      version: "0.0.1",
      description: "A semantic, real-time, distributed, knowledge database.",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "Wittgenstein",
      source_url: "https://github.com/AbstractMachinesLab/wittgenstein",
      homepage_url: "https://abstractmachines.dev/wittgenstein"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    []
  end
end
