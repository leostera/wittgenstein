defmodule FactDB.MixProject do
  use Mix.Project

  def project do
    [
      app: :factdb,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :grpc, :brod, :cqerl],
      mod: {FactDB.Application, []}
    ]
  end

  defp deps do
    [
      {:brod, "~> 3.14.0"},
      {:cowlib, "~> 2.9.0", override: true},
      {:cqex, "~> 1.0.1"},
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0", only: :dev, runtime: false},
      {:google_protos, "~> 0.1"},
      {:grpc, "~> 0.5.0-beta.1"},
      {:jiffy, "~> 1.0"},
      {:opentelemetry, "~> 0.4.0"},
      {:opentelemetry_zipkin, "> 0.0.0"},
      {:protobuf, "~> 0.7.1"},
      {:snappyer, "1.2.6", override: true},
      {:uuid, "~> 2.0", hex: :uuid_erl}
    ]
  end
end
