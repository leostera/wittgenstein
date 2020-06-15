defmodule Wittgenstein.GRPC.MixProject do
  use Mix.Project

  def project do
    [
      app: :wittgenstein_grpc,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :grpc],
      mod: {Wittgenstein.GRPC.Application, []}
    ]
  end

  defp deps do
    [
      {:cowlib, "~> 2.9.0", override: true},
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0", only: :dev, runtime: false},
      {:google_protos, "~> 0.1"},
      {:grpc, github: "elixir-grpc/grpc"},
      {:opentelemetry, "~> 0.4.0"},
      {:protobuf, "~> 0.7.1"},
      {:wittgenstein, path: "../core"},
    ]
  end
end
