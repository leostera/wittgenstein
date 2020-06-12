import Config

config :starwars, disney: :true

config :starwars, ecto_repos: [Wittgenstein.Store.Postgres]

config :grpc, start_server: true

config :wittgenstein, Store, backend: Wittgenstein.Store.Postgres

config :wittgenstein, Wittgenstein.Store.Postgres,
  username: System.get_env("WITTGENSTEIN_POSTGRES_USER", "postgres"),
  password: System.get_env("WITTGENSTEIN_POSTGRES_PASSWORD", "admin"),
  database: System.get_env("WITTGENSTEIN_POSTGRES_DB", "starwars_metadata"),
  hostname: System.get_env("WITTGENSTEIN_POSTGRES_HOST", "localhost"),
  port: System.get_env("WITTGENSTEIN_POSTGRES_PORT", "32069"),
  pool_size: System.get_env("WITTGENSTEIN_POSTGRES_POOL_SIZE", "5") |> String.to_integer()

config :wittgenstein, Wittgenstein.Projection.Transport.MessageSender,
  projections: [
    Starwars.Printer
  ]

opentelemetry_host = System.get_env("OPENTELEMETRY_HOST", "localhost")
opentelemetry_port = System.get_env("OPENTELEMETRY_PORT", "9411")

config :opentelemetry, :processors,
  ot_batch_processor: %{
    exporter:
      {:opentelemetry_zipkin,
       %{
         address: 'http://#{opentelemetry_host}:#{opentelemetry_port}/api/v2/spans',
         local_endpoint: %{service_name: "StarWars"}
       }}
  }

