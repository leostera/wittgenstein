defmodule WittgensteinAdmin.Repo do
  use Ecto.Repo,
    otp_app: :wittgenstein_admin,
    adapter: Ecto.Adapters.Postgres
end
