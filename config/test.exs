use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :orcasite, OrcasiteWeb.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :orcasite, Orcasite.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: System.get_env("POSTGRES_USER") || "postgres",
  password: System.get_env("POSTGRES_PASSWORD") || "postgres",
  database: "orcasite_test",
  hostname: System.get_env("POSTGRES_HOST") || "localhost",
  port: System.get_env("POSTGRES_PORT") || 5432,
  pool: Ecto.Adapters.SQL.Sandbox,
  types: Orcasite.PostgresTypes

# Configures the test mail adapter
config :orcasite, Orcasite.Mailer,
  adapter: Bamboo.TestAdapter