import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :orcasite, OrcasiteWeb.Endpoint,
  http: [port: 4001],
  server: false,
  secret_key_base: "99+mOEo5AOlhlQi4sJJd5MWnAdFFzm64",
  check_origin: false

# Print only warnings and errors during test
config :logger, level: :warning

# Configure your database
config :orcasite, Orcasite.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "orcasite_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox,
  types: Orcasite.PostgresTypes,
  pool_size: 10

config :orcasite, Orcasite.Mailer, adapter: Swoosh.Adapters.Test
config :orcasite, Oban, testing: :inline

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

config :orcasite, :cache_adapter, Nebulex.Adapters.Local

config :hammer,
  backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 4, cleanup_interval_ms: 60_000 * 10]}
config :orcasite, OrcasiteWeb.BasicAuth, username: "admin", password: "password"
config :ash_graphql, :policies, show_policy_breakdowns?: true
config :orcasite, Orcasite.Radio, graphql: [show_raised_errors?: true]
config :orcasite, Orcasite.Accounts, graphql: [show_raised_errors?: true]
