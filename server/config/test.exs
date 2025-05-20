import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :orcasite, OrcasiteWeb.Endpoint,
  http: [port: 4001],
  server: false,
  secret_key_base: "W6Mk5RQWbSDaeBuP6qeNZa+e/dA+Qu6te63CstikXctm1m7QrV4XwOM7oUdo/1z7",
  check_origin: false

# Print only warnings and errors during test
config :logger, level: :warning

# Configure your database
config :orcasite, Orcasite.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: System.get_env("POSTGRES_USER") || "postgres",
  password: System.get_env("POSTGRES_PASSWORD") || "postgres",
  database:
    System.get_env("POSTGRES_DATABASE") || "orcasite_test#{System.get_env("MIX_TEST_PARTITION")}",
  port: System.get_env("POSTGRES_PORT") || 5432,
  hostname: System.get_env("POSTGRES_HOST") || "localhost",
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
config :ash, :pub_sub, debug?: true
config :orcasite, Orcasite.Radio, graphql: [show_raised_errors?: true]
config :orcasite, Orcasite.Accounts, graphql: [show_raised_errors?: true]

config :ash, :disable_async?, true
config :ash, :missed_notifications, :ignore

config :ex_aws,
  access_key_id: [{:system, "AWS_ACCESS_KEY_ID"}, {:awscli, "orcasound", 1000}, :instance_role],
  secret_access_key: [
    {:system, "AWS_SECRET_ACCESS_KEY"},
    {:awscli, "orcasound", 1000},
    :instance_role
  ],
  region: "us-west-2"

config :orcasite, :env, :test
