import Config

# Evaluated when the release is built, so nothing here may depend on the
# environment of the app that runs it: read those values in runtime.exs.
config :orcasite, OrcasiteWeb.Endpoint, force_ssl: [rewrite_on: [:x_forwarded_proto]]

# Configure your database
config :orcasite, Orcasite.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  types: Orcasite.PostgresTypes,
  ssl_opts: [
    verify: :verify_none
  ]

# Do not print debug messages in production
config :logger, level: :info, format: {Orcasite.Logger, :format}

config :orcasite, Orcasite.Mailer,
  adapter: Swoosh.Adapters.AmazonSES,
  region: "us-west-2"

config :swoosh, api_client: Swoosh.ApiClient.Finch, finch_name: Orcasite.Finch

config :orcasite, OrcasiteWeb.Guardian, issuer: "orcasite"

config :orcasite, :env, :prod
