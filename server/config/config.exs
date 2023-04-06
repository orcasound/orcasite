# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
import Config

config :phoenix, :json_library, Jason

# General application configuration
config :orcasite,
  ecto_repos: [Orcasite.Repo]

# Configures the endpoint
config :orcasite, OrcasiteWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: OrcasiteWeb.ErrorView, accepts: ~w(json)],
  pubsub_server: Orcasite.PubSub,
  live_view: [signing_salt: "pOIfxPskzkrWLv1xBPoT7Ot5S7h6Sz5f"]


# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :orcasite, Orcasite.Mailer, adapter: Swoosh.Adapters.Local

# Swoosh API client is needed for adapters other than SMTP.
config :swoosh, :api_client, false

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.14.29",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure reverse proxy to use HTTPoison as http client
config :reverse_proxy_plug, :http_client, ReverseProxyPlug.HTTPClient.Adapters.HTTPoison

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Configures Guardian
config :orcasite, OrcasiteWeb.Guardian,
  issuer: "orcasite",
  secret_key: "1Kw9VhSyQWzgZm+Tle5H3/5KkF8frUZpsOsGrhRv99Jbi6fUwQkRCHvWLRPzgtqe"

config :orcasite, env: config_env()

config :orcasite, OrcasiteWeb.Auth.AuthAccessPipeline,
  module: OrcasiteWeb.Guardian,
  error_handler: OrcasiteWeb.Auth.AuthErrorHandler

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
