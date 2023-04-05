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
  secret_key_base: "ZaTk5BBbg4BWCa+zQ0rjJxr9T5WqSEUt3oS0bd1Ct1SOFQg1HgBjPJaGffsNXZU3",
  render_errors: [view: OrcasiteWeb.ErrorView, accepts: ~w(json)],
  pubsub_server: Orcasite.PubSub,
  live_view: [signing_salt: "pOIfxPskzkrWLv1xBPoT7Ot5S7h6Sz5f"]

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

config :orcasite, env: Mix.env()

config :orcasite, OrcasiteWeb.Auth.AuthAccessPipeline,
  module: OrcasiteWeb.Guardian,
  error_handler: OrcasiteWeb.Auth.AuthErrorHandler

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
