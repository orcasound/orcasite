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
  render_errors: [
    formats: [html: OrcasiteWeb.ErrorHTML, json: OrcasiteWeb.ErrorJSON],
    layout: false
  ],
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
  version: "0.18.20",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :tailwind,
  version: "3.2.7",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
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

config :orcasite, :ecto_repos, [Orcasite.Repo]
config :orcasite, :ash_domains, [Orcasite.Notifications, Orcasite.Accounts, Orcasite.Radio]
config :orcasite, :ash_uuid, migration_default?: true
config :ash, :custom_types, geometry: Orcasite.Types.Geometry
config :ash_graphql, :default_managed_relationship_type_name_template, :action_name
config :ash_graphql, :json_type, :json

config :mime, :types, %{
  "application/vnd.api+json" => ["json"]
}

config :mime, :extensions, %{
  "json" => "application/json"
}

config :orcasite, Oban,
  repo: Orcasite.Repo,
  # 7 day job retention
  plugins: [{Oban.Plugins.Pruner, max_age: 7 * 24 * 60 * 60}],
  queues: [default: 10, email: 10, feeds: 10, audio_images: 20]

config :spark, :formatter,
  remove_parens?: true,
  "Ash.Resource": [
    type: Ash.Resource,
    section_order: [
      :resource,
      :postgres,
      :identities,
      :attributes,
      :calculations,
      :aggregates,
      :relationships,
      :authentication,
      :token,
      :policies,
      :field_policies,
      :actions,
      :admin,
      :json_api,
      :graphql
    ]
  ]

config :ex_aws,
  region: "us-west-2"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
