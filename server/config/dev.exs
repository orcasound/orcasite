import Config

# For development, we disable any cache and enable
# debugging and code reloading.
#
# The watchers configuration can be used to run external
# watchers to your application. For example, we use it
# with brunch.io to recompile .js and .css sources.
config :orcasite, OrcasiteWeb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  secret_key_base: "ZaTk5BBbg4BWCa+zQ0rjJxr9T5WqSEUt3oS0bd1Ct1SOFQg1HgBjPJaGffsNXZU3",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]}
  ]

# ## SSL Support
#
# In order to use HTTPS in development, a self-signed
# certificate can be generated by running the following
# command from your terminal:
#
#     openssl req -new -newkey rsa:4096 -days 365 -nodes -x509 -subj "/C=US/ST=Denial/L=Springfield/O=Dis/CN=www.example.com" -keyout priv/server.key -out priv/server.pem
#
# The `http:` config above can be replaced with:
#
#     https: [port: 4000, keyfile: "priv/server.key", certfile: "priv/server.pem"],
#
# If desired, both `http:` and `https:` keys can be
# configured to run both http and https servers on
# different ports.

# Watch static and templates for browser reloading.
config :orcasite, OrcasiteWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r{priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$},
      ~r{priv/gettext/.*(po)$},
      ~r{lib/orcasite_web/(controllers|live|components)/.*(ex|heex)$}
    ]
  ]

config :orcasite, dev_routes: true

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: {Orcasite.Logger, :format}

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Configure your database
config :orcasite, Orcasite.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: System.get_env("POSTGRES_USER") || "postgres",
  password: System.get_env("POSTGRES_PASSWORD") || "postgres",
  database: System.get_env("POSTGRES_DATABASE") || "orcasite_dev",
  hostname: System.get_env("POSTGRES_HOST") || "localhost",
  stacktrace: true,
  port: System.get_env("POSTGRES_PORT") || 5432,
  pool_size: 10,
  types: Orcasite.PostgresTypes


config :orcasite,
       :orcasite_s3_url,
       System.get_env("ORCASITE_S3_URL") || "https://s3-us-west-2.amazonaws.com/orcasite"

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# Disable swoosh api client as it is only required for production adapters.
config :swoosh, :api_client, false

config :orcasite, OrcasiteWeb.BasicAuth, username: "admin", password: "password"
