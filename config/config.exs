# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

config :phoenix, :json_library, Jason

# General application configuration
config :orcasite,
  ecto_repos: [Orcasite.Repo]

# Configures the endpoint
config :orcasite, OrcasiteWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "ZaTk5BBbg4BWCa+zQ0rjJxr9T5WqSEUt3oS0bd1Ct1SOFQg1HgBjPJaGffsNXZU3",
  render_errors: [view: OrcasiteWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Orcasite.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :orcasite, env: Mix.env()

config :web_push_encryption, :vapid_details,
  subject: "mailto:administrator@example.com",
  public_key: "BItTHe7BXVxVxTWcwu-485CJ3ePaCQcoEbF_Qnlap5H9Do-J-hjnpD0VCcymjEt4JX5BkSHJrLfyGcwZ2XplZLs",
  private_key: "v4oOsWwufaMaFelexFj8NjCVAkzMsP2Wni-14wyB_YM"


# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
