import Config

# config/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.

# ## Using releases
#
# If you use `mix release`, you need to explicitly enable the server
# by passing the PHX_SERVER=true when you start it:
#
#     PHX_SERVER=true bin/orcasite start
#
# Alternatively, you can use `mix phx.gen.release` to generate a `bin/server`
# script that automatically sets the env var above.

if System.get_env("PHX_SERVER") do
  config :orcasite, OrcasiteWeb.Endpoint, server: true
end

config :orcasite, :prod_host, System.get_env("HOST_URL", "live.orcasound.net")

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

  config :orcasite, Orcasite.Repo,
    # ssl: true,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    socket_options: maybe_ipv6

  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("HOST_URL") || "live.orcasound.net"
  port = String.to_integer(System.get_env("PORT") || "4000")

  if System.get_env("FEED_STREAM_QUEUE_URL", "") != "" do
    config :orcasite, :feed_stream_queue_url, System.get_env("FEED_STREAM_QUEUE_URL")
  end

  config :orcasite, OrcasiteWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      # Enable IPv6 and bind on all interfaces.
      # Set it to  {0, 0, 0, 0, 0, 0, 0, 1} for local network only access.
      # See the documentation on https://hexdocs.pm/plug_cowboy/Plug.Cowboy.html
      # for details about using IPv6 vs IPv4 and loopback vs public addresses.
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  if System.get_env("REDIS_URL", "") != "" do
    redis_ssl = String.starts_with?(System.get_env("REDIS_URL"), "rediss://")
    # If 'DYNO' doesn't exist, add:
    # Use https://devcenter.heroku.com/articles/dyno-metadata
    config :orcasite,
           :pub_sub_redis,
           [
             enabled: true,
             url: System.get_env("REDIS_URL"),
             node_name:
               (System.get_env("DYNO") || System.get_env("USER"))
               |> IO.inspect(label: "Node name"),
             ssl: String.starts_with?(System.get_env("REDIS_URL"), "rediss://"),
             redis_pool_size: String.to_integer(System.get_env("REDIS_PUBSUB_POOL_SIZE", "5"))
           ]
           |> Keyword.merge(if redis_ssl, do: [socket_opts: [verify: :verify_none]], else: [])
  end

  config :swoosh, :api_client, Swoosh.ApiClient.Finch

  config :orcasite,
    audio_image_bucket:
      System.get_env("ORCASITE_AUDIO_IMAGE_BUCKET", "audio-deriv-orcasound-net"),
    audio_image_bucket_region: System.get_env("ORCASITE_AUDIO_IMAGE_BUCKET_REGION", "us-west-2")

end
