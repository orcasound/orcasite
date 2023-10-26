defmodule Orcasite.Mixfile do
  use Mix.Project

  def project do
    [
      app: :orcasite,
      version: "0.0.1",
      elixir: "~> 1.12",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Orcasite.Application, []},
      extra_applications: [
        :logger,
        :runtime_tools,
        :inets,
        :phoenix_pubsub_redis,
        :wx,
        :observer
      ]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.7.2"},
      {:phoenix_pubsub, "~> 2.0"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.6"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 3.3"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.20.0"},
      {:phoenix_live_dashboard, "~> 0.8.0"},
      {:esbuild, "~> 0.7", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.2.0", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.3"},
      {:finch, "~> 0.13"},
      {:floki, ">= 0.30.0", only: :test},
      {:gettext, "~> 0.20"},
      {:plug_cowboy, "~> 2.5"},
      {:plug, "~> 1.7"},
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      # Provides helper functions for easy batching of Ecto associations
      {:dataloader, "~> 1.0.0"},
      # Algorithm used by Comeonin to hash password
      {:bcrypt_elixir, "~> 3.0"},
      # JSON parser, works with Absinthe out of the box
      {:poison, "~> 4.0"},
      {:logfmt, "~> 3.0"},
      {:geo_postgis, "~> 3.0"},
      {:jason, "~> 1.2"},
      # Reverse proxy for proxying to nextjs app
      {:reverse_proxy_plug, "~> 2.1"},
      {:httpoison, "~> 1.8"},
      {:corsica, "~> 1.0"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:ash, "~> 2.6", override: true},
      {:ash_admin, github: "ash-project/ash_admin", branch: "main"},
      {:ash_postgres, "~> 1.3"},
      {:heroicons, "~> 0.5"},
      {:oban, "~> 2.14"},
      {:gen_smtp, "~> 1.0"},
      {:ash_authentication, "~> 3.11.6"},
      {:ash_authentication_phoenix, "~> 1.8.3"},
      {:syn, "~> 3.3"},
      {:mjml, "~> 1.5.0"},
      {:zappa, github: "skanderm/zappa", branch: "master"},
      {:ash_uuid, "~> 0.4"},
      {:ash_graphql, github: "ash-project/ash_graphql", branch: "main"},
      {:ash_json_api, "~> 0.33.0"},
      {:open_api_spex, "~> 3.16"},
      {:redoc_ui_plug, "~> 0.2.1"},
      {:phoenix_pubsub_redis, "~> 3.0.1"},
      {:nebulex, "~> 2.5"},
      {:nebulex_redis_adapter, "~> 2.3"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "assets.setup", "assets.build", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate --quiet", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind default", "esbuild default"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end
