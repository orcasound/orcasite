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
      deps: deps(),
      preferred_cli_env: [
        "test.watch": :test
      ]
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
        :os_mon
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
      {:phoenix_html, "~> 4.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.20.0"},
      {:phoenix_live_dashboard, "~> 0.8.0"},
      {:esbuild, "~> 0.7", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.2.0", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.16"},
      {:finch, "~> 0.13"},
      {:floki, ">= 0.30.0", only: :test},
      {:gettext, "~> 0.20"},
      {:plug_cowboy, "~> 2.7"},
      {:plug, "~> 1.7"},
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      # Provides helper functions for easy batching of Ecto associations
      {:dataloader, "~> 2.0"},
      # Algorithm used by Comeonin to hash password
      {:bcrypt_elixir, "~> 3.0"},
      # JSON parser, works with Absinthe out of the box
      {:poison, "~> 5.0"},
      {:logfmt, "~> 3.0"},
      {:geo_postgis, "~> 3.0"},
      {:jason, "~> 1.2"},
      # Reverse proxy for proxying to nextjs app
      {:reverse_proxy_plug, "~> 3.0.1"},
      {:httpoison, "~> 2.2"},
      {:corsica, "~> 2.1"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:ash, "~> 2.21.2"},
      {:ash_admin, "~> 0.10.8"},
      {:ash_postgres, "~> 1.3"},
      {:heroicons, "~> 0.5"},
      {:oban, "~> 2.14"},
      {:gen_smtp, "~> 1.0"},
      {:ash_authentication, "~> 3.12.0"},
      {:ash_authentication_phoenix, "~> 1.9.3"},
      {:syn, "~> 3.3"},
      {:mjml, "~> 3.0.1"},
      {:zappa, github: "skanderm/zappa", branch: "master"},
      {:ash_uuid, "~> 0.4"},
      {:ash_graphql, "~> 0.27.0"},
      {:ash_json_api, "~> 0.34.0"},
      {:open_api_spex, "~> 3.16"},
      {:redoc_ui_plug, "~> 0.2.1"},
      {:phoenix_pubsub_redis, "~> 3.0.1"},
      {:nebulex, "~> 2.5"},
      {:nebulex_redis_adapter, "~> 2.3"},
      {:hammer_backend_redis, "~> 6.1"},
      {:hammer, "~> 6.0"},
      {:mix_test_watch, "~> 1.0", only: [:dev, :test], runtime: false},
      {:ex_unit_notifier, "~> 1.2", only: :test},
      {:remote_ip, "~> 1.1"},
      {:ex_aws, "~> 2.1"},
      {:ex_aws_s3, "~> 2.0"},
      {:hackney, "~> 1.9"},
      {:sweet_xml, "~> 0.6"},
      {:configparser_ex, "~> 4.0", only: :dev},
      {:broadway_sqs, "~> 0.7"},
      {:recon, "~> 2.5"},
      {:ecto_psql_extras, "~> 0.6"}
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
