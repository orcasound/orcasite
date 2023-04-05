defmodule Orcasite.Mixfile do
  use Mix.Project

  def project do
    [
      app: :orcasite,
      version: "0.0.1",
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
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
      extra_applications: [:logger, :runtime_tools, :scrivener_ecto, :inets]
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
      {:phoenix, "~> 1.5.1"},
      {:phoenix_pubsub, "~> 2.0"},
      {:phoenix_ecto, "~> 4.1"},
      {:ecto_sql, "~> 3.4"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 2.11"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:phoenix_live_view, "~> 0.12.0"},
      {:phoenix_live_dashboard, "~> 0.2.0"},
      {:floki, ">= 0.0.0", only: :test},
      {:gettext, "~> 0.11"},
      {:plug_cowboy, "~> 2.1"},
      {:plug, "~> 1.7"},
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      # Provides helper functions for easy batching of Ecto associations
      {:dataloader, "~> 1.0.0"},
      # Authenitication library for admin dashboard
      {:guardian, "~> 1.0"},
      # Algorithm used by Comeonin to hash password
      {:bcrypt_elixir, "~> 3.0"},
      # JSON parser, works with Absinthe out of the box
      {:poison, "~> 4.0"},
      {:logfmt, "~> 3.0"},
      {:geo_postgis, "~> 3.0"},
      {:jason, "~> 1.1"},
      {:scrivener_ecto, "~> 2.0"},
      # Reverse proxy for proxying to nextjs app
      {:reverse_proxy_plug, "~> 2.1"},
      {:httpoison, "~> 1.8"},
      {:corsica, "~> 1.0"},
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 0.4"},
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
      setup: ["deps.get", "ecto.setup", "cmd npm install --prefix assets"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
