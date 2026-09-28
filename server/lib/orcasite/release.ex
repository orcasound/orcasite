defmodule Orcasite.Release do
  @moduledoc """
  Tasks run from the release, where Mix is not available:

      bin/orcasite eval "Orcasite.Release.migrate()"
  """
  @app :orcasite

  def migrate do
    load_app()

    for repo <- repos() do
      {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true))
    end
  end

  def rollback(repo, version) do
    load_app()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  defp repos, do: Application.fetch_env!(@app, :ecto_repos)

  defp load_app do
    # Migrations open their own connections; Ecto needs SSL started for them.
    Application.ensure_all_started(:ssl)
    Application.load(@app)
  end
end
